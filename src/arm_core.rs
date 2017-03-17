#![allow(unused_mut, unused_variables, private_no_mangle_fns)]

use std::cmp;

use arm7tdmi::{ Arm7TDMI, REG_PC, REG_LR, ConditionCode, StepEvent };
use interconnect::Interconnect;
use utils::{ Cycle, set_zn, add_set_vc, sub_set_vc };
use num::FromPrimitive;

pub fn step_arm(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    debug_assert!(arm.regs[REG_PC] & 3 == 0);

    let cond = ConditionCode::from_u32(op >> 28).unwrap();
    if !arm.eval_condition_code(cond) {
        return StepEvent::None;
    }

    let discr = (op >> 4 & 0xF) | (op >> 16 & 0xFF0);
    ARM_LUT[discr as usize](arm, interconnect, op)
}

pub fn barrel_shift_lsl(_arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount >= 32 {
        0
    } else {
        rm << shift_amount
    }
}
pub fn barrel_shift_lsl_set_flags(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount == 32 {
        arm.cpsr.c = (rm & 1) != 0;
        0
    } else if shift_amount > 32 {
        arm.cpsr.c = false;
        0
    } else {
        if shift_amount != 0 {
            arm.cpsr.c = ((rm >> (32 - shift_amount)) & 1) != 0;
        }
        rm << shift_amount
    }
}

pub fn barrel_shift_lsr(_arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount >= 32 || shift_amount == 0 {
        0
    } else {
        rm >> shift_amount
    }
}
pub fn barrel_shift_lsr_set_flags(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount == 32 || shift_amount == 0 {
        arm.cpsr.c = (rm >> 31) != 0;
        0
    } else if shift_amount > 32 {
        arm.cpsr.c = false;
        0
    } else {
        arm.cpsr.c = ((rm >> (shift_amount - 1)) & 1) != 0;
        rm >> shift_amount
    }
}

pub fn barrel_shift_asr(_arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount >= 32 || shift_amount == 0 {
        // Return sign extension of rm
        if (rm >> 31) != 0 {
            0xFFFF_FFFF
        } else {
            0
        }
    } else {
        ((rm as i32) >> shift_amount) as u32
    }
}
pub fn barrel_shift_asr_set_flags(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount >= 32 || shift_amount == 0 { // Return sign extension of rm
        if (rm >> 31) != 0 {
            arm.cpsr.c = true;
            0xFFFF_FFFF
        } else {
            arm.cpsr.c = false;
            0
        }
    } else {
        arm.cpsr.c = (((rm as i32) >> (shift_amount - 1)) & 1) != 0;
        ((rm as i32) >> shift_amount) as u32
    }
}

pub fn barrel_shift_ror(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount == 0 { // RRX
        let carry_in = arm.cpsr.c as u32;
        (rm >> 1) | (carry_in << 31)
    } else {
        rm.rotate_right(shift_amount)
    }
}
pub fn barrel_shift_ror_set_flags(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount == 0 { // RRX
        let carry_in = arm.cpsr.c as u32;
        arm.cpsr.c = (rm & 1) != 0;
        (rm >> 1) | (carry_in << 31)
    } else {
        arm.cpsr.c = ((rm >> ((shift_amount - 1) & 0x1F)) & 1) != 0;
        rm.rotate_right(shift_amount)
    }
}

type ArmOp = fn(&mut Arm7TDMI, &mut Interconnect, u32) -> StepEvent;

fn unhandled(_: &mut Arm7TDMI, _: &mut Interconnect, op: u32) -> StepEvent {
    println!("WARNING: Unhandled instruction {:08X}", op);
    StepEvent::None
}

fn op_coprocessor(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, _: u32) -> StepEvent {
    arm.signal_undef(interconnect);
    StepEvent::None
}

fn op_und(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, _: u32) -> StepEvent {
    arm.signal_undef(interconnect);
    StepEvent::None
}

fn op_ldm(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    let mut cycles = Cycle(1); // 1 internal cycle

    let rn_index = (op >> 16 & 0xF) as usize;
    let writeback = op >> 21 & 1 != 0;
    let up = op >> 23 & 1 != 0;
    let preindex = op >> 24 & 1 != 0;
    let reglist = op & 0xFFFF;

    let mut addr = arm.regs[rn_index];

    match (up, preindex) {
        (true, false) => {
            for i in 0..15 { // Post-increment
                if reglist & (1 << i) != 0 {
                    check_watchpoint!(arm, addr);
                    arm.regs[i] = add_cycles!(cycles, interconnect.read32(addr));
                    addr += 4;
                }
            }
            if reglist & (1 << REG_PC) != 0 {
                check_watchpoint!(arm, addr);
                let target = add_cycles!(cycles, interconnect.read32(addr));
                arm.branch_to(interconnect, target);
                addr += 4;
            }
        }

        (true, true) => { // Pre-increment
            for i in 0..15 {
                if reglist & (1 << i) != 0 {
                    addr += 4;
                    check_watchpoint!(arm, addr);
                    arm.regs[i] = add_cycles!(cycles, interconnect.read32(addr));
                }
            }
            if reglist & (1 << REG_PC) != 0 {
                addr += 4;
                check_watchpoint!(arm, addr);
                let target = add_cycles!(cycles, interconnect.read32(addr));
                arm.branch_to(interconnect, target);
            }
        }

        (false, false) => { // Post-decrement
            let new_base = addr - reglist.count_ones() * 4;
            addr = addr - reglist.count_ones() * 4 + 4;
            for i in 0..15 {
                if reglist & (1 << i) != 0 {
                    check_watchpoint!(arm, addr);
                    arm.regs[i] = add_cycles!(cycles, interconnect.read32(addr));
                    addr += 4;
                }
            }
            if reglist & (1 << REG_PC) != 0 {
                check_watchpoint!(arm, addr);
                let target = add_cycles!(cycles, interconnect.read32(addr));
                arm.branch_to(interconnect, target);
            }
            addr = new_base;
        }

        (false, true) => { // Pre-decrement
            let new_base = addr - reglist.count_ones() * 4;
            addr = addr - reglist.count_ones() * 4;
            for i in 0..15 {
                if reglist & (1 << i) != 0 {
                    addr += 4;
                    check_watchpoint!(arm, addr);
                    arm.regs[i] = add_cycles!(cycles, interconnect.read32(addr));
                }
            }
            if reglist & (1 << REG_PC) != 0 {
                addr += 4;
                check_watchpoint!(arm, addr);
                let target = add_cycles!(cycles, interconnect.read32(addr));
                arm.branch_to(interconnect, target);
            }
            addr = new_base;
        }
    }

    if writeback {
        arm.regs[rn_index] = addr;
    }

    arm.cycles += cycles;

    StepEvent::None
}

fn op_stm(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    let mut cycles = Cycle(0);

    let rn_index = (op >> 16 & 0xF) as usize;
    let writeback = op >> 21 & 1 != 0;
    let up = op >> 23 & 1 != 0;
    let preindex = op >> 24 & 1 != 0;
    let reglist = op & 0xFFFF;

    let mut addr = arm.regs[rn_index];

    match (up, preindex) {
        (true, false) => {
            for i in 0..15 { // Post-increment
                if reglist & (1 << i) != 0 {
                    check_watchpoint!(arm, addr);
                    cycles += interconnect.write32(addr, arm.regs[i]);
                    addr += 4;
                }
            }
            if reglist & (1 << REG_PC) != 0 {
                check_watchpoint!(arm, addr);
                // address of current instruction + 12 is stored
                cycles += interconnect.write32(addr, arm.regs[REG_PC] + 4);
                addr += 4;
            }
        }

        (true, true) => { // Pre-increment
            for i in 0..15 {
                if reglist & (1 << i) != 0 {
                    addr += 4;
                    check_watchpoint!(arm, addr);
                    cycles += interconnect.write32(addr, arm.regs[i]);
                }
            }
            if reglist & (1 << REG_PC) != 0 {
                addr += 4;
                check_watchpoint!(arm, addr);
                // address of current instruction + 12 is stored
                cycles += interconnect.write32(addr, arm.regs[REG_PC] + 4);
            }
        }

        (false, false) => { // Post-decrement
            let new_base = addr - reglist.count_ones() * 4;
            addr = addr - reglist.count_ones() * 4 + 4;
            for i in 0..15 {
                if reglist & (1 << i) != 0 {
                    check_watchpoint!(arm, addr);
                    cycles += interconnect.write32(addr, arm.regs[i]);
                    addr += 4;
                }
            }
            if reglist & (1 << REG_PC) != 0 {
                check_watchpoint!(arm, addr);
                // address of current instruction + 12 is stored
                cycles += interconnect.write32(addr, arm.regs[REG_PC] + 4);
            }
            addr = new_base;
        }

        (false, true) => { // Pre-decrement
            let new_base = addr - reglist.count_ones() * 4;
            addr = addr - reglist.count_ones() * 4;
            for i in 0..15 {
                if reglist & (1 << i) != 0 {
                    addr += 4;
                    check_watchpoint!(arm, addr);
                    cycles += interconnect.write32(addr, arm.regs[i]);
                }
            }
            if reglist & (1 << REG_PC) != 0 {
                addr += 4;
                check_watchpoint!(arm, addr);
                // address of current instruction + 12 is stored
                cycles += interconnect.write32(addr, arm.regs[REG_PC] + 4);
            }
            addr = new_base;
        }
    }

    if writeback {
        arm.regs[rn_index] = addr;
    }

    arm.cycles += cycles;

    StepEvent::None
}

fn op_swp(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    let mut cycles = Cycle(1); // 1 internal cycle

    let rm_index = (op & 0xF) as usize;
    let rd_index = (op >> 12 & 0xF) as usize;
    let rn_index = (op >> 16 & 0xF) as usize;

    assert!(op >> 8 & 0xF == 0);
    assert!(rm_index != REG_PC);
    assert!(rd_index != REG_PC);
    assert!(rn_index != REG_PC);

    let addr = arm.regs[rn_index];
    let rm = arm.regs[rm_index];

    check_watchpoint!(arm, addr);
    let old = add_cycles!(cycles, interconnect.read32(addr));
    arm.regs[rd_index] = old;
    cycles += interconnect.write32(addr, rm);

    arm.cycles += cycles;
    StepEvent::None
}

fn op_swpb(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    let mut cycles = Cycle(1); // 1 internal cycle

    let rm_index = (op & 0xF) as usize;
    let rd_index = (op >> 12 & 0xF) as usize;
    let rn_index = (op >> 16 & 0xF) as usize;

    assert!(op >> 8 & 0xF == 0);
    assert!(rm_index != REG_PC);
    assert!(rd_index != REG_PC);
    assert!(rn_index != REG_PC);

    let addr = arm.regs[rn_index];
    let rm = arm.regs[rm_index];

    check_watchpoint!(arm, addr);
    let old = add_cycles!(cycles, interconnect.read8(addr)) as u32;
    arm.regs[rd_index] = old;
    cycles += interconnect.write8(addr, rm as u8);

    arm.cycles += cycles;
    StepEvent::None
}

fn op_mrs_reg(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    if op & 0xF0F0F != 0xF0000 {
        return op_und(arm, interconnect, op);
    }

    let rd_index = (op >> 12 & 0xF) as usize;
    let ps = op & 0x0040_0000 != 0;

    debug_assert!(rd_index != REG_PC);

    let value = if ps {
        arm.get_spsr().into()
    } else {
        arm.cpsr.into()
    };

    arm.regs[rd_index] = value;

    StepEvent::None
}

fn op_msr_reg(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    let flags = match op >> 8 & 0xFFF {
        0b1001_1111_0000 => false,
        0b1000_1111_0000 => true,
        _ => {
            return op_und(arm, interconnect, op);
        }
    };

    let rm_index = (op & 0xF) as usize;
    let pd = op & 0x0040_0000 != 0;

    debug_assert!(rm_index != REG_PC);

    let rm = arm.regs[rm_index];

    match (pd, flags) {
        (true, true) => arm.get_spsr_mut().set_flags(rm),
        (false, true) => arm.cpsr.set_flags(rm),
        (true, false) => *arm.get_spsr_mut() = rm.into(),
        (false, false) => arm.set_cpsr(rm.into()),
    }

    StepEvent::None
}

fn op_msr_flag_imm(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    if op >> 12 & 0xFF != 0b1000_1111 {
        return op_und(arm, interconnect, op);
    }

    let imm = op & 0xFF;
    let rotate = op >> 8 & 0xF;
    let pd = op & 0x0040_0000 != 0;

    let value = imm.rotate_right(rotate);

    if pd {
        arm.get_spsr_mut().set_flags(value);
    } else {
        arm.cpsr.set_flags(value);
    }

    StepEvent::None
}

fn op_bx(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    if op & 0xFFF00 != 0xFFF00 {
        return op_und(arm, interconnect, op);
    }

    debug_assert!(op >> 8 & 0xFFF == 0xFFF);
    let rn = arm.regs[(op & 0xF) as usize];
    arm.branch_exchange(interconnect, rn);

    StepEvent::None
}

fn op_swi(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    arm.signal_swi(interconnect);
    StepEvent::None
}

include!(concat!(env!("OUT_DIR"), "/arm_core_generated.rs"));
