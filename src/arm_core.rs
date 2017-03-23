#![allow(unused_mut, unused_variables, private_no_mangle_fns, unused_assignments)]

use std::cmp;

use arm7tdmi::{ Arm7TDMI, REG_PC, REG_LR, ConditionCode, StepEvent };
use interconnect::Interconnect;
use utils::Cycle;
use core_common::*;
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
