#![allow(unused_mut, unused_variables, private_no_mangle_fns, unused_assignments)]

use std::cmp;

use num::FromPrimitive;

use super::{ Arm7TDMI, REG_PC, REG_LR, ConditionCode, StepEvent };
use interconnect::Interconnect;
use super::core_common::*;
use log::*;

pub fn step_arm(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    let cond = ConditionCode::from_u32(op >> 28).unwrap();
    if !arm.eval_condition_code(cond) {
        return StepEvent::None;
    }

    let discr = (op >> 4 & 0xF) | (op >> 16 & 0xFF0);
    ARM_LUT[discr as usize](arm, interconnect, op)
}

type ArmOp = fn(&mut Arm7TDMI, &mut Interconnect, u32) -> StepEvent;

fn unhandled(_: &mut Arm7TDMI, _: &mut Interconnect, op: u32) -> StepEvent {
    error!(CPU, "Arm instruction {:08X} wasn't handled by the decoder", op);
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
    let old = interconnect.read32(addr);
    arm.regs[rd_index] = old;
    interconnect.write32(addr, rm);

    interconnect.add_internal_cycles(1);
    StepEvent::None
}

fn op_swpb(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
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
    let old = interconnect.read8(addr) as u32;
    arm.regs[rd_index] = old;
    interconnect.write8(addr, rm as u8);

    interconnect.add_internal_cycles(1);
    StepEvent::None
}

fn op_mrs_reg(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    if op & 0xF0F0F != 0xF0000 {
        return op_und(arm, interconnect, op);
    }

    let rd_index = (op >> 12 & 0xF) as usize;
    let ps = op & 0x0040_0000 != 0;

    debug_assert!(rd_index != REG_PC);

    let value: u32 = if ps {
        if let Some(spsr) = arm.get_spsr() {
            spsr.into()
        } else {
            warn!(CPU, "Tried to get SPSR in mode {:?} which has no SPSR", arm.cpsr.get_mode());
            0
        }
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
        (false, true) => arm.cpsr.set_flags_from_bits(rm),
        (false, false) => arm.set_cpsr_from_bits(rm),
        (true, true) => {
            let mode = arm.cpsr.get_mode();
            if let Some(spsr) = arm.get_spsr_mut() {
                spsr.set_flags_from_bits(rm);
            } else {
                warn!(CPU, "Tried to set flags of SPSR in mode {:?} which has no SPSR", mode);
            }
        }
        (true, false) => {
            let mode = arm.cpsr.get_mode();
            if let Some(spsr) = arm.get_spsr_mut() {
                *spsr = rm.into();
            } else {
                warn!(CPU, "Tried to set SPSR in mode {:?} which has no SPSR", mode);
            }
        }
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
        let mode = arm.cpsr.get_mode();
        if let Some(spsr) = arm.get_spsr_mut() {
            spsr.set_flags_from_bits(value)
        } else {
            warn!(CPU, "Tried to set flags of SPSR in mode {:?} which has no SPSR", mode);
        }
    } else {
        arm.cpsr.set_flags_from_bits(value);
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
