#![allow(unused_variables, private_no_mangle_fns, unused_assignments)]

use std::cmp;

use num::{ FromPrimitive, NumCast };

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
//    ARM_LUT[discr as usize](arm, interconnect, op)
    StepEvent::None
}

struct ArmOp(u32);

impl ArmOp {
    fn field(&self, offset: u32, width: u32) -> u32 {
        let mask = (1 << width) - 1;
        (self.0 >> offset) & mask
    }

    fn reg(&self, offset: u32) -> usize { self.field(offset, 4) as usize }
    fn flag(&self, offset: u32) -> bool { self.field(offset, 1) != 0 }
}

fn unhandled(_: &mut Arm7TDMI, _: &mut Interconnect, op: u32) -> StepEvent {
    error!(CPU, "Arm instruction {:08X} wasn't handled by the decoder", op);
}

fn op_coprocessor(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, _: ArmOp) -> StepEvent {
    arm.signal_undef(interconnect);
    StepEvent::None
}

fn op_und(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, _: ArmOp) -> StepEvent {
    arm.signal_undef(interconnect);
    StepEvent::None
}

fn op_swp(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) -> StepEvent {
    let rm_index = op.reg(0);
    let rd_index = op.reg(12);
    let rn_index = op.reg(16);

    assert!(op.reg(8) == 0);
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

fn op_swpb(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) -> StepEvent {
    let rm_index = op.reg(0);
    let rd_index = op.reg(12);
    let rn_index = op.reg(16);

    assert!(op.reg(8) == 0);
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

fn op_mrs_reg(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) -> StepEvent {
    if op.field(0, 12) != 0 || op.field(16, 4) != 0xF {
        return op_und(arm, interconnect, op);
    }

    let rd_index = op.reg(12);
    let ps = op.flag(22);

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

fn op_msr_reg(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) -> StepEvent {
    if op.field(8, 12) & !0x100 != 0b1000_1111_0000 {
        return op_und(arm, interconnect, op);
    }

    let flags = op.flag(16);

    let rm_index = op.reg(0);
    let pd = op.flag(22);

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

fn op_msr_flag_imm(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) -> StepEvent {
    if op.field(12, 8) != 0b1000_1111 {
        return op_und(arm, interconnect, op);
    }

    let imm = op.field(0, 8);
    let rotate = op.field(8, 4);
    let pd = op.flag(22);

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

fn op_bx(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) -> StepEvent {
    if op.field(8, 12) != 0xFFF {
        return op_und(arm, interconnect, op);
    }

    let rn = arm.regs[op.reg(0)];
    arm.branch_exchange(interconnect, rn);

    StepEvent::None
}

fn op_swi(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, _: ArmOp) -> StepEvent {
    arm.signal_swi(interconnect);
    StepEvent::None
}

//include!(concat!(env!("OUT_DIR"), "/arm_core_generated.rs"));
