#![allow(unused_variables, private_no_mangle_fns)]

use std::mem;

use super::{ REG_PC, REG_LR, REG_SP, Arm7TDMI, ConditionCode, StepEvent };
use interconnect::Interconnect;
use super::core_common::*;

pub fn step_thumb(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u16) -> StepEvent {
    let discr = (op >> 6) as usize;
    THUMB_LUT[discr](arm, interconnect, op)
}

type ThumbOp = fn(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u16) -> StepEvent;

include!(concat!(env!("OUT_DIR"), "/thumb_core_generated.rs"));
