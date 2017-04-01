#![allow(unused_mut, unused_variables)]

use std::mem;

use super::{ REG_PC, REG_LR, REG_SP, Arm7TDMI, ConditionCode, StepEvent };
use interconnect::Interconnect;
use utils::Cycle;
use super::core_common::*;

pub fn step_thumb(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u16) -> StepEvent {
    debug_assert!(arm.regs[REG_PC] & 1 == 0);

    let discr = (op >> 6) as usize;
    THUMB_LUT[discr](arm, interconnect, op)
}

/*
#[inline(always)]
fn dispatch_thumb_fast(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, mut cycles_remaining: i32) -> i32 {
    if cycles_remaining > 0 {
        let pc = arm.regs[REG_PC] + 2;
        if let Some(preload) = interconnect.exec16_fast(pc, Some(&mut cycles_remaining)) {
            arm.regs[REG_PC] = pc;

            let next_op = interconnect.prefetch[0] as u16;
            interconnect.prefetch[0] = interconnect.prefetch[1];
            interconnect.prefetch[1] = preload as u32;

            THUMB_LUT[(next_op >> 6) as usize](arm, interconnect, next_op, cycles_remaining)
        } else {
            #[cold]dispatch_thumb_slow(arm, interconnect, cycles_remaining)
        }
    } else {
        cycles_remaining
    }
}

#[cold]
#[inline(never)]
fn dispatch_thumb_slow(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, mut cycles_remaining: i32) -> i32 {
    if cycles_remaining > 0 {
        let pc = arm.regs[REG_PC] + 2;
        let next_op = interconnect.prefetch[0] as u16;
        interconnect.prefetch[0] = interconnect.prefetch[1];
        interconnect.prefetch[1] = interconnect.exec16_slow(pc, Some(&mut cycles_remaining)) as u32;
        arm.regs[REG_PC] = pc;
        THUMB_LUT[(next_op >> 6) as usize](arm, interconnect, next_op, cycles_remaining)
    } else {
        cycles_remaining
    }
}*/

type ThumbOp = fn(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u16) -> StepEvent;

include!(concat!(env!("OUT_DIR"), "/thumb_core_generated.rs"));
