use arm7tdmi::Arm7TDMI;

macro_rules! check_watchpoint {
    ($arm:expr, $addr:expr) => {
        if $arm.watchpoints.contains($addr) {
            return StepEvent::TriggerWatchpoint($addr);
        }
    }
}

#[inline(always)]
pub fn set_zn(arm: &mut Arm7TDMI, value: u32) {
    arm.cpsr.z = value == 0;
    arm.cpsr.n = (value as i32) < 0;
}

#[inline(always)]
pub fn add_set_vc(arm: &mut Arm7TDMI, a: u32, b: u32) {
    arm.cpsr.v = (a as i32).overflowing_add(b as i32).1;
    arm.cpsr.c = a.overflowing_add(b).1;
}

#[inline(always)]
pub fn sub_set_vc(arm: &mut Arm7TDMI, a: u32, b: u32) {
    arm.cpsr.v = (a as i32).overflowing_sub(b as i32).1;
    arm.cpsr.c = a.overflowing_sub(b).1;
}

#[inline(always)]
pub fn sign_extend(n: u32, n_bits: usize) -> u32 {
    let shift = 32 - n_bits;
    (((n << shift) as i32) >> shift) as u32
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
