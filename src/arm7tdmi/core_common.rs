use super::Arm7TDMI;
pub use utils::sign_extend;
use arm7tdmi::disassemble::DisResult;
use arm7tdmi::disassemble::err;

macro_rules! check_watchpoint {
    ($arm:expr, $addr:expr) => {
        if $arm.watchpoints.contains($addr) {
            println!("Triggered watchpoint at {:08X}!", $addr)
//            return StepEvent::TriggerWatchpoint($addr);
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

pub mod cond {
    use arm7tdmi::ConditionCode;

    pub trait Cond { const CC: ConditionCode; }
    pub struct Eq; impl Cond for Eq { const CC: ConditionCode = ConditionCode::Eq; }
    pub struct Ne; impl Cond for Ne { const CC: ConditionCode = ConditionCode::Ne; }
    pub struct Cs; impl Cond for Cs { const CC: ConditionCode = ConditionCode::Cs; }
    pub struct Cc; impl Cond for Cc { const CC: ConditionCode = ConditionCode::Cc; }
    pub struct Mi; impl Cond for Mi { const CC: ConditionCode = ConditionCode::Mi; }
    pub struct Pl; impl Cond for Pl { const CC: ConditionCode = ConditionCode::Pl; }
    pub struct Vs; impl Cond for Vs { const CC: ConditionCode = ConditionCode::Vs; }
    pub struct Vc; impl Cond for Vc { const CC: ConditionCode = ConditionCode::Vc; }
    pub struct Hi; impl Cond for Hi { const CC: ConditionCode = ConditionCode::Hi; }
    pub struct Ls; impl Cond for Ls { const CC: ConditionCode = ConditionCode::Ls; }
    pub struct Ge; impl Cond for Ge { const CC: ConditionCode = ConditionCode::Ge; }
    pub struct Lt; impl Cond for Lt { const CC: ConditionCode = ConditionCode::Lt; }
    pub struct Gt; impl Cond for Gt { const CC: ConditionCode = ConditionCode::Gt; }
    pub struct Le; impl Cond for Le { const CC: ConditionCode = ConditionCode::Le; }
}

pub mod load {
    use interconnect::Interconnect;
    use num::NumCast;

    type LoadFn = fn(&mut Interconnect, u32) -> u32;

    pub trait Load: NumCast {
        const LOAD: LoadFn;
    }

    impl Load for u8  { const LOAD: LoadFn = Interconnect::read_ext_u8; }
    impl Load for i8  { const LOAD: LoadFn = Interconnect::read_ext_i8; }
    impl Load for u16 { const LOAD: LoadFn = Interconnect::read_ext_u16; }
    impl Load for i16 { const LOAD: LoadFn = Interconnect::read_ext_i16; }
    impl Load for u32 { const LOAD: LoadFn = Interconnect::read32; }
}

pub mod store {
    use interconnect::Interconnect;
    use num::NumCast;

    type StoreFn<T> = fn(&mut Interconnect, u32, T);

    pub trait Store: NumCast {
        const STORE: StoreFn<Self>;
    }

    impl Store for u8  { const STORE: StoreFn<Self> = Interconnect::write8; }
    impl Store for u16 { const STORE: StoreFn<Self> = Interconnect::write16; }
    impl Store for u32 { const STORE: StoreFn<Self> = Interconnect::write32; }
}

pub enum Bit {
    Any,
    Zero, // 0
    One, // 1
    AtLeastASingleOne, // ^
    AtLeastASingleZero, // v
}

pub(super) fn process_bit_format(fmt: &str, accept_index: fn(usize) -> bool) -> DisResult<Vec<Bit>> {
    fmt.chars()
        .filter(|&c| !c.is_whitespace())
        .rev()
        .enumerate()
        .filter(|&(i, _)| accept_index(i))
        .map(|(_, c)| parse_spec_char(c))
        .collect::<DisResult<Vec<Bit>>>()
}

pub(super) fn parse_spec_char(c: char) -> DisResult<Bit> {
    Ok(match c {
        '0' => Bit::Zero,
        '1' => Bit::One,
        '^' => Bit::AtLeastASingleOne,
        'v' => Bit::AtLeastASingleZero,
        'i' | 's' | 'n' | 'd' | 'o' | 'b' | 'l' | 'j' | 'h' | 'r' | 'S' | 'W' | 'U' | 'N' | 'L' | 'P' | 'c' | 'p' | 'm' | '_' | 'I' => Bit::Any,
        _ => return Err(err(format!("Unrecognised format spec character '{}'", c)))
    })
}

pub(super) fn encoding_matches(bits: &[Bit], discriminant: u32) -> bool {
    let mut saw_a_one: Option<bool> = None;
    let mut saw_a_zero: Option<bool> = None;

    for (i, expect) in bits.iter().enumerate() {
        let bit = (discriminant >> i) & 1;
        match expect {
            Bit::Zero => if bit != 0 { return false; },
            Bit::One => if bit != 1 { return false },
            Bit::AtLeastASingleZero => {
                if saw_a_zero == None { saw_a_zero = Some(false); }
                if bit == 0 {
                    saw_a_zero = Some(true);
                }
            }
            Bit::AtLeastASingleOne => {
                if saw_a_one == None { saw_a_one = Some(false); }
                if bit == 1 {
                    saw_a_one = Some(true);
                }
            }
            Bit::Any => {}
        }
    }

    if saw_a_one == Some(false) || saw_a_zero == Some(false) {
        false
    } else {
        true
    }
}