#![allow(unused_variables, private_no_mangle_fns)]

use std::mem::size_of;

use num::NumCast;

use super::{ REG_PC, REG_LR, REG_SP, Arm7TDMI, ConditionCode, StepEvent };
use interconnect::Interconnect;
use super::core_common::*;

pub fn step_thumb(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u16) -> StepEvent {
    let discr = (op >> 6) as usize;
//    THUMB_LUT[discr](arm, interconnect, op)
}

static REG_NAMES: [&str; 16] = [
    "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
    "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
];

struct ThumbOp(u16);

impl ThumbOp {
    fn field<T: NumCast>(&self, offset: u16, width: u16) -> T {
        let mask = (1 << width) - 1;
        T::from((self.0 >> offset) & mask).unwrap()
    }

    fn reg3(&self, offset: u16) -> usize { self.field(offset, 3) }
    fn imm3(&self, offset: u16) -> u32 { self.field(offset, 3) }
    fn imm5(&self, offset: u16) -> u32 { self.field(offset, 5) }
    fn imm8(&self) -> u32 { self.field(0, 8) }
    fn hrs(&self) -> usize { self.field(3, 4) }
    fn hrd(&self) -> usize { self.field::<usize>(0, 3) | (self.field::<usize>(8, 1) << 3) }

    fn get_rd(&self) -> &'static str {
        REG_NAMES[self.field(0, 3)]
    }

//    fn get_
}

type ThumbEmuFn = fn(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ThumbOp);

fn shift_imm<F>(arm: &mut Arm7TDMI, op: ThumbOp, f: F)
    where F: Fn(&mut Arm7TDMI, u32, u32) -> u32
{
    let rs = arm.regs[op.reg3(3)];
    let imm = op.imm5(6);
    let result = f(arm, rs, imm);
    set_zn(arm, result);
    arm.regs[op.reg3(0)] = result;
}

fn alu3_reg<F>(arm: &mut Arm7TDMI, op: ThumbOp, f: F)
    where F: Fn(u32, u32) -> u32
{
    let rs = arm.regs[op.reg3(3)];
    let rn = arm.regs[op.reg3(6)];
    let result = f(rs, rn);
    set_zn(arm, result);
    add_set_vc(arm, rs, rn);
    arm.regs[op.reg3(0)] = result;
}

fn alu3_imm<F>(arm: &mut Arm7TDMI, op: ThumbOp, f: F)
    where F: Fn(u32, u32) -> u32
{
    let rs = arm.regs[op.reg3(3)];
    let rn = op.imm3(6);
    let result = f(rs, rn);
    set_zn(arm, result);
    add_set_vc(arm, rs, rn);
    arm.regs[op.reg3(0)] = result;
}

fn alu2_imm<F>(arm: &mut Arm7TDMI, op: ThumbOp, f: F)
    where F: Fn(u32, u32) -> u32
{
    let imm = op.imm8();
    let rd_index = op.reg3(8);
    let rd = arm.regs[rd_index];
    let result = f(rd, imm);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
}

fn alu2_reg<F>(arm: &mut Arm7TDMI, op: ThumbOp, f: F)
    where F: Fn(&mut Arm7TDMI, u32, u32, usize)
{
    let rd_index = op.reg3(0);
    let rd = arm.regs[rd_index];
    let rs = arm.regs[op.reg3(3)];
    f(arm, rs, rd, rd_index)
}

fn alu2_hreg<F>(arm: &mut Arm7TDMI, op: ThumbOp, f: F)
    where F: Fn(&mut Arm7TDMI, u32, u32, usize)
{
    let rs = arm.regs[op.hrs()];
    let rd_index = op.hrd();
    let rd = arm.regs[rd_index];
    f(arm, rs, rd, rd_index);
}

fn str_reg_offset<T: NumCast, F>(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ThumbOp, f: F)
    where F: Fn(&mut Interconnect, u32, T)
{
    let rd = arm.regs[op.reg3(0)];
    let rb = arm.regs[op.reg3(3)];
    let ro = arm.regs[op.reg3(6)];
    let addr = rb.wrapping_add(ro);
    check_watchpoint!(arm, addr);
    f(ic, addr, T::from(rd).unwrap());
}

fn ldr_reg_offset<F>(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ThumbOp, f: F)
    where F: Fn(&mut Interconnect, u32) -> u32
{
    let rb = arm.regs[op.reg3(3)];
    let ro = arm.regs[op.reg3(6)];
    let addr = rb.wrapping_add(ro);
    check_watchpoint!(arm, addr);
    arm.regs[op.reg3(0)] = f(ic, addr);
}

fn str_imm_offset<T: NumCast, F>(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ThumbOp, f: F)
    where F: Fn(&mut Interconnect, u32, T)
{
    let rd = arm.regs[op.reg3(0)];
    let rb = arm.regs[op.reg3(3)];
    let offset = op.imm5(6) * size_of::<T>() as u32;
    let addr = rb.wrapping_add(offset);
    check_watchpoint!(arm, addr);
    f(ic, addr, T::from(rd).unwrap());
}

fn ldr_imm_offset<F>(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ThumbOp, size_of_t: usize, f: F)
    where F: Fn(&mut Interconnect, u32) -> u32
{
    let rb = arm.regs[op.reg3(3)];
    let offset = op.imm5(6) * size_of_t as u32;
    let addr = rb.wrapping_add(offset);
    check_watchpoint!(arm, addr);
    arm.regs[op.reg3(0)] = f(ic, addr);
    // TODO: The instruction cycle times for the THUMB instruction are identical to that of the equivalent ARM instruction. For more information on instruction cycle times, please refer to Chapter 10, Instruction Cycle Operations.
    ic.add_internal_cycles(1); // internal cycle for address calculation
}

//bitflags!()

/// Legend:
/// [isndob] can be anything
/// At least one of the bits marked as ^ must be 1
static THUMB_DISPATCH_TABLE: &[(&str, &str, ThumbEmuFn)] = &[
    ("000 00 iiiii sss ddd", "LSL rd, rs, #imm5",
        |arm, _, op| {
            shift_imm(arm, op, barrel_shift_lsl_set_flags);
        }
    ),
    ("000 01 iiiii sss ddd", "LSR rd, rs, #imm5",
        |arm, _, op| {
            shift_imm(arm, op, barrel_shift_lsr_set_flags);
        }
    ),
    ("000 10 iiiii sss ddd", "ASR rd, rs, #imm5",
        |arm, _, op| {
            shift_imm(arm, op, barrel_shift_asr_set_flags);
        }
    ),

    ("00011 00 nnn sss ddd", "ADD rd, rs, rn",
        |arm, _, op| {
            alu3_reg(arm, op, u32::wrapping_add)
        }
    ),
    ("00011 01 nnn sss ddd", "SUB rd, rs, rn",
        |arm, _, op| {
            alu3_reg(arm, op, u32::wrapping_sub)
        }
    ),
    ("00011 10 iii sss ddd", "ADD rd, rs, #imm3",
        |arm, _, op| {
            alu3_imm(arm, op, u32::wrapping_add)
        }
    ),
    ("00011 11 iii sss ddd", "SUB rd, rs, #imm3",
        |arm, _, op| {
            alu3_imm(arm, op, u32::wrapping_sub)
        }
    ),

    ("001 00 ddd iiiiiiii", "MOV rd, #imm8",
        |arm, _, op| {
            let imm = op.imm8();
            set_zn(arm, imm);
            arm.regs[op.reg3(8)] = imm;
        }
    ),
    ("001 01 ddd iiiiiiii", "CMP rd, #imm8",
        |arm, _, op| {
            let imm = op.imm8();
            let rd: u32 = arm.regs[op.reg3(8)];
            let result = rd.wrapping_sub(imm);
            set_zn(arm, result);
            sub_set_vc(arm, rd, imm);
        }
    ),
    ("001 10 ddd iiiiiiii", "ADD rd, #imm8",
        |arm, _, op| {
            let imm = op.imm8();
            let rd_index = op.reg3(8);
            let rd: u32 = arm.regs[rd_index];
            let result = rd.wrapping_add(imm);
            set_zn(arm, result);
            add_set_vc(arm, rd, imm);
            arm.regs[rd_index] = result;
        }
    ),
    ("001 11 ddd iiiiiiii", "SUB rd, #imm8",
        |arm, _, op| {
            let imm = op.imm8();
            let rd_index = op.reg3(8);
            let rd: u32 = arm.regs[rd_index];
            let result = rd.wrapping_sub(imm);
            set_zn(arm, result);
            sub_set_vc(arm, rd, imm);
            arm.regs[rd_index] = result;
        }
    ),

    ("010000 0000 sss ddd", "and rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = rd & rs;
            set_zn(arm, result);
            arm.regs[rd_index] = result;
        })
    ),
    ("010000 0001 sss ddd", "eor rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = rd ^ rs;
            set_zn(arm, result);
            arm.regs[rd_index] = result;
        })
    ),
    ("010000 0010 sss ddd", "lsl rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = barrel_shift_lsl_set_flags(arm, rd, rs);
            set_zn(arm, result);
            arm.regs[rd_index] = result;
            ic.add_internal_cycles(1);
        })
    ),
    ("010000 0011 sss ddd", "lsr rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = barrel_shift_lsr_set_flags(arm, rd, rs);
            set_zn(arm, result);
            arm.regs[rd_index] = result;
            ic.add_internal_cycles(1);
        })
    ),
    ("010000 0100 sss ddd", "asr rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = barrel_shift_asr_set_flags(arm, rd, rs);
            set_zn(arm, result);
            arm.regs[rd_index] = result;
            ic.add_internal_cycles(1);
        })
    ),
    ("010000 0101 sss ddd", "adc rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = rd.wrapping_add(rs).wrapping_add(arm.cpsr.c as u32);
            set_zn(arm, result);
            add_set_vc(arm, rd, rs);
            arm.regs[rd_index] = result;
        })
    ),
    ("010000 0110 sss ddd", "sbc rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = rs.wrapping_sub(rs).wrapping_sub(!arm.cpsr.c as u32);
            set_zn(arm, result);
            sub_set_vc(arm, rd, rs);
            arm.regs[rd_index] = result;
        })
    ),
    ("010000 0111 sss ddd", "ror rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = barrel_shift_ror_set_flags(arm, rd, rs);
            set_zn(arm, result);
            arm.regs[rd_index] = result;
            ic.add_internal_cycles(1);
        })
    ),
    ("010000 1000 sss ddd", "tst rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, _| {
            set_zn(arm, rd & rs);
        })
    ),
    ("010000 1001 sss ddd", "neg rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, _, rd_index| {
            let result = (rs as i32).wrapping_neg() as u32;
            set_zn(arm, result);
            sub_set_vc(arm, 0, rs);
            arm.regs[rd_index] = result;
        })
    ),
    ("010000 1010 sss ddd", "cmp rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, _| {
            let result = rd.wrapping_sub(rs);
            set_zn(arm, result);
            sub_set_vc(arm, rd, rs);
        })
    ),
    ("010000 1011 sss ddd", "cmn rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, _| {
            let result = rs.wrapping_add(rd);
            set_zn(arm, result);
            add_set_vc(arm, rd, rs);
        })
    ),
    ("010000 1100 sss ddd", "orr rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = rd | rs;
            set_zn(arm, result);
            arm.regs[rd_index] = result;
        })
    ),
    ("010000 1101 sss ddd", "mul rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = rd.wrapping_mul(rs);
            set_zn(arm, result);
            ic.add_internal_cycles((rs.leading_zeros() / 8) as _);
            // MUL sets c and v to meaningless values, so we dont need to touch them.
            arm.regs[rd_index] = result;
        })
    ),
    ("010000 1110 sss ddd", "bic rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = rd & !rs;
            set_zn(arm, result);
            arm.regs[rd_index] = result;
        })
    ),
    ("010000 1111 sss ddd", "mvn rd, rs",
        |arm, ic, op| alu2_reg(arm, op, |arm, rs, rd, rd_index| {
            let result = !rs;
            set_zn(arm, result);
            arm.regs[rd_index] = result;
        })
    ),

    ("010001 00 ^^ sss ddd", "ADD Hd, Hs",
        |arm, ic, op| alu2_hreg(arm, op, |arm, rs, rd, rd_index| {
            let result = rd.wrapping_add(rs);
            arm.set_reg(ic, rd_index, result);
        })
    ),
    ("010001 01 ^^ sss ddd", "CMP Hd, Hs",
        |arm, _, op| alu2_hreg(arm, op, |arm, rs, rd, _| {
            let result = rd.wrapping_sub(rs);
            set_zn(arm, result);
            sub_set_vc(arm, rd, rs);
        })
    ),
    ("010001 10 ^^ sss ddd", "MOV Hd, Hs",
        |arm, ic, op| alu2_hreg(arm, op, |arm, rs, _, rd_index| {
            arm.set_reg(ic, rd_index, rs);
        })
    ),
    // TODO: What does d do for BX?
    ("010001 11 0 ssss ddd", "BX Hs",
        |arm, ic, op| alu2_hreg(arm, op, |arm, rs, _, _| {
            arm.branch_exchange(ic, rs);
        })
    ),

    ("01001 ddd iiiiiiii", "LDR Rd, [PC, #Imm]",
        |arm, ic, op| {
            let offset = op.imm8() << 2;
            let addr = (arm.regs[REG_PC] & !2).wrapping_add(offset);
            check_watchpoint!(arm, addr);
            arm.regs[op.reg3(8)] = ic.read32(addr);
            ic.add_internal_cycles(1); // internal cycle for address calculation
        }
    ),

    ("0101 000 ooo bbb ddd", "STR Rd, [Rb, Ro]",
        |arm, ic, op| {
            str_reg_offset(arm, ic, op, Interconnect::write32);
        }
    ),
    ("0101 001 ooo bbb ddd", "STRH Rd, [Rb, Ro]",
        |arm, ic, op| {
            str_reg_offset(arm, ic, op, Interconnect::write16);
        }
    ),
    ("0101 010 ooo bbb ddd", "STRB Rd, [Rb, Ro]",
        |arm, ic, op| {
            str_reg_offset(arm, ic, op, Interconnect::write8);
        }
    ),

    ("0101 100 ooo bbb ddd", "LDR Rd, [Rb, Ro]",
        |arm, ic, op| {
            ldr_reg_offset(arm, ic, op, Interconnect::read32);
        }
    ),
    ("0101 011 ooo bbb ddd", "LDRH Rd, [Rb, Ro]",
        |arm, ic, op| {
            ldr_reg_offset(arm, ic, op, Interconnect::read_ext_u16);
        }
    ),
    ("0101 110 ooo bbb ddd", "LDRB Rd, [Rb, Ro]",
        |arm, ic, op| {
            ldr_reg_offset(arm, ic, op, Interconnect::read_ext_u8);
        }
    ),
    ("0101 111 ooo bbb ddd", "LDSH Rd, [Rb, Ro]",
        |arm, ic, op| {
            ldr_reg_offset(arm, ic, op, Interconnect::read_ext_i16);
        }
    ),
    ("0101 101 ooo bbb ddd", "LDSB Rd, [Rb, Ro]",
        |arm, ic, op| {
            ldr_reg_offset(arm, ic, op, Interconnect::read_ext_i8);
        }
    ),

    ("011 00 iiiii bbb ddd", "STR Rd, [Rb, #Imm]",
        |arm, ic, op| {
            str_imm_offset(arm, ic, op, Interconnect::write32);
        }
    ),
    ("1000 0 iiiii bbb ddd", "STRH Rd, [Rb, #Imm]",
        |arm, ic, op| {
            str_imm_offset(arm, ic, op, Interconnect::write16);
        }
    ),
    ("011 10 iiiii bbb ddd", "STRB Rd, [Rb, #Imm]",
        |arm, ic, op| {
            str_imm_offset(arm, ic, op, Interconnect::write8);
        }
    ),

    ("011 01 iiiii bbb ddd", "LDR Rd, [Rb, #Imm]",
        |arm, ic, op| {
            ldr_imm_offset(arm, ic, op, size_of::<u32>(), Interconnect::read32);
        }
    ),
    ("1000 1 iiiii bbb ddd", "LDRH Rd, [Rb, #Imm]",
        |arm, ic, op| {
            ldr_imm_offset(arm, ic, op, size_of::<u16>(), Interconnect::read_ext_u16);
        }
    ),
    ("011 11 iiiii bbb ddd", "LDRB Rd, [Rb, #Imm]",
        |arm, ic, op| {
            ldr_imm_offset(arm, ic, op, size_of::<u8>(), Interconnect::read_ext_u8);
        }
    ),

    ("1001 0 ddd iiiiiiii", "STR Rd, [SP, #Imm]",
        |arm, ic, op| {
            let rd = arm.regs[op.reg3(8)];
            let offset = op.imm8() << 2;
            let addr = arm.regs[REG_SP].wrapping_add(offset);
            check_watchpoint!(arm, addr);
            ic.write32(addr, rd);
        }
    ),
    ("1001 1 ddd iiiiiiii", "LDR Rd, [SP, #Imm]",
        |arm, ic, op| {
            let offset = op.imm8() << 2;
            let addr = arm.regs[REG_SP].wrapping_add(offset);
            check_watchpoint!(arm, addr);
            arm.regs[op.reg3(8)] = ic.read32(addr);
            ic.add_internal_cycles(1); // internal cycle for address calculation
        }
    ),

    ("1010 0 ddd iiiiiiii", "ADD Rd, PC, #Imm",
        |arm, _, op| {
            arm.regs[op.reg3(8)] = arm.regs[REG_PC] + (op.imm8() << 2);
        }
    ),
    ("1010 1 ddd iiiiiiii", "ADD Rd, SP, #Imm",
        |arm, _, op| {
            arm.regs[op.reg3(8)] = arm.regs[REG_SP] + (op.imm8() << 2);
        }
    ),

    ("1011 0000 0iiiiiii", "ADD SP, #Imm",
        |arm, _, op| {
            arm.regs[REG_SP] += op.imm8();
        }
    ),
    ("1011 0000 1iiiiiii", "ADD SP, -#Imm",
        |arm, _, op| {
            arm.regs[REG_SP] -= op.imm8() & !0x80;
        }
    ),
];
