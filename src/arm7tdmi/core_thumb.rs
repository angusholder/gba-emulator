use num_traits::NumCast;

use super::{ REG_PC, REG_LR, REG_SP, Arm7TDMI };
use super::core_common::*;
use crate::arm7tdmi::disassemble::{ DisResult, err };
use crate::bus::Bus;

pub fn step_thumb(arm: &mut Arm7TDMI, op: ThumbOp) {
    arm.thumb_enc_table.lookup(op)(arm, op);
}

static REG_NAMES: [&str; 16] = [
    "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
    "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
];

#[derive(Clone, Copy)]
pub struct ThumbOp(u16);

impl ThumbOp {
    pub fn new(bits: u16) -> ThumbOp {
        ThumbOp(bits)
    }

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

    pub fn discriminant(&self) -> u32 { self.field(6, 10) }

    fn get_rd(&self) -> &'static str {
        REG_NAMES[self.field::<usize>(0, 3)]
    }
}

pub type ThumbEmuFn = fn(&mut Arm7TDMI, ThumbOp);

pub fn thumb_und(arm: &mut Arm7TDMI, _: ThumbOp) {
    arm.signal_undef();
}

fn shift_imm(arm: &mut Arm7TDMI, op: ThumbOp, f: impl FnOnce(&mut Arm7TDMI, u32, u32) -> u32) {
    let rs = arm.regs[op.reg3(3)];
    let imm = op.imm5(6);
    let result = f(arm, rs, imm);
    set_zn(arm, result);
    arm.regs[op.reg3(0)] = result;
}

fn alu3_reg(arm: &mut Arm7TDMI, op: ThumbOp, f: impl FnOnce(u32, u32) -> u32) {
    let rs = arm.regs[op.reg3(3)];
    let rn = arm.regs[op.reg3(6)];
    let result = f(rs, rn);
    set_zn(arm, result);
    add_set_vc(arm, rs, rn);
    arm.regs[op.reg3(0)] = result;
}

fn alu3_imm(arm: &mut Arm7TDMI, op: ThumbOp, f: impl FnOnce(u32, u32) -> u32) {
    let rs = arm.regs[op.reg3(3)];
    let rn = op.imm3(6);
    let result = f(rs, rn);
    set_zn(arm, result);
    add_set_vc(arm, rs, rn);
    arm.regs[op.reg3(0)] = result;
}

#[inline(always)]
fn decode_alu2_reg(arm: &mut Arm7TDMI, op: ThumbOp) -> (usize, u32, u32) {
    let rd_index = op.reg3(0);
    let rd = arm.regs[rd_index];
    let rs = arm.regs[op.reg3(3)];
    (rd_index, rd, rs)
}

fn alu2_and(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = rd & rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
}

fn alu2_eor(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = rd ^ rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
}

fn alu2_lsl(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = barrel_shift_lsl_set_flags(arm, rd, rs);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
    arm.bus.add_internal_cycles(1);
}

fn alu2_lsr(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = barrel_shift_lsr_set_flags(arm, rd, rs);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
    arm.bus.add_internal_cycles(1);
}

fn alu2_asr(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = barrel_shift_asr_set_flags(arm, rd, rs);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
    arm.bus.add_internal_cycles(1);
}

fn alu2_adc(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = rd.wrapping_add(rs).wrapping_add(arm.cpsr.c as u32);
    set_zn(arm, result);
    add_set_vc(arm, rd, rs);
    arm.regs[rd_index] = result;
}

fn alu2_sbc(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = rs.wrapping_sub(rs).wrapping_sub(!arm.cpsr.c as u32);
    set_zn(arm, result);
    sub_set_vc(arm, rd, rs);
    arm.regs[rd_index] = result;
}

fn alu2_ror(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = barrel_shift_ror_set_flags(arm, rd, rs);
    set_zn(arm, result);
    arm.regs[rd_index] = result;
    arm.bus.add_internal_cycles(1);
}

fn alu2_tst(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (_, rd, rs) = decode_alu2_reg(arm, op);
    set_zn(arm, rd & rs);
}

fn alu2_neg(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, _, rs) = decode_alu2_reg(arm, op);
    let result = (rs as i32).wrapping_neg() as u32;
    set_zn(arm, result);
    sub_set_vc(arm, 0, rs);
    arm.regs[rd_index] = result;
}

fn alu2_cmp(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (_, rd, rs) = decode_alu2_reg(arm, op);
    let result = rd.wrapping_sub(rs);
    set_zn(arm, result);
    sub_set_vc(arm, rd, rs);
}

fn alu2_cmn(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (_, rd, rs) = decode_alu2_reg(arm, op);
    let result = rs.wrapping_add(rd);
    set_zn(arm, result);
    add_set_vc(arm, rd, rs);
}

fn alu2_orr(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = rd | rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
}

fn alu2_mul(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = rd.wrapping_mul(rs);
    set_zn(arm, result);
    arm.bus.add_internal_cycles((rs.leading_zeros() / 8) as _);
    // MUL sets c and v to meaningless values, so we dont need to touch them.
    arm.regs[rd_index] = result;
}

fn alu2_bic(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, rd, rs) = decode_alu2_reg(arm, op);
    let result = rd & !rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
}

fn alu2_mvn(arm: &mut Arm7TDMI, op: ThumbOp) {
    let (rd_index, _, rs) = decode_alu2_reg(arm, op);
    let result = !rs;
    set_zn(arm, result);
    arm.regs[rd_index] = result;
}

fn alu2_hreg(arm: &mut Arm7TDMI, op: ThumbOp, f: impl FnOnce(&mut Arm7TDMI, u32, u32, usize)) {
    let rs = arm.regs[op.hrs()];
    let rd_index = op.hrd();
    let rd = arm.regs[rd_index];
    f(arm, rs, rd, rd_index);
}

fn str_reg(arm: &mut Arm7TDMI, op: ThumbOp) {
    str_reg_offset(arm, op, |bus, addr, value| bus.write32(addr, value))
}
fn strh_reg(arm: &mut Arm7TDMI, op: ThumbOp) {
    str_reg_offset(arm, op, |bus, addr, value| bus.write16(addr, value as u16))
}
fn strb_reg(arm: &mut Arm7TDMI, op: ThumbOp) {
    str_reg_offset(arm, op, |bus, addr, value| bus.write8(addr, value as u8))
}

fn str_reg_offset(arm: &mut Arm7TDMI, op: ThumbOp, store: impl Fn(&mut Bus, u32, u32)) {
    let rd = arm.regs[op.reg3(0)];
    let rb = arm.regs[op.reg3(3)];
    let ro = arm.regs[op.reg3(6)];
    let addr = rb.wrapping_add(ro);
    store(&mut *arm.bus, addr, rd);
}

fn ldr_reg(arm: &mut Arm7TDMI, op: ThumbOp) {
    ldr_reg_offset(arm, op, |bus, addr| bus.read32(addr))
}
fn ldrh_reg(arm: &mut Arm7TDMI, op: ThumbOp) {
    ldr_reg_offset(arm, op, |bus, addr| bus.read_ext_u16(addr))
}
fn ldrb_reg(arm: &mut Arm7TDMI, op: ThumbOp) {
    ldr_reg_offset(arm, op, |bus, addr| bus.read_ext_u8(addr))
}
fn ldsh_reg(arm: &mut Arm7TDMI, op: ThumbOp) {
    ldr_reg_offset(arm, op, |bus, addr| bus.read_ext_i16(addr))
}
fn ldsb_reg(arm: &mut Arm7TDMI, op: ThumbOp) {
    ldr_reg_offset(arm, op, |bus, addr| bus.read_ext_i8(addr))
}

fn ldr_reg_offset(arm: &mut Arm7TDMI, op: ThumbOp, load: impl Fn(&mut Bus, u32) -> u32) {
    let rb = arm.regs[op.reg3(3)];
    let ro = arm.regs[op.reg3(6)];
    let addr = rb.wrapping_add(ro);
    arm.regs[op.reg3(0)] = load(&mut *arm.bus, addr);
}

fn str_imm(arm: &mut Arm7TDMI, op: ThumbOp) {
    str_imm_offset(arm, op, 4, |bus, addr, value| bus.write32(addr, value))
}
fn strh_imm(arm: &mut Arm7TDMI, op: ThumbOp) {
    str_imm_offset(arm, op, 2, |bus, addr, value| bus.write16(addr, value as u16))
}
fn strb_imm(arm: &mut Arm7TDMI, op: ThumbOp) {
    str_imm_offset(arm, op, 1, |bus, addr, value| bus.write8(addr, value as u8))
}

fn str_imm_offset(arm: &mut Arm7TDMI, op: ThumbOp, word_size: u32, store: impl Fn(&mut Bus, u32, u32)) {
    let rd = arm.regs[op.reg3(0)];
    let rb = arm.regs[op.reg3(3)];
    let offset = op.imm5(6) * word_size;
    let addr = rb.wrapping_add(offset);
    store(&mut *arm.bus, addr, rd);
}

fn ldr_imm(arm: &mut Arm7TDMI, op: ThumbOp) {
    ldr_imm_offset(arm, op, 4, |bus, addr| bus.read32(addr));
}
fn ldrh_imm(arm: &mut Arm7TDMI, op: ThumbOp) {
    ldr_imm_offset(arm, op, 2, |bus, addr| bus.read_ext_u16(addr));
}
fn ldrb_imm(arm: &mut Arm7TDMI, op: ThumbOp) {
    ldr_imm_offset(arm, op, 1, |bus, addr| bus.read_ext_u8(addr));
}

fn ldr_imm_offset(arm: &mut Arm7TDMI, op: ThumbOp, word_size: u32, load: impl Fn(&mut Bus, u32) -> u32)
{
    let rb = arm.regs[op.reg3(3)];
    let offset = op.imm5(6) * word_size;
    let addr = rb.wrapping_add(offset);
    arm.regs[op.reg3(0)] = load(&mut *arm.bus, addr);
    // TODO: The instruction cycle times for the THUMB instruction are identical to that of the equivalent ARM instruction. For more information on instruction cycle times, please refer to Chapter 10, Instruction Cycle Operations.
    arm.bus.add_internal_cycles(1); // internal cycle for address calculation
}

fn branch_cc<COND: cond::Cond>(arm: &mut Arm7TDMI, op: ThumbOp) {
    let offset = (op.imm8() as i8 as u32) << 1;
    if arm.eval_condition_code(COND::CC) {
        let addr = arm.regs[REG_PC].wrapping_add(offset);
        arm.branch_to(addr);
    }
}

/// Legend:
/// [isndobljhrSWUNLPcpm] can be anything
/// At least one of the bits marked as ^ must be 1
pub static THUMB_DISPATCH_TABLE: &[(&str, &str, ThumbEmuFn)] = &[
    ("000 00 iiiii sss ddd", "LSL %Rd, %Rs, #shamt[i]",
        |arm, op| {
            shift_imm(arm, op, barrel_shift_lsl_set_flags);
        }
    ),
    ("000 01 iiiii sss ddd", "LSR %Rd, %Rs, #shamt[i]",
        |arm, op| {
            shift_imm(arm, op, barrel_shift_lsr_set_flags);
        }
    ),
    ("000 10 iiiii sss ddd", "ASR %Rd, %Rs, #shamt[i]",
        |arm, op| {
            shift_imm(arm, op, barrel_shift_asr_set_flags);
        }
    ),

    ("00011 00 nnn sss ddd", "ADD %Rd, %Rs, %Rn",
        |arm, op| {
            alu3_reg(arm, op, u32::wrapping_add)
        }
    ),
    ("00011 01 nnn sss ddd", "SUB %Rd, %Rs, %Rn",
        |arm, op| {
            alu3_reg(arm, op, u32::wrapping_sub)
        }
    ),
    ("00011 10 iii sss ddd", "ADD %Rd, %Rs, #imm[i]",
        |arm, op| {
            alu3_imm(arm, op, u32::wrapping_add)
        }
    ),
    ("00011 11 iii sss ddd", "SUB %Rd, %Rs, #imm[i]",
        |arm, op| {
            alu3_imm(arm, op, u32::wrapping_sub)
        }
    ),

    ("001 00 ddd iiiiiiii", "MOV %Rd, #imm[i]",
        |arm, op| {
            let imm = op.imm8();
            set_zn(arm, imm);
            arm.regs[op.reg3(8)] = imm;
        }
    ),
    ("001 01 ddd iiiiiiii", "CMP %Rd, #imm[i]",
        |arm, op| {
            let imm = op.imm8();
            let rd: u32 = arm.regs[op.reg3(8)];
            let result = rd.wrapping_sub(imm);
            set_zn(arm, result);
            sub_set_vc(arm, rd, imm);
        }
    ),
    ("001 10 ddd iiiiiiii", "ADD %Rd, #imm[i]",
        |arm, op| {
            let imm = op.imm8();
            let rd_index = op.reg3(8);
            let rd: u32 = arm.regs[rd_index];
            let result = rd.wrapping_add(imm);
            set_zn(arm, result);
            add_set_vc(arm, rd, imm);
            arm.regs[rd_index] = result;
        }
    ),
    ("001 11 ddd iiiiiiii", "SUB %Rd, #imm[i]",
        |arm, op| {
            let imm = op.imm8();
            let rd_index = op.reg3(8);
            let rd: u32 = arm.regs[rd_index];
            let result = rd.wrapping_sub(imm);
            set_zn(arm, result);
            sub_set_vc(arm, rd, imm);
            arm.regs[rd_index] = result;
        }
    ),

    ("010000 0000 sss ddd", "AND %Rd, %Rs", alu2_and),
    ("010000 0001 sss ddd", "EOR %Rd, %Rs", alu2_eor),
    ("010000 0010 sss ddd", "LSL %Rd, %Rs", alu2_lsl),
    ("010000 0011 sss ddd", "LSR %Rd, %Rs", alu2_lsr),
    ("010000 0100 sss ddd", "ASR %Rd, %Rs", alu2_asr),
    ("010000 0101 sss ddd", "ADC %Rd, %Rs", alu2_adc),
    ("010000 0110 sss ddd", "SBC %Rd, %Rs", alu2_sbc),
    ("010000 0111 sss ddd", "ROR %Rd, %Rs", alu2_ror),
    ("010000 1000 sss ddd", "TST %Rd, %Rs", alu2_tst),
    ("010000 1001 sss ddd", "NEG %Rd, %Rs", alu2_neg),
    ("010000 1010 sss ddd", "CMP %Rd, %Rs", alu2_cmp),
    ("010000 1011 sss ddd", "CMN %Rd, %Rs", alu2_cmn),
    ("010000 1100 sss ddd", "ORR %Rd, %Rs", alu2_orr),
    ("010000 1101 sss ddd", "MUL %Rd, %Rs", alu2_mul),
    ("010000 1110 sss ddd", "BIC %Rd, %Rs", alu2_bic),
    ("010000 1111 sss ddd", "MVN %Rd, %Rs", alu2_mvn),

    ("010001 00 ^^ sss ddd", "ADD %Hd, %Hs",
        |arm, op| alu2_hreg(arm, op, |arm, rs, rd, rd_index| {
            let result = rd.wrapping_add(rs);
            arm.set_reg(rd_index, result);
        })
    ),
    ("010001 01 ^^ sss ddd", "CMP %Hd, %Hs",
        |arm, op| alu2_hreg(arm, op, |arm, rs, rd, _| {
            let result = rd.wrapping_sub(rs);
            set_zn(arm, result);
            sub_set_vc(arm, rd, rs);
        })
    ),
    ("010001 10 ^^ sss ddd", "MOV %Hd, %Hs",
        |arm, op| alu2_hreg(arm, op, |arm, rs, _, rd_index| {
            arm.set_reg(rd_index, rs);
        })
    ),
    // TODO: What does d do for BX?
    ("010001 11 0 ssss ddd", "BX %Hs",
        |arm, op| alu2_hreg(arm, op, |arm, rs, _, _| {
            arm.branch_exchange(rs);
        })
    ),

    ("01001 ddd iiiiiiii", "LDR %Rd, [PC, #Imm[i]]",
        |arm, op| {
            let offset = op.imm8() << 2;
            let addr = (arm.regs[REG_PC] & !2).wrapping_add(offset);
            arm.regs[op.reg3(8)] = arm.bus.read32(addr);
            arm.bus.add_internal_cycles(1); // internal cycle for address calculation
        }
    ),

    ("0101 000 ooo bbb ddd", "STR %Rd, [%Rb, %Ro]",  str_reg),
    ("0101 001 ooo bbb ddd", "STRH %Rd, [%Rb, %Ro]", strh_reg),
    ("0101 010 ooo bbb ddd", "STRB %Rd, [%Rb, %Ro]", strb_reg),

    ("0101 100 ooo bbb ddd", "LDR %Rd, [%Rb, %Ro]",  ldr_reg),
    ("0101 011 ooo bbb ddd", "LDRH %Rd, [%Rb, %Ro]", ldrh_reg),
    ("0101 110 ooo bbb ddd", "LDRB %Rd, [%Rb, %Ro]", ldrb_reg),
    ("0101 111 ooo bbb ddd", "LDSH %Rd, [%Rb, %Ro]", ldsh_reg),
    ("0101 101 ooo bbb ddd", "LDSB %Rd, [%Rb, %Ro]", ldsb_reg),

    ("011 00 iiiii bbb ddd", "STR %Rd, [%Rb, #offset[i]]",  str_imm),
    ("1000 0 iiiii bbb ddd", "STRH %Rd, [%Rb, #offset[i]]", strh_imm),
    ("011 10 iiiii bbb ddd", "STRB %Rd, [%Rb, #offset[i]]", strb_imm),

    ("011 01 iiiii bbb ddd", "LDR %Rd, [%Rb, #offset[i]]",  ldr_imm),
    ("1000 1 iiiii bbb ddd", "LDRH %Rd, [%Rb, #offset[i]]", ldrh_imm),
    ("011 11 iiiii bbb ddd", "LDRB %Rd, [%Rb, #offset[i]]", ldrb_imm),

    ("1001 0 ddd iiiiiiii", "STR %Rd, [SP, #offset[i]]",
        |arm, op| {
            let rd = arm.regs[op.reg3(8)];
            let offset = op.imm8() << 2;
            let addr = arm.regs[REG_SP].wrapping_add(offset);
            arm.bus.write32(addr, rd);
        }
    ),
    ("1001 1 ddd iiiiiiii", "LDR %Rd, [SP, #offset[i]]",
        |arm, op| {
            let offset = op.imm8() << 2;
            let addr = arm.regs[REG_SP].wrapping_add(offset);
            arm.regs[op.reg3(8)] = arm.bus.read32(addr);
            arm.bus.add_internal_cycles(1); // internal cycle for address calculation
        }
    ),

    ("1010 0 ddd iiiiiiii", "ADD %Rd, PC, #offset[i]",
        |arm, op| {
            arm.regs[op.reg3(8)] = (arm.regs[REG_PC] & !3) + (op.imm8() << 2);
        }
    ),
    ("1010 1 ddd iiiiiiii", "ADD %Rd, SP, #offset[i]",
        |arm, op| {
            arm.regs[op.reg3(8)] = arm.regs[REG_SP] + (op.imm8() << 2);
        }
    ),

    ("1011 0000 0iiiiiii", "ADD SP, #imm[i]",
        |arm, op| {
            arm.regs[REG_SP] += op.imm8() << 2;
        }
    ),
    ("1011 0000 1iiiiiii", "ADD SP, -#imm[i]",
        |arm, op| {
            arm.regs[REG_SP] -= (op.imm8() & !0x80) << 2;
        }
    ),

    ("1011 010 lllllllll", "PUSH { %+Ul }",
        |arm, op| {
            let reglist = op.imm8();
            let store_lr = op.field::<u16>(8, 1) != 0;
            let mut sp = arm.regs[REG_SP] - reglist.count_ones() * 4;
            if store_lr { sp -= 4; }
            arm.regs[REG_SP] = sp; // writeback

            for i in 0..8 {
                if reglist & (1 << i) != 0 {
                    arm.bus.write32(sp, arm.regs[i]);
                    sp += 4;
                }
            }

            if store_lr {
                arm.bus.write32(sp, arm.regs[REG_LR]);
            }
        }
    ),
    ("1011 110 lllllllll", "POP { %+Ol }",
        |arm, op| {
            let reglist = op.imm8();
            let mut sp = arm.regs[REG_SP];
            let load_pc = op.field::<u16>(8, 1) != 0;

            for i in 0..8 {
                if reglist & (1 << i) != 0 {
                    arm.regs[i] = arm.bus.read32(sp);
                    sp += 4;
                }
            }

            if load_pc {
                let addr = arm.bus.read32(sp);
                sp += 4;
                arm.branch_to(addr & !1);
            }

            arm.regs[REG_SP] = sp;
        }
    ),

    // Not strictly an ArmV4 instruction, but if we want a software breakpoint instruction, we
    // might as well use what Arm actually ended up using
    ("1011 1110 iiiiiiii", "BKPT #imm[i]", |arm, op| {
        unimplemented!()
    }),

    ("1100 0 bbb llllllll", "STMIA %Rb!, { %+Ll }",
        |arm, op| {
            let rlist = op.imm8();
            let rb_index = op.reg3(8);
            let mut rb = arm.regs[rb_index];

            for i in 0..8 {
                if rlist & (1 << i) != 0 {
                    arm.bus.write32(rb, arm.regs[i]);
                    rb += 4;
                }
            }

            arm.regs[rb_index] = rb;
        }
    ),
    ("1100 1 bbb llllllll", "LDMIA %Rb!, { %+Ll }",
        |arm, op| {
            let rlist = op.imm8();
            let rb_index = op.reg3(8);
            let mut rb = arm.regs[rb_index];

            for i in 0..8 {
                if rlist & (1 << i) != 0 {
                    arm.regs[i] = arm.bus.read32(rb);
                    rb += 4;
                }
            }

            arm.regs[rb_index] = rb;
        }
    ),

    ("1101 0000 jjjjjjjj", "BEQ $label[j]", branch_cc::<cond::Eq>),
    ("1101 0001 jjjjjjjj", "BNE $label[j]", branch_cc::<cond::Ne>),
    ("1101 0010 jjjjjjjj", "BCS $label[j]", branch_cc::<cond::Cs>),
    ("1101 0011 jjjjjjjj", "BCC $label[j]", branch_cc::<cond::Cc>),
    ("1101 0100 jjjjjjjj", "BMI $label[j]", branch_cc::<cond::Mi>),
    ("1101 0101 jjjjjjjj", "BPL $label[j]", branch_cc::<cond::Pl>),
    ("1101 0110 jjjjjjjj", "BVS $label[j]", branch_cc::<cond::Vs>),
    ("1101 0111 jjjjjjjj", "BVC $label[j]", branch_cc::<cond::Vc>),
    ("1101 1000 jjjjjjjj", "BHI $label[j]", branch_cc::<cond::Hi>),
    ("1101 1001 jjjjjjjj", "BLS $label[j]", branch_cc::<cond::Ls>),
    ("1101 1010 jjjjjjjj", "BGE $label[j]", branch_cc::<cond::Ge>),
    ("1101 1011 jjjjjjjj", "BLT $label[j]", branch_cc::<cond::Lt>),
    ("1101 1100 jjjjjjjj", "BGT $label[j]", branch_cc::<cond::Gt>),
    ("1101 1101 jjjjjjjj", "BLE $label[j]", branch_cc::<cond::Le>),

    ("1101 1111 iiiiiiii", "SWI #[i]",
        |arm, _| {
            arm.signal_swi();
        }
    ),

    ("11100 jjjjjjjjjjj", "B $label[j]",
        |arm, op| {
            let offset = sign_extend(op.field(0, 11), 11) << 1;
            let addr = arm.regs[REG_PC].wrapping_add(offset);
            arm.branch_to(addr);
        }
    ),

    ("11110 jjjjjjjjjjj", "BL $longlabel[j]",
        |arm, op| {
            let hi_offset = sign_extend(op.field(0, 11), 11) << 12;
            arm.regs[REG_LR] = arm.regs[REG_PC].wrapping_add(hi_offset);
        }
    ),

    ("11111 jjjjjjjjjjj", "BLlow $label[j]",
        |arm, op| {
            let lo_offset = op.field::<u32>(0, 11) << 1;
            arm.regs[REG_LR] = arm.regs[REG_LR].wrapping_add(lo_offset);
            let temp = arm.regs[REG_PC];
            let target = arm.regs[REG_LR];
            arm.branch_to(target);

            // LR contains address of instruction following this,
            // and has bit0 set to force thumb mode upon return.
            arm.regs[REG_LR] = (temp - 2) | 1;
        }
    ),
];

fn process_thumb(fmt: &str, exec: ThumbEmuFn) -> DisResult<ThumbEnc> {
    process_bit_format(fmt, |i| 6 <= i && i < 16)
        .map(|bits| ThumbEnc(bits, exec))
}

struct ThumbEnc(Vec<Bit>, ThumbEmuFn);

impl ThumbEnc {
    fn try_match(&self, op: ThumbOp) -> Option<ThumbEmuFn> {
        self.try_match_discriminant(op.discriminant())
    }

    fn try_match_discriminant(&self, discriminant: u32) -> Option<ThumbEmuFn> {
        if encoding_matches(&self.0, discriminant) {
            Some(self.1)
        } else {
            None
        }
    }
}

lazy_static! {
    static ref TABLE: ThumbEncTable = ThumbEncTable::compute();
}

#[derive(Clone)]
pub struct ThumbEncTable {
    level1: Vec<u8>,
    level2: Vec<ThumbEmuFn>,
}

impl ThumbEncTable {
    pub fn get_instance() -> &'static ThumbEncTable {
        &TABLE
    }

    fn compute() -> ThumbEncTable {
        let thumb_encodings = THUMB_DISPATCH_TABLE.iter()
            .map(|&(spec, _, exec)| process_thumb(spec, exec))
            .collect::<DisResult<Vec<ThumbEnc>>>().unwrap();

        let mut thumb_fns = Vec::<ThumbEmuFn>::new();

        let thumb_fn_indices = (0..=0b1111_1111_11).map(|discriminant| {
            let encs = thumb_encodings.iter()
                .filter_map(|enc| enc.try_match_discriminant(discriminant))
                .collect::<Vec<ThumbEmuFn>>();

            let enc: ThumbEmuFn = match encs.len() {
                0 => Ok(thumb_und as ThumbEmuFn),
                1 => Ok(encs[0]),
                _ => Err(err(format!("More than 1 encoding matched discriminant {:03X}", discriminant))),
            }?;

            Ok(thumb_fns.iter()
                .position(|&e| (e as usize) == (enc as usize))
                .unwrap_or_else(|| {
                    thumb_fns.push(enc);
                    thumb_fns.len() - 1
                }) as u8)
        }).collect::<DisResult<Vec<u8>>>().unwrap();

        ThumbEncTable { level1: thumb_fn_indices, level2: thumb_fns }
    }

    pub fn lookup(&self, op: ThumbOp) -> ThumbEmuFn {
        let discr = op.discriminant();
        let l2_index = self.level1[discr as usize];
        self.level2[l2_index as usize]
    }
}

#[test]
fn parse_thumb_dispatch_table() {
    // Force evaluation of the lazy_static
    ThumbEncTable::get_instance();
}