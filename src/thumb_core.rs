use arm7tdmi::*;
use std::mem;

#[inline(always)]
fn set_zn(arm: &mut Arm7TDMI, value: u32) {
    arm.cpsr.z = value == 0;
    arm.cpsr.n = (value >> 31) != 0;
}

#[inline(always)]
fn add_set_vc(arm: &mut Arm7TDMI, a: u32, b: u32, r: u32) {
    arm.cpsr.v = (!(a ^ b) ^ r) >> 31 != 0;
    // arm.cpsr.v = ((a & b & !r) | (!a & !b & r)) >> 31 != 0;
    arm.cpsr.c = ((a & b) | (a & !r) | (b & !r)) >> 31 != 0;
}

#[inline(always)]
fn sub_set_vc(arm: &mut Arm7TDMI, a: u32, b: u32, r: u32) {
    arm.cpsr.v = (a ^ !(b ^ r)) >> 31 != 0;
    // arm.cpsr.v = ((a & !b & !r) | (!a & b & r)) >> 31 != 0;
    arm.cpsr.c = ((a & !b) | (a & !r) | (!b & !r)) >> 31 != 0;
}

macro_rules! load_op_reg_offset {
    ($arm:expr, $op:expr, $T:ty, $load_fn:ident) => {
        let rb = $arm.regs[($op >> 3 & 7) as usize];
        let ro = $arm.regs[($op >> 6 & 7) as usize];
        let addr = rb.wrapping_add(ro);
        $arm.regs[($op & 7) as usize] = ($arm.mem.$load_fn(addr) as $T) as u32;
    }
}

macro_rules! store_op_reg_offset {
    ($arm:expr, $op:expr, $T:ty, $store_fn:ident) => {
        let rb = $arm.regs[($op >> 3 & 7) as usize];
        let ro = $arm.regs[($op >> 6 & 7) as usize];
        let addr = rb.wrapping_add(ro);
        $arm.mem.$store_fn(addr, $arm.regs[($op & 7) as usize] as $T);
    }
}

macro_rules! load_op_immed_offset {
    ($arm:expr, $op:expr, $T:ty, $load_fn:ident) => {
        let rb = $arm.regs[($op >> 3 & 7) as usize];
        let offset = (($op >> 6 & 0x1F) as u32) * mem::size_of::<$T>() as u32;
        let addr = rb.wrapping_add(offset);
        $arm.regs[($op & 7) as usize] = ($arm.mem.$load_fn(addr) as $T) as u32;
    }
}

macro_rules! store_op_immed_offset {
    ($arm:expr, $op:expr, $T:ty, $store_fn:ident) => {
        let rb = $arm.regs[($op >> 3 & 7) as usize];
        let offset = (($op >> 6 & 0x1F) as u32) * mem::size_of::<$T>() as u32;
        let addr = rb.wrapping_add(offset);
        $arm.mem.$store_fn(addr, $arm.regs[($op & 7) as usize] as $T);
    }
}

#[inline(always)]
fn sign_extend(n: u32, n_bits: usize) -> u32 {
    let lower_mask = (1 << n_bits) - 1;
    let upper_mask = !lower_mask;
    if (n & (1 << (n_bits - 1))) != 0 {
        upper_mask | n
    } else {
        n
    }
}

pub fn step_thumb(arm: &mut Arm7TDMI, op: u16) {
    // Make op 32-bit for convenience.
    let op = op as u32;

    match op >> 8 {
        0x00 ... 0x07 => { // LSL
            let rs = arm.regs[(op >> 3 & 7) as usize];
            let imm = op >> 6 & 0x1F;
            let result = rs << imm;
            set_zn(arm, result);
            arm.regs[(op & 7) as usize] = result;
        }

        0x08 ... 0x0F => { // LSR
            let rs = arm.regs[(op >> 3 & 7) as usize];
            let imm = op >> 6 & 0x1F;
            let result = rs >> imm;
            set_zn(arm, result);
            arm.regs[(op & 7) as usize] = result;
        }

        0x10 ... 0x17 => { // ASR
            let rs = arm.regs[(op >> 3 & 7) as usize];
            let imm = op >> 6 & 0x1F;
            let result = ((rs as i32) >> imm) as u32;
            set_zn(arm, result);
            arm.regs[(op & 7) as usize] = result;
        }

        0x18 | 0x19 => { // ADD Rd, Rs, Rn
            let rs = arm.regs[(op >> 3 & 7) as usize];
            let rn = arm.regs[(op >> 6 & 7) as usize];
            let result = rs.wrapping_add(rn);
            set_zn(arm, result);
            add_set_vc(arm, rs, rn, result);
            arm.regs[(op & 7) as usize] = result;
        }

        0x1A | 0x1B => { // SUB Rd, Rs, Rn
            let rs = arm.regs[(op >> 3 & 7) as usize];
            let rn = arm.regs[(op >> 6 & 7) as usize];
            let result = rs - rn;
            set_zn(arm, result);
            sub_set_vc(arm, rs, rn, result);
            arm.regs[(op & 7) as usize] = result;
        }

        0x1C | 0x1D => { // ADD Rd, #imm3
            let rs = arm.regs[(op >> 3 & 7) as usize];
            let imm = op >> 6 & 7;
            let result = rs.wrapping_add(imm);
            set_zn(arm, result);
            add_set_vc(arm, rs, imm, result);
            arm.regs[(op & 7) as usize] = result;
        }

        0x1E | 0x1F => { // SUB Rd, #imm3
            let rs = arm.regs[(op >> 3 & 7) as usize];
            let imm = op >> 6 & 7;
            let result = rs.wrapping_sub(imm);
            set_zn(arm, result);
            sub_set_vc(arm, rs, imm, result);
            arm.regs[(op & 7) as usize] = result;
        }

        0x20 ... 0x27 => { // MOV Rd, #imm8
            let imm = op & 0xFF;
            set_zn(arm, imm);
            arm.regs[(op >> 8 & 7) as usize] = imm;
        }

        0x28 ... 0x2F => { // CMP Rd, #imm8
            let imm = op & 0xFF;
            let rd = arm.regs[(op >> 8 & 7) as usize];
            let result = rd.wrapping_sub(imm);
            set_zn(arm, result);
            sub_set_vc(arm, rd, imm, result);
        }

        0x30 ... 0x37 => { // ADD Rd, #imm8
            let imm = op & 0xFF;
            let rd_index = (op >> 8 & 7) as usize;
            let rd = arm.regs[rd_index];
            let result = rd.wrapping_add(imm);
            set_zn(arm, result);
            add_set_vc(arm, rd, imm, result);
            arm.regs[rd_index] = result;
        }

        0x38 ... 0x3F => { // SUB Rd, #imm8
            let imm = op & 0xFF;
            let rd_index = (op >> 8 & 7) as usize;
            let rd = arm.regs[rd_index];
            let result = rd.wrapping_sub(imm);
            set_zn(arm, result);
            sub_set_vc(arm, rd, imm, result);
            arm.regs[rd_index] = result;
        }

        0x40 ... 0x43 => { // ALU_OP Rd, Rs
            let rd_index = (op & 7) as usize;
            let rd = arm.regs[rd_index];
            let rs = arm.regs[(op >> 3 & 7) as usize];
            let result: u32;

            match op >> 6 & 0xF {
                0b0000 => { // AND Rd, Rs
                    result = rd & rs;
                    set_zn(arm, result);
                }
                0b0001 => { // EOR Rd, Rs
                    result = rd ^ rs;
                    set_zn(arm, result);
                }
                0b0010 => { // LSL Rd, Rs
                    result = rd << rs;
                    set_zn(arm, result);
                    arm.cpsr.c = ((rd >> (32 - rs)) & 1) != 0;
                }
                0b0011 => { // LSR Rd, Rs
                    result = rd >> rs;
                    set_zn(arm, result);
                    arm.cpsr.c = ((rd >> (rs - 1)) & 1) != 0;
                }
                0b0100 => { // ASR Rd, Rs
                    result = ((rd as i32) >> rs) as u32;
                    set_zn(arm, result);
                    arm.cpsr.c = ((rd >> (rs - 1)) & 1) != 0;
                }
                0b0101 => { // ADC Rd, Rs
                    result = rd.wrapping_add(rs).wrapping_add(arm.cpsr.c as u32);
                    set_zn(arm, result);
                    add_set_vc(arm, rd, rs, result);
                }
                0b0110 => { // SBC Rd, Rs
                    result = rs.wrapping_sub(rs).wrapping_sub(!arm.cpsr.c as u32);
                    set_zn(arm, result);
                    sub_set_vc(arm, rd, rs, result);
                }
                0b0111 => { // ROR Rd, Rs
                    result = rd.rotate_right(rs);
                    set_zn(arm, result);
                    arm.cpsr.c = ((rd >> ((rs - 1) & 0x1F)) & 1) != 0;
                }
                0b1000 => { // TST Rd, Rs
                    set_zn(arm, rd & rs);
                    return;
                }
                0b1001 => { // NEG Rd, Rs
                    result = -(rs as i32) as u32;
                    set_zn(arm, result);
                    sub_set_vc(arm, 0, rs, result);
                }
                0b1010 => { // CMP Rd, Rs
                    result = rd - rs;
                    set_zn(arm, result);
                    sub_set_vc(arm, rd, rs, result);
                    return;
                }
                0b1011 => { // CMN Rd, Rs
                    result = rs.wrapping_add(rd);
                    set_zn(arm, result);
                    add_set_vc(arm, rd, rs, result);
                    return;
                }
                0b1100 => { // ORR Rd, Rs
                    result = rd | rs;
                    set_zn(arm, result);
                }
                0b1101 => { // MUL Rd, Rs
                    result = rd * rs;
                    set_zn(arm, result);
                    // MUL sets c and v to meaningless values, so we don't need to touch them.
                }
                0b1110 => { // BIC Rd, Rs
                    result = rd & !rs;
                    set_zn(arm, result);
                }
                0b1111 => { // MVN Rd, Rs
                    result = !rs;
                    set_zn(arm, result);
                }
                _ => unreachable!(),
            }
            arm.regs[rd_index] = result;
        }

        0x44 => { // ADD
            let rs = arm.regs[(op >> 3 & 0xF) as usize];
            let rd_index = ((op & 0b111) | ((op & 0x80) >> 4)) as usize;
            arm.regs[rd_index] = arm.regs[rd_index].wrapping_add(rs);
        }
        0x45 => { // CMP
            let rs = arm.regs[(op >> 3 & 0xF) as usize];
            let rd = arm.regs[((op & 0b111) | ((op & 0x80) >> 4)) as usize];
            let result = rd.wrapping_sub(rs);
            set_zn(arm, result);
            sub_set_vc(arm, rd, rs, result);
        }
        0x46 => { // MOV
            let rs = arm.regs[(op >> 3 & 0xF) as usize];
            let rd_index = ((op & 0b111) | ((op & 0x80) >> 4)) as usize;
            arm.regs[rd_index] = rs;
        }
        0x47 => { // BX
            let rs = arm.regs[(op >> 3 & 0xF) as usize];
            arm.branch_exchange(rs);
        }

        0x48 ... 0x4F => { // LDR Rd, [PC, #imm8]
            let offset = (op & 0xFF) << 2;
            let addr = (arm.regs[REG_PC] & !2) + offset;
            arm.regs[(op >> 8 & 7) as usize] = arm.mem.read32(addr);
        }

        0x50 | 0x51 => { // STR Rd, [Rb, Ro]
            store_op_reg_offset!(arm, op, u8, write8);
        }
        0x52 | 0x53 => { // STRH Rd, [Rb, Ro]
            store_op_reg_offset!(arm, op, u16, write16);
        }
        0x54 | 0x55 => { // STRB Rd, [Rb, Ro]
            store_op_reg_offset!(arm, op, u32, write32);
        }
        0x56 | 0x57 => { // LDSB Rd, [Rb, Ro]
            load_op_reg_offset!(arm, op, i8, read8);
        }
        0x58 | 0x59 => { // LDR Rd, [Rb, Ro]
            load_op_reg_offset!(arm, op, u32, read32);
        }
        0x5A | 0x5B => { // LDRH Rd, [Rb, Ro]
            load_op_reg_offset!(arm, op, u16, read16);
        }
        0x5C | 0x5D => { // LDRB Rd, [Rb, Ro]
            load_op_reg_offset!(arm, op, u8, read8);
        }
        0x5E | 0x5F => { // LDSH Rd, [Rb, Ro]
            load_op_reg_offset!(arm, op, i16, read16);
        }

        0x60 ... 0x67 => { // STR Rd, [RB, #imm5]
            store_op_immed_offset!(arm, op, u32, write32);
        }
        0x68 ... 0x6F => { // LDR Rd, [RB, #imm5]
            load_op_immed_offset!(arm, op, u32, read32);
        }
        0x70 ... 0x77 => { // STRB Rd, [RB, #imm5]
            store_op_immed_offset!(arm, op, u8, write8);
        }
        0x78 ... 0x7F => { // LDRB Rd, [RB, #imm5]
            load_op_immed_offset!(arm, op, u8, read8);
        }
        0x80 ... 0x87 => { // STRH Rd, [RB, #imm5]
            store_op_immed_offset!(arm, op, u16, write16);
        }
        0x88 ... 0x8F => { // LDRH Rd, [RB, #imm5]
            load_op_immed_offset!(arm, op, u16, read16);
        }

        0x90 ... 0x97 => { // STR Rd, [SP, #imm8]
            let rd = arm.regs[(op >> 8 & 7) as usize];
            let offset = (op & 0xFF) << 2;
            let addr = arm.regs[REG_SP].wrapping_add(offset);
            arm.mem.write32(addr, rd);
        }
        0x98 ... 0x9F => { // LDR Rd, [SP, #imm8]
            let offset = (op & 0xFF) << 2;
            let addr = arm.regs[REG_SP].wrapping_add(offset);
            arm.regs[(op >> 8 & 7) as usize] = arm.mem.read32(addr);
        }

        0xA0 ... 0xA7 => { // ADD Rd, PC, #imm8
            let imm = (op & 0xF) << 2;
            arm.regs[(op >> 8 & 7) as usize] = arm.regs[REG_PC] + imm;
        }
        0xA8 ... 0xAF => { // ADD Rd, SP, #imm8
            let imm = (op & 0xF) << 2;
            arm.regs[(op >> 8 & 7) as usize] = arm.regs[REG_SP] + imm;
        }

        0xB0 => { // ADD SP, {+-}#imm7
            let offset = (op & 0x7F) << 2;
            if (op & 0x80) == 0 {
                arm.regs[REG_SP] += offset;
            } else {
                arm.regs[REG_SP] -= offset;
            }
        }

        0xB4 | 0xB5 => { // PUSH { rlist, [LR] }
            let mut sp = arm.regs[REG_SP];
            let store_lr = (op & 0x0100) != 0;

            for i in 0..8 {
                if op & (1 << i) != 0 {
                    sp -= 4;
                    arm.mem.write32(sp, arm.regs[i]);
                }
            }

            if store_lr {
                sp -= 4;
                arm.mem.write32(sp, arm.regs[REG_LR]);
            }

            arm.regs[REG_SP] = sp;
        }
        0xBC | 0xBD => { // POP { rlist, [PC] }
            let mut sp = arm.regs[REG_SP];
            let load_pc = (op & 0x0100) != 0;

            for i in 0..8 {
                if op & (1 << i) != 0 {
                    arm.regs[i] = arm.mem.read32(sp);
                    sp += 4;
                }
            }

            if load_pc {
                let addr = arm.mem.read32(sp);
                sp += 4;
                arm.branch_to(addr);
            }

            arm.regs[REG_SP] = sp;
        }

        0xC0 ... 0xC7 => { // STMIA Rb!, { Rlist }
            let rb_index = (op >> 8 & 7) as usize;
            let mut rb = arm.regs[rb_index];

            for i in 0..8 {
                if op & (1 << i) != 0 {
                    arm.mem.write32(rb, arm.regs[i]);
                    rb += 4;
                }
            }

            arm.regs[rb_index] = rb;
        }
        0xC8 ... 0xCF => { // LDMIA Rb!, { Rlist }
            let rb_index = (op >> 8 & 7) as usize;
            let mut rb = arm.regs[rb_index];

            for i in 0..8 {
                if op & (1 << i) != 0 {
                    arm.regs[i] = arm.mem.read32(rb);
                    rb += 4;
                }
            }

            arm.regs[rb_index] = rb;
        }

        0xD0 ... 0xDD => { // Bcc #SOffset9
            let offset = sign_extend(op & 0xFF, 8) << 1;
            let cond = ConditionCode::from((op >> 8 & 0xF) as u32);
            if arm.eval_condition_code(cond) {
                let addr = arm.regs[REG_PC].wrapping_add(offset);
                arm.branch_to(addr);
            }
        }

        0xDE => unreachable!(), // B{ALWAYS} is undefined

        0xDF => { // SWI #value8
            // let comment = (op & 0xFF) as u8;
            // arm.execute_swi(comment);
            unimplemented!();
        }

        0xE0 ... 0xE7 => { // B #SOffset12
            let offset = sign_extend(op & 0x7FF, 11) << 1;
            let addr = arm.regs[REG_PC].wrapping_add(offset);
            arm.branch_to(addr);
        }

        0xF0 ... 0xF7 => { // BL (high)
            let hi_offset = sign_extend(op & 0x7FF, 11) << 12;
            arm.regs[REG_LR] = arm.regs[REG_PC].wrapping_add(hi_offset);
        }
        0xF8 ... 0xFF => { // BL (low)
            let lo_offset = (op & 0x7FF) << 1;
            arm.regs[REG_LR] = arm.regs[REG_LR].wrapping_add(lo_offset);
            let temp = arm.regs[REG_PC];
            let target = arm.regs[REG_LR];
            arm.branch_to(target);

            // LR contains address of instruction following this,
            // and has bit0 set to force thumb mode upon return.
            arm.regs[REG_LR] = (temp - 2) | 1;
        }

        _ => unreachable!(),
    }
}
