use std::cmp;

use num::FromPrimitive;

use super::{ Arm7TDMI, REG_PC, REG_LR, ConditionCode, StepEvent };
use interconnect::Interconnect;
use super::core_common::*;
use log::*;

pub fn step_arm(arm: &mut Arm7TDMI, _interconnect: &mut Interconnect, op: u32) -> StepEvent {
    let cond = ConditionCode::from_u32(op >> 28).unwrap();
    if !arm.eval_condition_code(cond) {
        return StepEvent::None;
    }

    let _discr = (op >> 4 & 0xF) | (op >> 16 & 0xFF0);
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

type ArmEmuFn = fn(&mut Arm7TDMI, &mut Interconnect, ArmOp);

fn unhandled(_: &mut Arm7TDMI, _: &mut Interconnect, op: u32) {
    error!(CPU, "Arm instruction {:08X} wasn't handled by the decoder", op);
}

fn op_coprocessor(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, _: ArmOp) {
    arm.signal_undef(interconnect);
}

fn op_und(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, _: ArmOp) {
    arm.signal_undef(interconnect);
}

fn op_swp(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) {
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
}

fn op_swpb(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) {
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
}

fn op_mrs_reg(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) {
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
}

fn op_msr_reg(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) {
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
}

fn op_msr_flag_imm(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) {
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
}

fn op_bx(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: ArmOp) {
    if op.field(8, 12) != 0xFFF {
        return op_und(arm, interconnect, op);
    }

    let rn = arm.regs[op.reg(0)];
    arm.branch_exchange(interconnect, rn);
}

fn op_swi(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, _: ArmOp) {
    arm.signal_swi(interconnect);
}

fn mul(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ArmOp) {
    let rd_index = op.reg(16);
    let rn_index = op.reg(12);
    let rs_index = op.reg(8);
    let rm_index = op.reg(0);

    let setcc = op.flag(20);
    let acc = op.flag(21);

    assert!(rd_index != rm_index);
    assert!(rd_index != REG_PC);
    assert!(rs_index != REG_PC);
    assert!(rn_index != REG_PC);
    assert!(rm_index != REG_PC);

    let rn = arm.regs[rn_index];
    let rs = arm.regs[rs_index];
    let rm = arm.regs[rm_index];

    ic.add_internal_cycles((rs.leading_zeros() / 8) as _);

    let result = if acc {
        ic.add_internal_cycles(1);
        rm.wrapping_mul(rs).wrapping_add(rn)
    } else {
        rm.wrapping_mul(rs)
    };

    arm.regs[rd_index] = result;

    if setcc {
        set_zn(arm, result);
    }
}

fn mul_long(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ArmOp) {
    let rd_hi_index = op.reg(16);
    let rd_lo_index = op.reg(12);
    let rs_index = op.reg(8);
    let rm_index = op.reg(0);

    let setcc = op.flag(20);
    let acc = op.flag(21);
    let signed = op.flag(22);

    assert!(rd_hi_index != REG_PC);
    assert!(rd_lo_index != REG_PC);
    assert!(rs_index != REG_PC);
    assert!(rm_index != REG_PC);
    assert!(rd_hi_index != rd_lo_index);
    assert!(rd_hi_index != rm_index);
    assert!(rd_lo_index != rm_index);

    let rs = arm.regs[rs_index];

    if signed {
        ic.add_internal_cycles((1 + cmp::max(rs.leading_zeros(), (!rs).leading_zeros()) / 8) as _);
    } else {
        ic.add_internal_cycles((1 + rs.leading_zeros() / 8) as _);
    }

    if acc {
        ic.add_internal_cycles(1);
    }

    let result: u64 = if signed {
        let rm = arm.regs[rm_index] as i32 as i64;
        let rs = arm.regs[rs_index] as i32 as i64;

        if acc {
            let rlo = arm.regs[rd_lo_index] as i32 as i64;
            let rhi = arm.regs[rd_hi_index] as i32 as i64;
            let rfull = rlo | (rhi << 32);
            rm.wrapping_mul(rs).wrapping_add(rfull) as u64
        } else {
            rm.wrapping_mul(rs) as u64
        }
    } else {
        let rm = arm.regs[rm_index] as u64;
        let rs = arm.regs[rs_index] as u64;

        if acc {
            let rlo = arm.regs[rd_lo_index] as u64;
            let rhi = arm.regs[rd_hi_index] as u64;
            let rfull = rlo | (rhi << 32);
            rm.wrapping_mul(rs).wrapping_add(rfull)
        } else {
            rm.wrapping_mul(rs)
        }
    };

    arm.regs[rd_lo_index] = result as u32;
    arm.regs[rd_hi_index] = (result >> 32) as u32;

    if setcc {
        arm.cpsr.c = result == 0;
        arm.cpsr.n = (result as i64) < 0;
    }
}

fn block_data_transfer<F>(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ArmOp, f: F)
    where F: Fn(&mut Arm7TDMI, &mut Interconnect, u16, u32) -> u32
{
    let rn_index = op.reg(16);
//    let load = op.flag(20);
    let writeback = op.flag(21);
    let set_cc = op.flag(22);
//    let up = op.flag(23);
//    let preindex = op.flag(24);
    let reglist = op.field(0, 16) as u16;

    if set_cc {
        unimplemented!("block data transfer set_cc");
    }

    let mut addr = arm.regs[rn_index];

    addr = f(arm, ic, reglist, addr);

    if writeback {
        assert!(rn_index != REG_PC);
        arm.regs[rn_index] = addr;
    }
}

fn ldmia(arm: &mut Arm7TDMI, ic: &mut Interconnect, reglist: u16, mut addr: u32) -> u32 {
    for i in 0usize..16 { // Post-increment
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            let value = ic.read32(addr);
            arm.set_reg(ic, i, value);
            addr += 4;
        }
    }
    addr
}

fn ldmib(arm: &mut Arm7TDMI, ic: &mut Interconnect, reglist: u16, mut addr: u32) -> u32 {
    for i in 0usize..16 {
        if reglist & (1 << i) != 0 {
            addr += 4;
            check_watchpoint!(arm, addr);
            let value = ic.read32(addr);
            arm.set_reg(ic, i, value);
        }
    }
    addr
}

fn ldmda(arm: &mut Arm7TDMI, ic: &mut Interconnect, reglist: u16, mut addr: u32) -> u32 {
    let new_base = addr - reglist.count_ones() * 4;
    addr = addr - reglist.count_ones() * 4 + 4;
    for i in 0usize..16 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            let value = ic.read32(addr);
            arm.set_reg(ic, i, value);
            addr += 4;
        }
    }
    new_base
}

fn ldmdb(arm: &mut Arm7TDMI, ic: &mut Interconnect, reglist: u16, mut addr: u32) -> u32 {
    let new_base = addr - reglist.count_ones() * 4;
    addr = addr - reglist.count_ones() * 4;
    for i in 0usize..16 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            let value = ic.read32(addr);
            arm.set_reg(ic, i, value);
            addr += 4;
        }
    }
    new_base
}

fn get_reg(arm: &Arm7TDMI, index: usize) -> u32 {
    if index == REG_PC {
        // address of current instruction + 12 is stored
        arm.regs[REG_PC] + 4
    } else {
        arm.regs[index]
    }
}

fn stmia(arm: &mut Arm7TDMI, ic: &mut Interconnect, reglist: u16, mut addr: u32) -> u32 {
    for i in 0usize..16 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            ic.write32(addr, get_reg(arm, i));
            addr += 4;
        }
    }
    addr
}

fn stmib(arm: &mut Arm7TDMI, ic: &mut Interconnect, reglist: u16, mut addr: u32) -> u32 {
    for i in 0usize..16 {
        if reglist & (1 << i) != 0 {
            addr += 4;
            check_watchpoint!(arm, addr);
            ic.write32(addr, get_reg(arm, i));
        }
    }
    addr
}

fn stmda(arm: &mut Arm7TDMI, ic: &mut Interconnect, reglist: u16, mut addr: u32) -> u32 {
    let new_base = addr - reglist.count_ones() * 4;
    addr = addr - reglist.count_ones() * 4 + 4;
    for i in 0usize..16 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            ic.write32(addr, get_reg(arm, i));
            addr += 4;
        }
    }
    new_base
}

fn stmdb(arm: &mut Arm7TDMI, ic: &mut Interconnect, reglist: u16, mut addr: u32) -> u32 {
    let new_base = addr - reglist.count_ones() * 4;
    addr = addr - reglist.count_ones() * 4;
    for i in 0usize..16 {
        if reglist & (1 << i) != 0 {
            check_watchpoint!(arm, addr);
            ic.write32(addr, get_reg(arm, i));
            addr += 4;
        }
    }
    new_base
}

fn branch(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ArmOp) {
    let offset = op.field(0, 24);
    let link = op.flag(24);

    let offset = sign_extend(offset, 24);

    let pc = arm.regs[REG_PC];
    let addr = pc.wrapping_add(offset);
    if link {
        arm.regs[REG_LR] = pc - 4;
    }
    arm.branch_to(ic, addr);
}

fn get_rotated_immediate(op: ArmOp) -> u32 {
    let imm = op.field(0, 8);
    let rotate = op.field(8, 4);
    imm.rotate_right(rotate * 2)
}

fn get_shifted_register(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ArmOp, setcc: bool) -> u32 {
    const LSL: u32 = 0b00;
    const LSR: u32 = 0b01;
    const ASR: u32 = 0b10;
    const ROR: u32 = 0b11;

    let rm_index = op.reg(0);
    let shift_by_reg = op.flag(4);
    let shift_type = op.field(5, 2);

    let rm = arm.regs[rm_index];
    let shift_amount = if shift_by_reg {
        ic.add_internal_cycles(1);
        arm.regs[op.reg(8)]
    } else {
        op.field(7, 5)
    };

    let shift_func = match (shift_type, setcc) {
        (LSL, false) => barrel_shift_lsl,
        (LSL, true) => barrel_shift_lsl_set_flags,
        (LSR, false) => barrel_shift_lsr,
        (LSR, true) => barrel_shift_lsr_set_flags,
        (ASR, false) => barrel_shift_asr,
        (ASR, true) => barrel_shift_asr_set_flags,
        (ROR, false) => barrel_shift_ror,
        (ROR, true) => barrel_shift_ror_set_flags,
        _ => unreachable!()
    };
    shift_func(arm, rm, shift_amount)
}

fn data_processing(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ArmOp) {
    const AND: u32 = 0b0000;
    const EOR: u32 = 0b0001;
    const SUB: u32 = 0b0010;
    const RSB: u32 = 0b0011;
    const ADD: u32 = 0b0100;
    const ADC: u32 = 0b0101;
    const SBC: u32 = 0b0110;
    const RSC: u32 = 0b0111;
    const TST: u32 = 0b1000;
    const TEQ: u32 = 0b1001;
    const CMP: u32 = 0b1010;
    const CMN: u32 = 0b1011;
    const ORR: u32 = 0b1100;
    const MOV: u32 = 0b1101;
    const BIC: u32 = 0b1110;
    const MVN: u32 = 0b1111;

    let rn_index = op.reg(16);
    let rd_index = op.reg(12);
    let setcc = op.flag(20);
    let opcode = op.field(21, 4);
    let imm = op.flag(25);

    let op1 = arm.regs[rn_index];
    let op2 = if imm {
        get_rotated_immediate(op)
    } else {
        get_shifted_register(arm, ic, op, setcc)
    };

    let result = match opcode {
        AND => op1 & op2,
        EOR => op1 ^ op2,
        SUB => op1.wrapping_sub(op2),
        RSB => op2.wrapping_sub(op1),
        ADD => op1.wrapping_add(op2),
        ADC => op1.wrapping_add(op2.wrapping_add(arm.cpsr.c as u32)),
        SBC => op1.wrapping_sub(op2.wrapping_add(!arm.cpsr.c as u32)),
        RSC => op2.wrapping_sub(op1.wrapping_add(!arm.cpsr.c as u32)),
        TST => op1 & op2,
        TEQ => op1 ^ op2,
        CMP => op1.wrapping_sub(op2),
        CMN => op1.wrapping_add(op2),
        ORR => op1 | op2,
        MOV => op2,
        BIC => op1 & !op2,
        MVN => !op2,
        _ => unreachable!()
    };

    if setcc {
        match opcode {
            AND | EOR | ORR | MOV | BIC | MVN | TST | TEQ => {
                set_zn(arm, result);
            }
            ADD | ADC | CMN => {
                set_zn(arm, result);
                add_set_vc(arm, op1, op2);
            }
            RSB | RSC => {
                set_zn(arm, result);
                sub_set_vc(arm, op2, op1);
            }
            SUB | SBC | CMP => {
                set_zn(arm, result);
                sub_set_vc(arm, op1, op2);
            }
            _ => unreachable!()
        }
    }

    match opcode {
        TST | TEQ | CMP | CMN => {}
        _ => {
            arm.set_reg(ic, rd_index, result);
        }
    }
}

fn single_data_load<T: load::Load>(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ArmOp) {
    let rd_index = op.reg(12);
    let rn_index = op.reg(16);
    let writeback = op.flag(21);
    let up = op.flag(23);
    let preindex = op.flag(24);
    let imm = op.flag(25);

    let rn = arm.regs[rn_index];

    let mut offset = if imm {
        op.field(0, 12)
    } else {
        get_shifted_register(arm, ic, op, false)
    };

    if !up {
        offset = ((offset) as i32).wrapping_neg() as u32;
    }

    let addr = if preindex { rn.wrapping_add(offset) } else { rn };

    check_watchpoint!(arm, addr);

    ic.add_internal_cycles(1);
    let value = T::LOAD(ic, addr);
    arm.set_reg(ic, rd_index, value);

    if writeback {
        // TODO: Handle PC?
        arm.regs[rn_index] += offset;
    }
}

fn single_data_store<T: store::Store>(arm: &mut Arm7TDMI, ic: &mut Interconnect, op: ArmOp) {
    let rd_index = op.reg(12);
    let rn_index = op.reg(16);
    let writeback = op.flag(21);
    let up = op.flag(23);
    let preindex = op.flag(24);
    let imm = op.flag(25);

    let rn = arm.regs[rn_index];
    let rd = arm.regs[rd_index];

    let mut offset = if imm {
        op.field(0, 12)
    } else {
        get_shifted_register(arm, ic, op, false)
    };

    if !up {
        offset = ((offset) as i32).wrapping_neg() as u32;
    }

    let addr = if preindex { rn.wrapping_add(offset) } else { rn };

    check_watchpoint!(arm, addr);

    ic.add_internal_cycles(1);
    let value = T::STORE(ic, addr, rd);

    if writeback {
        // TODO: Handle PC?
        arm.regs[rn_index] += offset;
    }
}

static ARM_DISPATCH_TABLE: &[(&str, &str, ArmEmuFn)] = &[
    ("0000 000S dddd nnnn ssss 1001 mmmm", "MUL*<S> %Rd, %Rm, %Rs", mul),
    ("0000 001S dddd nnnn ssss 1001 mmmm", "MLA*<S> %Rd, %Rm, %Rs, %Rn", mul),

    ("0000 100S hhhh llll ssss 1001 mmmm", "UMULL*<S> %Rl, %Rh, %Rm, %Rs", mul_long),
    ("0000 101S hhhh llll ssss 1001 mmmm", "UMLAL*<S> %Rl, %Rh, %Rm, %Rs", mul_long),
    ("0000 110S hhhh llll ssss 1001 mmmm", "SMULL*<S> %Rl, %Rh, %Rm, %Rs", mul_long),
    ("0000 111S hhhh llll ssss 1001 mmmm", "SMLAL*<S> %Rl, %Rh, %Rm, %Rs", mul_long),

    ("0001 0010 1111 1111 1111 0001 nnnn", "BX* %Rn", op_bx),
    ("0001 0s00 1111 dddd 0000 0000 0000", "MRS* %Rd, %Ps", op_mrs_reg),
    ("0001 0d10 1001 1111 0000 0000 mmmm", "MSR* %Pd, %Rm", op_msr_reg),
    ("0001 0d10 1000 1111 0000 0000 mmmm", "MSR* %Pd_flg, %Rm", op_msr_reg),
    ("0011 0d10 1000 1111 rrrr iiii iiii", "MSR* %Pd_flg, <rot_imm>", op_msr_flag_imm),
    ("0001 0000 nnnn dddd 0000 1001 mmmm", "SWP* %Rd, %Rm [%Rn]", op_swp),
    ("0001 0100 nnnn dddd 0000 1001 mmmm", "SWPB* %Rd, %Rm [%Rn]", op_swpb),
    ("011_ ____ ____ ____ ____ ___1 ____", "UNDEF*", op_und),

    ("1000 1SW1 nnnn rrrr rrrr rrrr rrrr", "LDMIA<S>* %Rn<wb>, { %+Rr }",
        |arm, ic, op| block_data_transfer(arm, ic, op, ldmia)
    ),
    ("1001 1SW1 nnnn rrrr rrrr rrrr rrrr", "LDMIB<S>* %Rn<wb>, { %+Rr }",
        |arm, ic, op| block_data_transfer(arm, ic, op, ldmib)
    ),
    ("1000 0SW1 nnnn rrrr rrrr rrrr rrrr", "LDMDA<S>* %Rn<wb>, { %+Rr }",
        |arm, ic, op| block_data_transfer(arm, ic, op, ldmda)
    ),
    ("1001 0SW1 nnnn rrrr rrrr rrrr rrrr", "LDMDB<S>* %Rn<wb>, { %+Rr }",
        |arm, ic, op| block_data_transfer(arm, ic, op, ldmdb)
    ),

    ("1000 1SW0 nnnn rrrr rrrr rrrr rrrr", "STMIA<S>* %Rn<wb>, { %+Rr }",
        |arm, ic, op| block_data_transfer(arm, ic, op, stmia)
    ),
    ("1001 1SW0 nnnn rrrr rrrr rrrr rrrr", "STMIB<S>* %Rn<wb>, { %+Rr }",
        |arm, ic, op| block_data_transfer(arm, ic, op, stmib)
    ),
    ("1000 0SW0 nnnn rrrr rrrr rrrr rrrr", "STMDA<S>* %Rn<wb>, { %+Rr }",
        |arm, ic, op| block_data_transfer(arm, ic, op, stmda)
    ),
    ("1001 0SW0 nnnn rrrr rrrr rrrr rrrr", "STMDB<S>* %Rn<wb>, { %+Rr }",
        |arm, ic, op| block_data_transfer(arm, ic, op, stmdb)
    ),

    ("1010 oooo oooo oooo oooo oooo oooo", "B* $offset[o]", branch),
    ("1011 oooo oooo oooo oooo oooo oooo", "BL* $offset[o]", branch),

    // Don't bother disassembling these properly, they shouldn't occur
    ("110P UNWL nnnn dddd #### oooo oooo", "CP_DATA_TRANS*", op_coprocessor),
    ("1110 cccc nnnn dddd #### ppp0 mmmm", "CP_REG_OP*", op_coprocessor),
    ("1110 cccL nnnn dddd #### ppp1 mmmm", "CP_DATA_OP*", op_coprocessor),

    ("1111 iiii iiii iiii iiii iiii iiii", "SWI* #[i]", op_swi),

    // At least one of the 'v's must be 0.
    ("00I 0000 S nnnn dddd iiii viiv iiii", "AND<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 0001 S nnnn dddd iiii viiv iiii", "EOR<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 0010 S nnnn dddd iiii viiv iiii", "SUB<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 0011 S nnnn dddd iiii viiv iiii", "RSB<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 0100 S nnnn dddd iiii viiv iiii", "ADD<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 0101 S nnnn dddd iiii viiv iiii", "ADC<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 0110 S nnnn dddd iiii viiv iiii", "SBC<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 0111 S nnnn dddd iiii viiv iiii", "RSC<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 1000 1 nnnn dddd iiii viiv iiii", "TST* %Rd, %Rn, <op2>", data_processing),
    ("00I 1001 1 nnnn dddd iiii viiv iiii", "TEQ* %Rd, %Rn, <op2>", data_processing),
    ("00I 1010 1 nnnn dddd iiii viiv iiii", "CMP* %Rd, %Rn, <op2>", data_processing),
    ("00I 1011 1 nnnn dddd iiii viiv iiii", "CMN* %Rd, %Rn, <op2>", data_processing),
    ("00I 1100 S nnnn dddd iiii viiv iiii", "ORR<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 1101 S nnnn dddd iiii viiv iiii", "MOV<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 1110 S nnnn dddd iiii viiv iiii", "BIC<S>* %Rd, %Rn, <op2>", data_processing),
    ("00I 1111 S nnnn dddd iiii viiv iiii", "MVN<S>* %Rd, %Rn, <op2>", data_processing),

    ("011P U0W1 nnnn dddd iiii iiii iiii", "LDR* %Rd, [%Rn, #offset[i]]", single_data_load::<u32>),
    ("010P U0W1 nnnn dddd iiii iiii iiii", "LDR* %Rd, [%Rn, <op2_reg>]", single_data_load::<u32>),
    ("011P U1W1 nnnn dddd iiii iiii iiii", "LDRB* %Rd, [%Rn, #offset[i]]", single_data_load::<u8>),
    ("010P U1W1 nnnn dddd iiii iiii iiii", "LDRB* %Rd, [%Rn, <op2_reg>]", single_data_load::<u8>),

    ("011P U0W0 nnnn dddd iiii iiii iiii", "STR* %Rd, [%Rn, #offset[i]]", single_data_store::<u32>),
    ("010P U0W0 nnnn dddd iiii iiii iiii", "STR* %Rd, [%Rn, <op2_reg>]", single_data_store::<u32>),
    ("011P U1W0 nnnn dddd iiii iiii iiii", "STRB* %Rd, [%Rn, #offset[i]]", single_data_store::<u8>),
    ("010P U1W0 nnnn dddd iiii iiii iiii", "STRB* %Rd, [%Rn, <op2_reg>]", single_data_store::<u8>),
];

//include!(concat!(env!("OUT_DIR"), "/arm_core_generated.rs"));
