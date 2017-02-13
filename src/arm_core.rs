use arm7tdmi::*;
use interconnect::Interconnect;

#[inline(always)]
fn set_zn(arm: &mut Arm7TDMI, value: u32) {
    arm.cpsr.z = value == 0;
    arm.cpsr.n = (value >> 31) != 0;
}

#[inline(always)]
fn add_set_vc(arm: &mut Arm7TDMI, a: u32, b: u32, r: u32) {
    arm.cpsr.v = ((a & b & !r) | (!a & !b & r)) >> 31 != 0;
    arm.cpsr.c = ((a & b) | (a & !r) | (b & !r)) >> 31 != 0;
}

#[inline(always)]
fn sub_set_vc(arm: &mut Arm7TDMI, a: u32, b: u32, r: u32) {
    arm.cpsr.v = ((a & !b & !r) | (!a & b & r)) >> 31 != 0;
    arm.cpsr.c = ((a & !b) | (a & !r) | (!b & !r)) >> 31 != 0;
}

/// Returns the operand generated from the barrel shifter, and the new value
/// for the carry flag.
// TODO: Handle out-of-range(0...31) shift amounts given by register specified
//       shift operations
fn barrel_shift(arm: &mut Arm7TDMI, operand: u32) -> (u32, bool) {
    let rm = arm.regs[(operand & 0xF) as usize];
    let shift_field = operand >> 4 & 0xFF;

    let shift_op = shift_field >> 1 & 3;

    let mut shift_amount = if shift_field & 1 == 0 { // Operand = register shifted by immediate
        (shift_field >> 3 & 0b11111)
    } else { // Operand = register shifted by register
        let rs = (shift_field >> 4) as usize;
        assert!(rs != REG_PC);
        arm.regs[rs]
    };

    match shift_op {
        0b00 => { // LSL
            if shift_amount == 32 {
                let shifted = 0u32;
                let carry = (rm & 1) != 0;
                (shifted, carry)
            } else if shift_amount > 32 {
                (0u32, false)
            } else {
                let shifted = rm << shift_amount;
                if shift_amount == 0 {
                    // TODO Fewer cycles
                    (shifted, arm.cpsr.c)
                } else {
                    let carry = ((rm >> (32 - shift_amount)) & 1) != 0;
                    (shifted, carry)
                }
            }
        }
        0b01 => { // LSR
            if shift_amount == 0 {
                shift_amount = 32;
            }

            if shift_amount == 32 {
                let shifted = 0u32;
                let carry = (rm >> 31) != 0;
                (shifted, carry)
            } else if shift_amount > 32 {
                (0u32, false)
            } else {
                let shifted = rm >> shift_amount;
                let carry = ((rm >> (shift_amount - 1)) & 1) != 0;
                (shifted, carry)
            }
        }
        0b10 => { // ASR
            if shift_amount == 0 {
                shift_amount = 32;
            }

            if shift_amount >= 32 {
                // Return sign extension of rm
                if (rm >> 31) != 0 {
                    (0xFFFF_FFFFu32, true)
                } else {
                    (0u32, false)
                }
            } else {
                let shifted = ((rm as i32) >> shift_amount) as u32;
                let carry = (((rm as i32) >> (shift_amount - 1)) & 1) != 0;
                (shifted, carry)
            }
        }
        0b11 => { // ROR
            if shift_amount == 0 { // RRX
                let carry_in = arm.cpsr.c as u32;
                let shifted = (rm >> 1) | (carry_in << 31);
                let carry = (rm & 1) != 0;
                (shifted, carry)
            } else {
                let shifted = rm.rotate_right(shift_amount);
                let carry = ((rm >> ((shift_amount - 1) & 0x1F)) & 1) != 0;
                (shifted, carry)
            }
        }
        _ => unreachable!(),
    }
}

pub fn step_arm(arm: &mut Arm7TDMI, interconnect: &mut Interconnect, op: u32) -> StepEvent {
    debug_assert!(arm.regs[REG_PC] & 3 == 0);

    macro_rules! invalid {
        ($op:expr) => {
            panic!("Unsupported operation 0x{:08X}", op);
        }
    }

    let mut event = StepEvent::None;

    let cond = ConditionCode::from(op >> 28);
    if !arm.eval_condition_code(cond) {
        return event;
    }

    let mask = (op >> 4 & 0xF) | (op >> 16 & 0xFF0);

    match mask {
        // 00xx_xxxx_axxb given a!=1 || b!=1
        0x000 ... 0x3FF if (mask & 0x200) != 0 || (mask & 0b1001) != 0b1001 => { // Data processing
            let rd_index = (op >> 12 & 0xF) as usize;
            let rn = arm.regs[(op >> 16 & 0xF) as usize];
            let set_cc = (op & 0x0010_0000) != 0;
            let opcode = (op >> 21 & 0xF) as usize;


            // Only one of the flag set without set_cc modes uses operand2
            let calculate_operand2 = (opcode & 0b1100) != 0b1000 || set_cc || (op >> 16 & 0x3F) == 0b101000;
            let (operand2, barrel_shifter_carry) = if calculate_operand2 {
                if (op & 0x0200_0000) != 0 { // Operand = immediate rotated right by immediate
                    let imm = (op & 0xFF) as u32;
                    let rotate = (op >> 8 & 0xF) as u32;
                    let shifted = imm.rotate_right(rotate * 2);
                    let carry = arm.cpsr.c;
                    (shifted, carry)
                } else {
                    barrel_shift(arm, op)
                }
            } else {
                (0u32, false) // Dummy value, unused.
            };

            let result = match opcode {
                0b0000 => { // AND
                    rn & operand2
                }
                0b0001 => { // EOR
                    rn ^ operand2
                }
                0b0010 => { // SUB
                    rn.wrapping_sub(operand2)
                }
                0b0011 => { // RSB
                    operand2.wrapping_sub(rn)
                }
                0b0100 => { // ADD
                    rn.wrapping_add(operand2)
                }
                0b0101 => { // ADC
                    rn.wrapping_add(operand2.wrapping_add(arm.cpsr.c as u32))
                }
                0b0110 => { // SBC
                    rn.wrapping_sub(operand2.wrapping_add(!arm.cpsr.c as u32))
                }
                0b0111 => { // RSC
                    operand2.wrapping_sub(rn.wrapping_add(!arm.cpsr.c as u32))
                }
                0b1000 if set_cc => { // TST
                    let result = rn & operand2;
                    set_zn(arm, result);
                    arm.cpsr.c = barrel_shifter_carry;
                    return event;
                }
                0b1001 if set_cc => { // TEQ
                    let result = rn ^ operand2;
                    set_zn(arm, result);
                    arm.cpsr.c = barrel_shifter_carry;
                    return event;
                }
                0b1010 if set_cc => { // CMP
                    let result = rn.wrapping_sub(operand2);
                    set_zn(arm, result);
                    sub_set_vc(arm, rn, operand2, result);
                    return event;
                }
                0b1011 if set_cc => { // CMN
                    let result = rn.wrapping_add(operand2);
                    set_zn(arm, result);
                    add_set_vc(arm, rn, operand2, result);
                    return event;
                }
                0b1100 => { // ORR
                    rn | operand2
                }
                0b1101 => { // MOV
                    operand2
                }
                0b1110 => { // BIC
                    rn & !operand2
                }
                0b1111 => { // MVN
                    !operand2
                }

                0b1000 ... 0b1011 if !set_cc => {
                    let use_cpsr = (op & 0x0040_0000) == 0;

                    match op >> 16 & 0x3F {
                        0b001111 => { // MRS Rd, PSR
                            if use_cpsr {
                                arm.regs[rd_index] = arm.cpsr.into();
                            } else {
                                // panic if we try this in a mode without spsr
                                arm.regs[rd_index] = arm.get_spsr().into();
                            }
                        }
                        0b101001 => { // MSR PSR, operand2
                            let rm = (op & 0xF) as usize;
                            let new_psr = StatusRegister::from(arm.regs[rm]);

                            assert!(new_psr.thumb_mode == arm.cpsr.thumb_mode);
                            if !arm.cpsr.mode.is_privileged() {
                                // User mode may only change flags
                                assert!(new_psr.mode == arm.cpsr.mode);
                                assert!(new_psr.irq_disable == arm.cpsr.irq_disable);
                                assert!(new_psr.fiq_disable == arm.cpsr.fiq_disable);
                            }

                            if use_cpsr {
                                arm.switch_mode(new_psr.mode);
                                arm.cpsr = new_psr;
                            } else {
                                arm.set_spsr(new_psr);
                            }
                        }
                        0b101000 => { // MSR PSR_flag, operand2
                            if use_cpsr {
                                arm.cpsr.set_flags(operand2);
                            } else {
                                if arm.has_spsr() {
                                    arm.get_spsr_mut().set_flags(operand2);
                                }
                            }
                        }
                        0b101111 => { // BX
                            let rn = arm.regs[(op & 0xF) as usize];
                            arm.branch_exchange(interconnect, rn);
                        }

                        _ => invalid!(op),
                    }

                    return event;
                }

                _ => unreachable!()
            };

            arm.regs[rd_index] = result;
            if set_cc {
                match opcode {
                    0b0000 | // AND
                    0b0001 | // EOR
                    0b1100 | // ORR
                    0b1101 | // MOV
                    0b1110 | // BIC
                    0b1111 => { // MVN
                        set_zn(arm, result);
                        arm.cpsr.c = barrel_shifter_carry;
                    }

                    0b0100 => { // ADD
                        set_zn(arm, result);
                        add_set_vc(arm, rn, operand2, result);
                    }

                    0b0011 => { // RSB
                        set_zn(arm, result);
                        sub_set_vc(arm, operand2, rn, result);
                    }

                    0b0010 => { // SUB
                        set_zn(arm, result);
                        sub_set_vc(arm, rn, operand2, result);
                    }

                    0b0101 => { // ADC
                        set_zn(arm, result);
                        add_set_vc(arm, rn, operand2, result);
                    }
                    0b0110 => { // SBC
                        set_zn(arm, result);
                        sub_set_vc(arm, rn, operand2, result);
                    }
                    0b0111 => { // RSC
                        set_zn(arm, result);
                        sub_set_vc(arm, operand2, rn, result);
                    }

                    0b1010 | // CMP
                    0b1011 | // CMN
                    0b1000 | // TST
                    0b1001 => { // TEQ
                        unreachable!();
                    }

                    _ => unreachable!(),
                }
            }

            if rd_index == REG_PC {
                if set_cc {
                    // panic if mode has no spsr
                    unreachable!(); // TODO: Check this
                    // arm.cpsr = arm.get_spsr();
                } else {
                    let target = arm.regs[REG_PC];
                    arm.branch_to(interconnect, target);
                }
            }
        }

        // 0b0000_00xx_1001
        0x009 | 0x019 | 0x029 | 0x039 => { // Multiply
            let accumulate = (op & 0x00200000) != 0;
            let set_cc = (op & 0x00100000) != 0;
            let rd_index = (op >> 16 & 0xF) as usize;
            let rn_index = (op >> 12 & 0xF) as usize;
            let rs_index = (op >> 8 & 0xF) as usize;
            let rm_index = (op & 0xF) as usize;

            assert!(rd_index != rm_index);
            assert!(rd_index != REG_PC);
            assert!(rs_index != REG_PC);
            assert!(rn_index != REG_PC);
            assert!(rm_index != REG_PC);

            let rn = arm.regs[rn_index];
            let rs = arm.regs[rs_index];
            let rm = arm.regs[rm_index];

            let result = if accumulate {
                (rm * rs).wrapping_add(rn)
            } else {
                rm.wrapping_mul(rs)
            };

            arm.regs[rd_index] = result;
            if set_cc {
                arm.cpsr.z = result == 0;
                arm.cpsr.n = (result >> 31) != 0;
                arm.cpsr.c = false; // Meaningless value
            }
        }

        // 0b0000_1xxx_1001
        0x089 | 0x099 | 0x0A9 | 0x0B9 | 0x0C9 | 0x0D9 | 0x0E9 | 0x0F9 => { // Multiply
            let unsigned = (op & 0x00400000) != 0;
            let accumulate = (op & 0x00200000) != 0;
            let set_cc = (op & 0x00100000) != 0;
            let rd_hi_index = (op >> 16 & 0xF) as usize;
            let rd_lo_index = (op >> 12 & 0xF) as usize;
            let rs_index = (op >> 8 & 0xF) as usize;
            let rm_index = (op & 0xF) as usize;

            assert!(rd_hi_index != REG_PC);
            assert!(rd_lo_index != REG_PC);
            assert!(rs_index != REG_PC);
            assert!(rm_index != REG_PC);
            assert!(rd_hi_index != rd_lo_index);
            assert!(rd_hi_index != rm_index);
            assert!(rd_lo_index != rm_index);

            let result: u64 = if unsigned {
                let a = arm.regs[rm_index] as u64;
                let b = arm.regs[rs_index] as u64;

                let result: u64 = if accumulate {
                    let low = arm.regs[rd_lo_index] as u64;
                    let high = arm.regs[rd_hi_index] as u64;
                    let c = low | (high << 32);
                    a.wrapping_mul(b).wrapping_add(c)
                } else {
                    a.wrapping_mul(b)
                };

                result
            } else {
                let a = arm.regs[rm_index] as i32 as i64;
                let b = arm.regs[rs_index] as i32 as i64;

                let result: i64 = if accumulate {
                    let low = arm.regs[rd_lo_index] as i32 as i64;
                    let high = arm.regs[rd_hi_index] as i32 as i64;
                    let c = low | (high << 32);
                    a * b + c
                } else {
                    a * b
                };

                result as u64
            };

            if set_cc {
                arm.cpsr.z = result == 0;
                arm.cpsr.n = (result >> 63) != 0;
                arm.cpsr.c = false; // Meaningless value
                arm.cpsr.v = false; // Meaningless value
            }

            arm.regs[rd_lo_index] = result as u32;
            arm.regs[rd_hi_index] = (result >> 32) as u32;
        }

        // 0b01xx_xxxx_axxb given a!=1 || b!=1
        0x400 ... 0x7FF if (mask & 0x200) == 0 || (mask & 0b1111) != 0b1001 => { // Single Data Transfer
            let rn_index = (op >> 16 & 0xF) as usize;
            let rd_index = (op >> 12 & 0xF) as usize;
            let rn = arm.regs[rn_index];

            let load = (op & 0x0010_0000) != 0;
            let writeback = (op & 0x0020_0000) != 0;
            let byte = (op & 0x0040_0000) != 0;
            let up = (op & 0x0080_0000) != 0;
            let preindex = (op & 0x0100_0000) != 0;
            let imm = (op & 0x0200_0000) == 0;

            let mut offset = if imm {
                op & 0xFFF
            } else {
                barrel_shift(arm, op).0
            };

            if !up {
                // Make negative
                offset = -(offset as i32) as u32
            }

            let addr = if preindex {
                rn.wrapping_add(offset)
            } else {
                rn
            };

            if arm.watchpoints.contains(addr) {
                event = StepEvent::TriggerWatchpoint(addr);
            }

            match (load, byte) {
                (false, false) => {
                    // STR
                    interconnect.write32(addr, arm.regs[rd_index]);
                }
                (false, true ) => {
                    // STRB
                    interconnect.write8(addr, arm.regs[rd_index] as u8);
                }
                (true , false) => {
                    // LDR
                    arm.regs[rd_index] = interconnect.read32(addr);
                }
                (true , true ) => {
                    // LDRB
                    arm.regs[rd_index] = interconnect.read8(addr) as u32;
                }
            }

            if preindex {
                if writeback {
                    arm.regs[rn_index] = addr;
                }
            } else {
                assert!(!writeback);
                arm.regs[rn_index] += offset;
            }
        },

        // // Halfword and Signed Data Transfer
        // // When S=1, L must be 1
        // // Rm must not be R15(PC)
        0x000 ... 0x1FF if (mask & 0b1001) == 0b1001 && (mask & 0b0110) != 0 => {
            let load = (op & 0x0010_0000) != 0;
            let writeback = (op & 0x0020_0000) != 0;
            let use_imm = (op & 0x0040_0000) != 0;
            let up = (op & 0x0080_0000) != 0;
            let preindex = (op & 0x0100_0000) != 0;
            let rd_index = (op >> 12 & 0xF) as usize;
            let rn_index = (op >> 16 & 0xF) as usize;
            let rn = arm.regs[rn_index];

            if writeback {
                assert!(rn_index != REG_PC);
            }

            let mut offset = if use_imm {
                (op & 0xF) | (op >> 4 & 0xF0)
            } else {
                let rm_index = (op & 0xF) as usize;
                assert!(rm_index != REG_PC);
                arm.regs[rm_index]
            };

            if up {
                // Make negative
                offset = !offset + 1;
            }

            let addr = if preindex {
                rn.wrapping_add(offset)
            } else {
                rn
            };

            if arm.watchpoints.contains(addr) {
                event = StepEvent::TriggerWatchpoint(addr);
            }

            match (load, op >> 5 & 3) {
                (true, 0b01) => {
                    // LDH
                    arm.regs[rd_index] = interconnect.read16(addr) as u32;
                }
                (true, 0b10) => {
                    // LDSB
                    arm.regs[rd_index] = interconnect.read8(addr) as i8 as i32 as u32;
                }
                (true, 0b11) => {
                    // LDSH
                    arm.regs[rd_index] = interconnect.read16(addr) as i16 as i32 as u32;
                }
                (false, 0b01) => {
                    // STH
                    interconnect.write16(addr, arm.regs[rd_index] as u16);
                }

                // Signed stores aren't valid operations
                (false, 0b10) | (false, 0b11) => unreachable!(),

                (_, _) => unreachable!(),
            }

            if preindex {
                if writeback {
                    arm.regs[rn_index] = addr;
                }
            } else {
                // Writeback is implicit with post-indexing, so this is invalid
                assert!(!writeback);
                arm.regs[rn_index] = rn.wrapping_add(offset);
            }
        }

        0x109 | 0x149 => { // Single Data Swap
            let byte = (op & 0x0040_0000) != 0;
            let rm_index = (op & 0xF) as usize;
            let rd_index = (op >> 12 & 0xF) as usize;
            let rn_index = (op >> 16 & 0xF) as usize;

            assert!(rm_index != REG_PC);
            assert!(rd_index != REG_PC);
            assert!(rn_index != REG_PC);

            let addr = arm.regs[rn_index];
            let rm = arm.regs[rm_index];

            if arm.watchpoints.contains(addr) {
                event = StepEvent::TriggerWatchpoint(addr);
            }

            if byte {
                let old = interconnect.read8(addr) as u32;
                arm.regs[rd_index] = old;
                interconnect.write8(addr, rm as u8);
            } else {
                let old = interconnect.read32(addr);
                arm.regs[rd_index] = old;
                interconnect.write32(addr, rm);
            }
        }

        // 0x601 ... 0x7FF if (mask & 1) == 1 => {
        //     format!("UNDEF {:08X}", op)
        // }

        0x800 ... 0x9FF => { // Block Data Transfer
            let rn_index = (op >> 16 & 0xF) as usize;
            let load = (op & 0x0010_0000) != 0;
            let writeback = (op & 0x0020_0000) != 0;
            let set_cc = (op & 0x0040_0000) != 0; // load PSR or force user mode
            let up = (op & 0x0080_0000) != 0;
            let preindex = (op & 0x0100_0000) != 0;
            let reglist = op & 0xFFFF;
            let pc_in_reglist = reglist & (1 << REG_PC) != 0;

            // Register list cannot be empty
            assert!(reglist != 0);
            assert!(rn_index != REG_PC);

            const WIDTH: u32 = 4;
            let rn = arm.regs[rn_index];
            let span = WIDTH * reglist.count_ones();

            let (mut ptr, writeback_value) = match (up, preindex) {
                (true,  true ) => (rn + WIDTH, rn + span),
                (true,  false) => (rn, rn + span),
                (false, true ) => (rn - span, rn - span),
                (false, false) => (rn - span + WIDTH,rn - span),
            };

            if preindex {
                ptr = ptr.wrapping_add(WIDTH);
            }

            if set_cc {
                assert!(arm.cpsr.mode.is_privileged());

                // If in STM or in LDM without PC in reglist then do a user
                // bank transfer operation. Helpful for process context switches.
                if !pc_in_reglist || !load {
                    assert!(writeback);
                    let from_mode = arm.cpsr.mode;
                    arm.bank_swap(from_mode);
                    arm.bank_swap(OperatingMode::User);
                }
            }

            if load {
                // If there is writeback and rn is in reglist, writeback always
                // overwrites the value loaded by LDM, so we don't need any
                // special handling here, as we do writeback last.
                for i in 0..arm.regs.len()-1 {
                    if reglist & (1 << i as u32) != 0 {
                        arm.regs[i] = interconnect.read32(ptr);
                        if arm.watchpoints.contains(ptr) {
                            event = StepEvent::TriggerWatchpoint(ptr);
                        }
                        ptr = ptr.wrapping_add(WIDTH);
                    }
                }

                if reglist & (1 << REG_PC) != 0 {
                    let target = interconnect.read32(ptr);
                    if arm.watchpoints.contains(ptr) {
                        event = StepEvent::TriggerWatchpoint(ptr);
                    }
                    arm.branch_to(interconnect, target);

                    if set_cc {
                        arm.cpsr = arm.get_spsr();
                    }
                }
            } else {
                {
                    let rn_mask = 1 << rn_index;
                    let rn_is_in_reglist = reglist & rn_mask != 0;
                    let rn_is_first_in_reglist = reglist & (rn_mask - 1) == 0;

                    if rn_is_in_reglist && writeback {
                        if !rn_is_first_in_reglist {
                            // TODO: Work out what this means:
                            /*
                                "A STM which includes storing the base [...]
                                with the base second or later in the transfer
                                order, will store the modified value."
                            */
                            // Store base+4 or base+4*i or ..?
                            unimplemented!();
                        }
                    }
                }

                for i in 0..arm.regs.len()-1 {
                    if reglist & (1 << i as u32) != 0 {
                        interconnect.write32(ptr, arm.regs[i]);
                        if arm.watchpoints.contains(ptr) {
                            event = StepEvent::TriggerWatchpoint(ptr);
                        }
                        ptr = ptr.wrapping_add(WIDTH);
                    }
                }

                if reglist & (1 << REG_PC) != 0 {
                    // "Whenever R15 is stored to memory the stored value is
                    //  the address of the STM instruction plus 12."
                    interconnect.write32(ptr, arm.regs[REG_PC] + 4);
                    if arm.watchpoints.contains(ptr) {
                        event = StepEvent::TriggerWatchpoint(ptr);
                    }
                }
            }

            // Restore register bank mode from user mode
            if set_cc && (!pc_in_reglist || !load) {
                let to_mode = arm.cpsr.mode;
                arm.bank_swap(OperatingMode::User);
                arm.bank_swap(to_mode);
            }

            if writeback {
                arm.regs[rn_index] = writeback_value;
            }
        }

        0xA00 ... 0xBFF => { // Branch / Branch with Link
            let link = (op & 0x0100_0000) != 0;

            let offset = if op & 0x80_0000 != 0 {
                ((op & 0xFF_FFFF) | 0xFF00_0000) << 2
            } else {
                (op & 0xFF_FFFF) << 2
            };

            let pc = arm.regs[REG_PC];
            let addr = pc.wrapping_add(offset);

            if link {
                // PC is 8 bytes ahead of this instruction, we want LR to
                // point to the instruction after this current one.
                arm.regs[REG_LR] = pc - 4;
            }

            arm.branch_to(interconnect, addr);
        }

        // // Coprocessor operations
        // 0xC00 ... 0xEFF => {
        //     panic!("Coprocessor operations unsupported");
        // }

        0xF00 ... 0xFFF => { // Software Interrupt
            // GBA only uses most significant 8 bits of comment field, matching
            // the thumb version of the instruction.
            arm.signal_swi(interconnect);
        }

        _ => invalid!(op),
    }

    event
}
