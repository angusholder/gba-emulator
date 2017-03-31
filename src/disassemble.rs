use std::fmt::Write;
use arm7tdmi::{ REG_PC, REG_LR };

static CC_NAMES: [&'static str; 15] = [
    "EQ", // 0000 Z                 equal
    "NE", // 0001 !Z                not equal
    "CS", // 0010 C                 unsigned higher or same
    "CC", // 0011 !C                unsigned lower
    "MI", // 0100 N                 negative
    "PL", // 0101 !N                positive or zero
    "VS", // 0110 V                 overflow
    "VC", // 0111 !V                no overflow
    "HI", // 1000 C && !Z           unsigned higher
    "LS", // 1001 !C || Z           unsigned lower or same
    "GE", // 1010 N == V            greater or equal
    "LT", // 1011 N != V            less than
    "GT", // 1100 !Z && (N == V)    greater than
    "LE", // 1101 Z || (N != V)     less than or equal
    "",   // 1110 (ignored)         always
];

static ARM_REGS: [&'static str; 16] = [
    "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7",
    "R8", "R9", "R10", "R11", "R12", "SP", "LR", "PC"
];

fn build_register_list(registers: u16) -> String {
    let registers = registers as usize;
    let mut result = String::new();
    let mut n = 0;

    for i in 0usize..ARM_REGS.len() {
        if (registers & 1 << i) != 0 {
            write!(result, "{}, ", ARM_REGS[i]).unwrap();
            n += 1;
        }
    }

    if n > 0 {
        // Strip final ", "
        let newlen = result.len() - 2;
        result.truncate(newlen);
    }

    result
}

fn sign_extend(n: u32, n_bits: usize) -> u32 {
    let shift = 32 - n_bits;
    (((n << shift) as i32) >> shift) as u32
}

pub fn disassemble_arm_opcode(op: u32, pc: u32) -> String {
    let cond = (op >> 28) as usize;
    assert!(cond != 0b1111);
    let cond = CC_NAMES[cond];

    let mask = (op >> 4 & 0xF) | (op >> 16 & 0xFF0);

    fn invalid(op: u32) -> String {
        format!("???{} 0x{:08X}", CC_NAMES[(op >> 28 & 0xF) as usize], op)
    }

    match mask {
        // 00xx_xxxx_axxb given a!=1 || b!=1
        0x000 ... 0x3FF if (mask & 0x200) != 0 || (mask & 0b1001) != 0b1001 => { // Data processing
            static DATA_OP_NAMES: [&'static str; 16] = [
                "AND", // 0000 - Rd := Op1 AND Op2
                "EOR", // 0001 - Rd := Op1 EOR Op2
                "SUB", // 0010 - Rd := Op1 - Op2
                "RSB", // 0011 - Rd := Op2 - Op1
                "ADD", // 0100 - Rd := Op1 + Op2
                "ADC", // 0101 - Rd := Op1 + Op2 + C
                "SBC", // 0110 - Rd := Op1 - Op2 + C
                "RSC", // 0111 - Rd := Op2 - Op1 + C
                "TST", // 1000 - set condition codes on Op1 AND Op2
                "TEQ", // 1001 - set condition codes on Op1 EOR Op2
                "CMP", // 1010 - set condition codes on Op1 - Op2
                "CMN", // 1011 - set condition codes on Op1 + Op2
                "ORR", // 1100 - Rd := Op1 OR Op2
                "MOV", // 1101 - Rd := Op2
                "BIC", // 1110 - Rd := Op1 AND NOT Op2
                "MVN", // 1111 - Rd := NOT Op2
            ];

            let rd = (op >> 12 & 0xF) as usize;
            let rn = (op >> 16 & 0xF) as usize;
            let set_cc = (op & 0x0010_0000) != 0;
            let opcode = (op >> 21 & 0xF) as usize;

            let operand2 = if (op & 0x0200_0000) != 0 { // Operand = immediate rotated right by immediate
                let imm = (op & 0xFF) as u32;
                let rotate = (op >> 8 & 0xF) as u32;
                let value = imm.rotate_right(rotate * 2);
                format!("#{} ; 0x{:X}", value, value)
            } else {
                let rm = (op & 0xF) as usize;
                let shift = op >> 4 & 0xFF;
                let shift_type: &'static str = match shift >> 1 & 3 {
                    0b00 => "LSL",
                    0b01 => "LSR",
                    0b10 => "ASR",
                    0b11 => "ROR",
                    _ => unreachable!(),
                };

                if shift & 1 == 0 { // Operand = register shifted by immediate
                    let shift_amount = shift >> 3 & 0b11111;
                    if shift_amount == 0 {
                        format!("{}", ARM_REGS[rm])
                    } else {
                        format!("{} {} #{}", ARM_REGS[rm], shift_type, shift_amount)
                    }
                } else { // Operand = register shifted by register
                    let reg = (shift >> 4) as usize;
                    format!("{} {} {}", ARM_REGS[rm], shift_type, ARM_REGS[reg])
                }
            };

            match opcode {
                // Unary: MVN/MOV
                0b1111 | 0b1101 => format!("{name}{set_cc}{cond} {rd}, {op2}",
                    name = DATA_OP_NAMES[opcode],
                    cond = cond,
                    set_cc = if set_cc {"S"} else {""},
                    rd = ARM_REGS[rd],
                    op2 = operand2
                ),

                // No destination, set_cc mandatory: TST/TEQ/CMP/CMN
                // TODO: (set_cc=0 encodes MRS/MSR instructions for CPSR/SPSR))
                0b1000 ... 0b1011 => {
                    if set_cc {
                        format!("{name}{cond} {rn}, {op2}",
                            name = DATA_OP_NAMES[opcode],
                            cond = cond,
                            rn = ARM_REGS[rn],
                            op2 = operand2
                        )
                    } else {
                        let psr = if op & 0x0040_0000 == 0 {"CPSR"} else {"SPSR"};
                        match op >> 16 & 0x3F {
                            0b001111 => format!("MRS{cond} {rd}, {psr}",
                                cond = cond,
                                rd = ARM_REGS[rd],
                                psr = psr
                            ),
                            0b101001 => format!("MSR{cond} {psr}, {rm}",
                                cond = cond,
                                rm = ARM_REGS[(op & 0xF) as usize],
                                psr = psr
                            ),
                            0b101000 => format!("MSR{cond} {psr}_flg, {imm}",
                                cond = cond,
                                imm = operand2,
                                psr = psr
                            ),
                            0b101111 => format!("BX{cond} {rn}",
                                cond = cond,
                                rn = ARM_REGS[(op & 0xF) as usize]
                            ),
                            _ => invalid(op),
                        }
                    }
                }

                // All others
                _ => format!("{name}{set_cc}{cond} {rd}, {rn}, {op2}",
                    name = DATA_OP_NAMES[opcode],
                    cond = cond,
                    set_cc = if set_cc {"S"} else {""},
                    rd = ARM_REGS[rd], rn = ARM_REGS[rn],
                    op2 = operand2
                ),
            }
        }

        // 0b0000_00xx_1001
        0x009 | 0x019 | 0x029 | 0x039 => { // Multiply
            let accumulate = (op & 0x00200000) != 0;
            let set_cc = (op & 0x00100000) != 0;
            let rd = ARM_REGS[(op >> 16 & 0xF) as usize];
            let rn = ARM_REGS[(op >> 12 & 0xF) as usize];
            let rs = ARM_REGS[(op >> 8 & 0xF) as usize];
            let rm = ARM_REGS[(op & 0xF) as usize];

            if accumulate {
                format!("MLA{cond}{set_cc} {rd}, {rm}, {rs}, {rn}",
                    cond = cond,
                    set_cc = if set_cc {"S"} else {""},
                    rd = rd, rm = rm, rs = rs, rn = rn
                )
            } else {
                format!("MUL{cond}{set_cc} {rd}, {rm}, {rs}",
                    cond = cond,
                    set_cc = if set_cc {"S"} else {""},
                    rd = rd, rm = rm, rs = rs,
                )
            }
        }

        // 0b0000_1xxx_1001
        0x089 | 0x099 | 0x0A9 | 0x0B9 | 0x0C9 | 0x0D9 | 0x0E9 | 0x0F9 => { // Multiply
            let unsigned = (op & 0x00400000) != 0;
            let accumulate = (op & 0x00200000) != 0;
            let set_cc = (op & 0x00100000) != 0;
            let rd_hi = ARM_REGS[(op >> 16 & 0xF) as usize];
            let rd_lo = ARM_REGS[(op >> 12 & 0xF) as usize];
            let rs = ARM_REGS[(op >> 8 & 0xF) as usize];
            let rm = ARM_REGS[(op & 0xF) as usize];

            format!("{sign}{op}{cond}{set_cc} {rd_lo}, {rd_hi}, {rm}, {rs}",
                rd_lo = rd_lo, rd_hi = rd_hi, rm = rm, rs = rs,
                op = if accumulate {"MLAL"} else {"MULL"},
                sign = if unsigned {"U"} else {"S"},
                set_cc = if set_cc {"S"} else {""},
                cond = cond
            )
        }

        // 0b01xx_xxxx_axxb given a!=1 || b!=1
        0x400 ... 0x7FF if (mask & 0x200) == 0 || (mask & 0b1111) != 0b1001 => { // Single Data Transfer
            let rn = ARM_REGS[(op >> 16 & 0xF) as usize];
            let rd = ARM_REGS[(op >> 12 & 0xF) as usize];

            let load = (op & 0x0010_0000) != 0;
            let writeback = (op & 0x0020_0000) != 0;
            let byte = (op & 0x0040_0000) != 0;
            let up = if (op & 0x0080_0000) != 0 {""} else {"-"};
            let preindex = (op & 0x0100_0000) != 0;
            let imm_offset = (op & 0x0200_0000) == 0;

            let offset = if imm_offset {
                let offset = op & 0xFFF;
                format!("#{}{}", up, offset)
            } else {
                let rm = (op & 0xF) as usize;
                let shift = op >> 4 & 0xFF;
                let shift_type: &'static str = match shift >> 1 & 3 {
                    0b00 => "LSL",
                    0b01 => "LSR",
                    0b10 => "ASR",
                    0b11 => "ROR",
                    _ => unreachable!(),
                };

                if shift & 1 == 1 {
                    // Register specified shift amounts are not available
                    // in this instruction class.
                    return invalid(op);
                }

                let shift_amount = shift >> 3 & 0b11111;
                if shift_amount == 0 {
                    format!("{}{}", up, ARM_REGS[rm])
                } else {
                    format!("{}{} {} #{}", up, ARM_REGS[rm], shift_type, shift_amount)
                }
            };

            if preindex {
                format!("{name}{byte}{cond} {rd}, [{rn}, {offset}]{writeback}",
                    writeback = if writeback {"!"} else {""},
                    name = if load {"LDR"} else {"STR"},
                    byte = if byte {"B"} else {""},
                    rd = rd, rn = rn,
                    offset = offset,
                    cond = cond
                )
            } else {
                format!("{name}{byte}{cond} {rd}, [{rn}], {offset}",
                    byte = if byte {"B"} else {""},
                    name = if load {"LDR"} else {"STR"},
                    rn = rn, rd = rd,
                    offset = offset,
                    cond = cond
                )
            }
        },

        // Halfword and Signed Data Transfer
        // When S=1, L must be 1
        // Rm must not be R15(PC)
        0x000 ... 0x1FF if (mask & 0b1001) == 0b1001 && (mask & 0b0110) != 0 => {
            let load = (op & 0x0010_0000) != 0;
            let writeback = (op & 0x0020_0000) != 0;
            let use_imm = (op & 0x0040_0000) != 0;
            let up = if (op & 0x0080_0000) != 0 {""} else {"-"};
            let pre = (op & 0x0100_0000) != 0;
            let rd = (op >> 12 & 0xF) as usize;
            let rn = (op >> 16 & 0xF) as usize;

            let name = match (load, op >> 5 & 3) {
                (true,  0b01) => "LDRH",
                (true,  0b10) => "LDSB",
                (true,  0b11) => "LDSH",
                (false, 0b01) => "STRH",
                _ => return invalid(op),
            };

            let offset = if use_imm {
                let imm = (op & 0xF) | (op >> 4 & 0xF0);
                format!("#{up}{imm}", up = up, imm = imm)
            } else {
                let rm = (op & 0xF) as usize;
                format!("{up}{rm}", up = up, rm = ARM_REGS[rm])
            };

            if pre {
                format!("{name}{cond} {rd}, [{rn}, {offset}]{writeback}",
                    writeback = if writeback {"!"} else {""},
                    name = name, cond = cond,
                    rd = ARM_REGS[rd],
                    rn = ARM_REGS[rn],
                    offset = offset
                )
            } else {
                format!("{name}{cond} {rd}, [{rn}], {offset}",
                    name = name, cond = cond,
                    rd = ARM_REGS[rd],
                    rn = ARM_REGS[rn],
                    offset = offset
                )
            }
        }

        0x109 | 0x149 => { // Single Data Swap
            let byte = (op & 0x0040_0000) != 0;
            let rm = (op & 0xF) as usize;
            let rd = (op >> 12 & 0xF) as usize;
            let rn = (op >> 16 & 0xF) as usize;

            format!("SWP{cond}{byte} {rd}, {rm}, [{rn}]",
                rm = ARM_REGS[rm], rd = ARM_REGS[rd], rn = ARM_REGS[rn],
                cond = cond,
                byte = if byte {"B"} else {""}
            )
        }

        0x601 ... 0x7FF if (mask & 1) == 1 => {
            format!("UNDEF {:08X}", op)
        }

        0x800 ... 0x9FF => { // Block Data Transfer
            let rn = (op >> 16 & 0xF) as usize;
            let load = (op & 0x0010_0000) != 0;
            let writeback = (op & 0x0020_0000) != 0;
            let set_cc = (op & 0x0040_0000) != 0; // load PSR or force user mode
            let up = (op & 0x0080_0000) != 0;
            let pre = (op & 0x0100_0000) != 0;
            let rlist = build_register_list((op & 0xFFFF) as u16);

            format!("{name}{inc}{before}{cond} {rn}{writeback}, {{ {rlist} }}{set}",
                name = if load {"LDM"} else {"STM"},
                cond = cond,
                inc = if up {"I"} else {"D"},
                before = if pre {"B"} else {"A"},
                rn = ARM_REGS[rn],
                writeback = if writeback {"!"} else {""},
                rlist = rlist,
                set = if set_cc {"^"} else {""}
            )
        }

        0xA00 ... 0xBFF => { // Branch / Branch with Link
            let offset = if op & 0x80_0000 != 0 {
                ((op & 0xFF_FFFF) | 0xFF00_0000) << 2
            } else {
                (op & 0xFF_FFFF) << 2
            };

            let addr = pc.wrapping_add(8).wrapping_add(offset);

            if op & 0x0100_0000 == 0 {
                format!("B{} #{:X}", cond, addr)
            } else {
                format!("BL{} #{:X}", cond, addr)
            }
        }

        0xC00 ... 0xDFF => { // Coprocessor Data Transfer
            let cp_num = op >> 8 & 0xF;
            let crd = op >> 12 & 0xF;
            let rn = ARM_REGS[(op >> 16 & 0xF) as usize];
            let offset = (op & 0xFF) << 2;

            let load = (op & 0x0010_0000) != 0;
            let writeback = (op & 0x0020_0000) != 0;
            let transfer_length = (op & 0x0040_0000) != 0;
            let up = if (op & 0x0080_0000) != 0 {""} else {"-"};
            let preindex = (op & 0x0100_0000) != 0;

            let name = if load {"LDC"} else {"STC"};
            let l = if transfer_length {"L"} else {""};

            if preindex {
                format!("{name}{cond}{l} {cp_num}, {crd}, [{rn}, {up}{offset}]{writeback}",
                    name = name, cond = cond, l = l,
                    cp_num = cp_num, crd = crd, up = up,
                    rn = rn, offset = offset,
                    writeback = if writeback {"!"} else {""}
                )
            } else {
                format!("{name}{cond}{l} {cp_num}, {crd}, [{rn}], {up}{offset}",
                    name = name, cond = cond, l = l,
                    cp_num = cp_num, crd = crd,
                    rn = rn, offset = offset, up = up
                )
            }
        }

        0xE00 ... 0xEFF => {
            let crm = op & 0xF;
            let cp = op >> 5 & 7;
            let cp_num = op >> 8 & 0xF;
            let crn = op >> 16 & 0xF;
            if (mask & 0x10) == 0 { // Coprocessor Data Operation
                let crd = op >> 12 & 0xF;
                let cp_opcode = op >> 20 & 0xF;

                format!("CDP{cond} {cp_num}, {cp_opcode}, CR{crd}, CR{crn}, CR{crm}, {cp}",
                    cond = cond, cp_num = cp_num, cp_opcode = cp_opcode,
                    crd = crd, crn = crn, crm = crm, cp = cp
                )
            } else { // Coprocessor Register Transfer
                let rd = (op >> 12 & 0xF) as usize;
                let load = (op & 0x0010_0000) != 0;
                let cp_opcode = op >> 21 & 7;

                let name = if load {"MRC"} else {"MCR"};

                format!("{name}{cond} {cp_num}, {cp_opcode}, {rd}, CR{crn}, CR{crm}, {cp}",
                    name = name, cond = cond, cp_num = cp_num, cp_opcode = cp_opcode,
                    rd = ARM_REGS[rd], crn = crn, crm = crm, cp = cp
                )
            }
        }

        0xF00 ... 0xFFF => { // Software Interrupt
            format!("SWI{} #{:06X}", cond, op & 0x00FF_FFFF)
        }

        _ => invalid(op),
    }
}

#[test]
fn data_processing() {
    assert_eq!(disassemble_arm_opcode(0xE0110002, 0), "ANDS R0, R1, R2");
    assert_eq!(disassemble_arm_opcode(0x022C900F, 0), "EOREQ R9, R12, #15 ; 0xF");
    assert_eq!(disassemble_arm_opcode(0xC255BC0F, 0), "SUBSGT R11, R5, #3840 ; 0xF00"); // 0xFF ROR 24 (half the shift)
    assert_eq!(disassemble_arm_opcode(0x3087320D, 0), "ADDCC R3, R7, SP LSL #4");
    assert_eq!(disassemble_arm_opcode(0x51865C5A, 0), "ORRPL R5, R6, R10 ASR R12");
    assert_eq!(disassemble_arm_opcode(0xE1E01001, 0), "MVN R1, R1");
    assert_eq!(disassemble_arm_opcode(0xE1130424, 0), "TST R3, R4 LSR #8");
}

pub fn disassemble_thumb_opcode(op: u32, pc: u32) -> String {
    static ALU_OP_NAMES: [&'static str; 16] = [
        "AND", "EOR", "LSL", "LSR", "ASR", "ADC", "SBC", "ROR",
        "TST", "NEG", "CMP", "CMN", "ORR", "MUL", "BIC", "MVN",
    ];

    match op >> 8 {
        0x00 ... 0x07 => format!("LSL R{}, R{}, #{}", op & 7, op >> 3 & 7, op >> 6 & 31),
        0x08 ... 0x0F => format!("LSR R{}, R{}, #{}", op & 7, op >> 3 & 7, op >> 6 & 31),
        0x10 ... 0x17 => format!("ASR R{}, R{}, #{}", op & 7, op >> 3 & 7, op >> 6 & 31),

        0x18 | 0x19 => format!("ADD R{}, R{}, R{}", op & 7, op >> 3 & 7, op >> 6 & 7),
        0x1A | 0x1B => format!("SUB R{}, R{}, R{}", op & 7, op >> 3 & 7, op >> 6 & 7),
        0x1C | 0x1D => format!("ADD R{}, R{}, #{}", op & 7, op >> 3 & 7, op >> 6 & 7),
        0x1E | 0x1F => format!("SUB R{}, R{}, #{}", op & 7, op >> 3 & 7, op >> 6 & 7),

        0x20 ... 0x27 => format!("MOV R{}, #{} ; 0x{:X}", op >> 8 & 7, op & 0xFF, op & 0xFF),
        0x28 ... 0x2F => format!("CMP R{}, #{}", op >> 8 & 7, op & 0xFF),
        0x30 ... 0x37 => format!("ADD R{}, #{}", op >> 8 & 7, op & 0xFF),
        0x38 ... 0x3F => format!("SUB R{}, #{}", op >> 8 & 7, op & 0xFF),

        0x40 ... 0x43 => format!("{name} R{rd}, R{rs}",
            name = ALU_OP_NAMES[(op as usize) >> 6 & 0b1111],
            rd = op & 7, rs = op >> 3 & 7
        ),

        0x44 ... 0x47 => {
            let rs = ARM_REGS[(op >> 3 & 0b1111) as usize];
            let rd = ARM_REGS[((op & 0b111) | ((op & 0x80) >> 4)) as usize];
            match op >> 8 & 3 {
                0b00 => format!("ADD {}, {}", rd, rs),
                0b01 => format!("CMP {}, {}", rd, rs),
                0b10 => format!("MOV {}, {}", rd, rs),
                0b11 => format!("BX {}", rs),
                _ => unreachable!(),
            }
        }

        0x48 ... 0x4F => format!("LDR R{}, [PC, #{}]", (op >> 8) & 7, (op & 0xFF) << 2),

        0x50 ... 0x5F => {
            static MEMOP_NAMES: [&'static str; 8] = [
                "STR", "STRH", "STRB", "LDSB", "LDR", "LDRH", "LDRB", "LDSH",
            ];

            let rd = op & 7;
            let rb = (op >> 3) & 7;
            let ro = (op >> 6) & 7;
            let name = MEMOP_NAMES[((op >> 9) & 7) as usize];

            format!("{name} R{rd}, [R{rb}, R{ro}]",
                name = name,
                rd = rd, rb = rb, ro = ro
            )
        }

        0x60 ... 0x7F => {
            let load = (op & 0x0800) != 0;
            let byte = (op & 0x1000) != 0;
            let rd = op & 7;
            let rb = (op >> 3) & 7;

            let offset = if byte {
                (op >> 6) & 0b11111
            } else {
                ((op >> 6) & 0b11111) << 2
            };

            let name = match (load, byte) {
                (false, false) => "STR",
                (false, true ) => "LDR",
                (true,  false) => "STRB",
                (true,  true ) => "LDRB",
            };

            format!("{name} R{rd}, [R{rb}, #{imm}]",
                name = name,
                rd = rd, rb = rb,
                imm = offset
            )
        }

        0x80 ... 0x8F => {
            let load = (op & 0x0800) != 0;
            let rd = op & 7;
            let rb = (op >> 3) & 7;
            let offset = ((op >> 6) & 0b11111) << 1;

            if load {
                format!("LDRH R{rd}, [R{rb}, #{imm}]",
                    rd = rd, rb = rb, imm = offset
                )
            } else {
                format!("STRH R{rd}, [R{rb}, #{imm}]",
                    rd = rd, rb = rb, imm = offset
                )
            }
        }

        0x90 ... 0x9F => {
            let imm = (op & 0xFF) << 2;
            let rd = (op >> 8) & 7;
            let load = (op & 0x0800) != 0;

            if load {
                format!("LDR R{rd}, [SP, #{imm}]", rd = rd, imm = imm)
            } else {
                format!("STR R{rd}, [SP, #{imm}]", rd = rd, imm = imm)
            }
        }

        0xA0 ... 0xAF => {
            let imm = (op & 0xFF) << 2;
            let rd = (op >> 8) & 7;
            let sp = (op & 0x0800) != 0;

            format!("ADD R{rd}, {rs}, #{imm}",
                rd = rd, imm = imm,
                rs = if sp { "SP" } else { "PC" }
            )
        }

        0xB0 => {
            let sign = (op & 0x80) != 0;
            let imm = ((op & 0x7F) as i32) << 2;
            format!("ADD SP, #{imm}",
                imm = if sign { -imm } else { imm }
            )
        }

        0xB4 | 0xB5 | 0xBC | 0xBD => {
            let load = (op & 0x0800) != 0;
            let lr = (op & 0x0100) != 0;
            let mut registers = op & 0xFF;

            if lr {
                if load {
                    registers |= 1 << REG_PC;
                } else {
                    registers |= 1 << REG_LR;
                }
            }

            let rlist = build_register_list(registers as u16);

            if load {
                format!("POP {{ {rlist} }}", rlist = rlist)
            } else {
                format!("PUSH {{ {rlist} }}", rlist = rlist)
            }
        }

        0xC0 ... 0xCF => {
            let load = (op & 0x0800) != 0;
            let rb = (op >> 8) & 7;
            let registers = op & 0xFF;

            let rlist = build_register_list(registers as u16);

            if load {
                format!("LDMIA R{rb}!, {{ {rlist} }}", rb = rb, rlist = rlist)
            } else {
                format!("STMIA R{rb}!, {{ {rlist} }}", rb = rb, rlist = rlist)
            }
        }

        0xD0 ... 0xDD => {
            // cond 14(b1110) is undefined, 15(b1111) creates SWI instruction
            let cond = ((op >> 8) & 0xF) as usize;
            let offset: u32 = if (op & 0x80) != 0 { //((op & 0xFF) as i8 as i32) * 2;
                (0xFFFF_FF00u32 | (op & 0xFF) as u32) << 1
            } else {
                ((op & 0xFF) as u32) << 1
            } as u32;
            let addr = pc.wrapping_add(4).wrapping_add(offset);

            format!("B{} {:08X}", CC_NAMES[cond], addr)
        }

        0xDE => format!("UNDEFINED #{:02X}", op & 0xFF),
        0xDF => format!("SWI #{}", op & 0xFF),

        0xE0 ... 0xE7 => {
            let op = op as u32;

            let offset: u32 = if (op & 0x0400) != 0 {
                (0xFFFF_8000 | (op & 0x07FF)) << 1
            } else {
                (op & 0x07FF) << 1
            };

            let addr = pc.wrapping_add(4).wrapping_add(offset);

            format!("B {:08X}", addr)
        }

        0xF0 ... 0xFF => {
            if (op & 0x0800) == 0 { // hi
                let offset = sign_extend(op & 0x7FF, 11) << 12;
                let addr = pc.wrapping_add(offset);
                format!("BL_hi {:05X}xxx", addr >> 12)
            } else { // lo
                let offset = (op & 0x7FF) << 1;
                let addr = pc.wrapping_add(offset);
                format!("BL_lo xxxxx{:03X}", addr & 0xFFF)
            }
        }

        _ => format!("UNKNOWN OPCODE 0x{:04X}", op),
    }
}
