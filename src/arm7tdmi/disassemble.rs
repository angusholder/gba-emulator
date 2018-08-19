use bitintr::Pext;
use num::abs;
use regex::Regex;

use std::error::Error as StdError;
use std::ops::BitOr;
use std::fmt::Write;
use regex::Captures;

use arm7tdmi::{ REG_LR, REG_PC };

type DisResult<T> = Result<T, Box<StdError>>;

#[derive(Clone, Copy)]
struct BitExtractMask(u32);

enum Tag {
    Operand2,
    SetCC,
    RotatedImmediate,
    Writeback,
}

enum FormatOp {
    // A plain string
    Str(String),

    // %Rd, %Hs
    Reg {
        bit_extract_mask: u32,
        reg_names: &'static [&'static str],
    },

    // %+Rl
    RegList {
        mask: u16,
    },

    // <some_name>
    Tag(Tag),

    // $jmp[j]
    JumpOffset {
        bit_extract_mask: u32,
    },

    // #imm[i]
    Imm {
        bit_extract_mask: u32,
    },
}

lazy_static! {
    static ref IMM_REGEX: Regex = Regex::new(r"[[:word:]]*\[([a-z])\]").unwrap();
    static ref JMP_REGEX: Regex = Regex::new(r"[[:word:]]*\[([a-z])\]").unwrap();
    static ref FLAG_REGEX: Regex = Regex::new( r"'([^']+)'\[([a-z])\]").unwrap();
}

static REG_NAMES: [&'static str; 16] = [
    "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7",
    "R8", "R9", "R10", "R11", "R12", "SP", "LR", "PC"
];

static PSR_REG_NAMES: [&'static str; 2] = [
    "CPSR", "SPSR",
];

fn err<T: Into<Box<StdError>>>(t: T) -> Box<StdError> { t.into() }

fn is_plaintext(c: char) -> bool {
    match c {
        'A'..='Z' | 'a'..='z' | ' ' | ',' | '[' | ']' | '-' | '{' | '}' => true,
        _ => false,
    }
}

fn make_extract_mask(bit_fmt: &str, bitfield_name: char) -> DisResult<u32> {
    if bit_fmt.find(bitfield_name) == None {
        Err(err(format!("Bitfield '{}' was not found in bit format", bitfield_name)))
    } else {
        Ok(bit_fmt
            .char_indices()
            .filter(move |&(_, c)| c == bitfield_name)
            .map(|(i, _)| 1 << i)
            .fold(0u32, BitOr::bitor))
    }
}

fn compile(asm_fmt: &str, bit_fmt: &str) -> DisResult<Vec<FormatOp>> {
    // Remove spaces, and reverse the bits so they are in order from bit0 onwards
    // This way we can just use the character index as the bit index
    let bit_fmt = bit_fmt.chars().rev().filter(|&c| c != ' ').collect::<String>();
    let bit_len = bit_fmt.len();

    if bit_len != 16 && bit_len != 28 {
        return Err(err(format!("Expected bit format to contain 16 or 32 field characters, got {}", bit_len)));
    }

    let mut asm_iter = asm_fmt.char_indices().peekable();
    let mut ops = Vec::new();
    while let Some((index, c)) = asm_iter.next() {
        let mut to_skip = 0;
        let op: FormatOp = match c {
            '%' => {
                let reglist = asm_iter.peek().map(|(_, c)| *c) == Some('+');

                let (_, reg_kind) = asm_iter.next().ok_or(err("Register definition is missing its kind"))?;
                let (_, reg_name) = asm_iter.next().ok_or(err("Register definition is missing its name"))?;

                let bit_extract_mask = make_extract_mask(&bit_fmt, reg_name)?;

                if reglist {
                    let mut mask = bit_extract_mask as u16;

                    match reg_kind {
                        'U' => {
                            // Lower 8 regs only + optionally LR
                            mask &= 0xFF;
                            if bit_extract_mask & 0x100 != 0 {
                                mask |= REG_LR as u16;
                            }
                        }
                        'O' => {
                            // Lower 8 regs only + optionally PC
                            mask &= 0xFF;
                            if bit_extract_mask & 0x100 != 0 {
                                mask |= REG_PC as u16;
                            }
                        }
                        'L' => {
                            mask &= 0xFF;
                        }
                        'R' => {
                            // Leave mask alone, this is a full ARM instr, we can do all 16 regs
                        }
                        _ => return Err(err(format!("Unrecognised register kind '{}'", reg_kind)))
                    };

                    FormatOp::RegList { mask }
                } else {
                    let reg_names = match reg_kind {
                        'R' | 'H' => &REG_NAMES[..],
                        'P' => &PSR_REG_NAMES,
                        _ => return Err(err(format!("Unrecognised register kind '{}'", reg_kind)))
                    };

                    FormatOp::Reg { bit_extract_mask, reg_names }
                }
            }
            '$' => {
                let caps: Captures = JMP_REGEX.captures(&asm_fmt[index..]).ok_or(err("Jump offset was formatted incorrectly"))?;
                to_skip = caps[0].len();
                let bitfield_name = caps[1].chars().next().unwrap();
                let bit_extract_mask = make_extract_mask(&bit_fmt, bitfield_name)?;
                FormatOp::JumpOffset { bit_extract_mask }
            }
            '#' => {
                let caps: Captures = IMM_REGEX.captures(&asm_fmt[index..]).ok_or(err("Immediate was formatted incorrectly"))?;
                to_skip = caps[0].len();
                let bitfield_name = caps[1].chars().next().unwrap();
                let bit_extract_mask = make_extract_mask(&bit_fmt, bitfield_name)?;
                FormatOp::Imm { bit_extract_mask }
            }
            _ if is_plaintext(c) => {
                let mut text = String::new();
                text.push(c);
                while let Some((_, c)) = asm_iter.next() {
                    if is_plaintext(c) {
                        text.push(c);
                    } else {
                        break;
                    }
                }
                FormatOp::Str(text)
            }
            _ => {
                return Err(err(format!("Unexpected character '{}'", c)));
            }
        };
        ops.push(op);
        for _ in 0..to_skip { asm_iter.next(); }
    }
    Ok(ops)
}

fn disassemble(word: u32, address: u32, ops: &[FormatOp], output: &mut Write) -> DisResult<()> {
    for op in ops {
        match op {
            FormatOp::Str(s) => {
                output.write_str(s)?;
            }
            FormatOp::Reg { bit_extract_mask, reg_names } => {
                let reg_index = word.pext(*bit_extract_mask) as usize;
                let reg_name = reg_names[reg_index];
                output.write_str(reg_name)?;
            }
            FormatOp::RegList { mask } => {
                // It's a shame join isn't available for iterators, collect() shouldn't be necessary
                let reglist = (0usize..16)
                    .filter(|i| (*mask as usize) & (1usize << i) != 0)
                    .map(|i| REG_NAMES[i])
                    .collect::<Vec<&str>>()
                    .join(", ");
                output.write_str(&reglist)?;
            }
            FormatOp::JumpOffset { bit_extract_mask } => {
                let offset = word.pext(*bit_extract_mask);
                // TODO: Add the appropriate branch target offset of 2/4/8
                let target = address.wrapping_add(offset);
                output.write_fmt(format_args!("${:08X}", target))?;
            }
            FormatOp::Imm { bit_extract_mask } => {
                let imm = word.pext(*bit_extract_mask);
                if abs(imm as i32) < 10_000 {
                    output.write_fmt(format_args!("d#{}", imm as i32))?;
                } else {
                    output.write_fmt(format_args!("#{:08X}", imm))?;
                }
            }
            FormatOp::Tag(_) => unimplemented!()
        }
    }
    Ok(())
}