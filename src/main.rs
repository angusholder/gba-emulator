#![allow(dead_code)]

use std::io::{ Read };
use std::fs::File;

mod arm7tdmi;
mod memory;
mod thumb_core;
mod arm_core;
mod disassemble;

#[macro_use]
mod utils;
mod io;

use disassemble::{ disassemble_arm_opcode, disassemble_thumb_opcode };
use arm_core::step_arm;
use thumb_core::step_thumb;
use arm7tdmi::{ Arm7TDMI, REG_PC };

fn main() {
    let mut file = File::open("roms/bios.bin").unwrap();
    let mut bios = Vec::new();
    file.read_to_end(&mut bios).unwrap();

    let mut arm = Arm7TDMI::new(bios.into_boxed_slice());

    arm.sig_reset();

    loop {
        let op = arm.fetch_next_opcode();

        if arm.cpsr.thumb_mode {
            debug_assert!(arm.regs[REG_PC] & 1 == 0);
            let dis = disassemble_thumb_opcode(&arm.mem, arm.regs[REG_PC] - 4);
            println!("@{:08X}: ({:04X}): {}", arm.regs[REG_PC] - 4, op, dis);
            step_thumb(&mut arm, op as u16);
        } else {
            debug_assert!(arm.regs[REG_PC] & 3 == 0);
            let dis = disassemble_arm_opcode(op, arm.regs[REG_PC] - 8);
            println!("@{:08X}: ({:08X}): {}", arm.regs[REG_PC] - 8, op, dis);
            step_arm(&mut arm, op);
        }

        println!("{:?}", arm);
    }
}
