#![allow(dead_code)]

use std::io::{ Read };
use std::fs::File;

extern crate num;

#[macro_use]
mod utils;
mod io;

mod arm7tdmi;
mod interconnect;
mod thumb_core;
mod arm_core;
mod disassemble;
mod debugger;

use disassemble::{ disassemble_arm_opcode, disassemble_thumb_opcode };
use arm7tdmi::{ Arm7TDMI, REG_PC, StepResult };

fn main() {
//    let cmdline ="
//        s
//        step 4
//        lregs
//        c
//        save State1
//        storew 0x04000208 0xDEADBEEF
//        storeb 0x04000000 123
//        restore State1
//    ";
//
//    for cmd in cmdline.trim().split('\n') {
//        println!("{:?}", debugger::parse_command(cmd).unwrap());
//    }
//
//    return;

    let mut file = File::open("roms/bios.bin").unwrap();
    let mut bios = Vec::new();
    file.read_to_end(&mut bios).unwrap();

    let mut arm = Arm7TDMI::new(bios.into_boxed_slice());

    arm.signal_reset();

    loop {
        let step = arm.get_op_size();
        let StepResult { op, thumb_mode } = arm.step();
        let pc = arm.regs[REG_PC] - 2*step;

        if thumb_mode {
            let dis = disassemble_thumb_opcode(&arm.mem, pc);
            println!("@{:08X}: ({:04X}): {}", pc, op, dis);
        } else {
            let dis = disassemble_arm_opcode(op, pc);
            println!("@{:08X}: ({:08X}): {}", pc, op, dis);
        }

        println!("{:?}", arm);
    }
}
