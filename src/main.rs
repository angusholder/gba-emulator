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
use arm_core::step_arm;
use thumb_core::step_thumb;
use arm7tdmi::{ Arm7TDMI, REG_PC };

fn main() {
    let cmdline ="
        s
        step 4
        lregs
        c
        save State1
        storew 0x04000208 0xDEADBEEF
        storeb 0x04000000 123
        restore State1
    ";

    for cmd in cmdline.trim().split('\n') {
        println!("{:?}", debugger::parse_command(cmd).unwrap());
    }

    return;

    let mut file = File::open("roms/bios.bin").unwrap();
    let mut bios = Vec::new();
    file.read_to_end(&mut bios).unwrap();

    let mut arm = Arm7TDMI::new(bios.into_boxed_slice());

    arm.signal_reset();

    loop {
        let step = arm.get_op_size();
        arm.regs[REG_PC] += step;

        let op = arm.mem.prefetch[0];
        arm.mem.prefetch[0] = arm.mem.prefetch[1];
        arm.mem.prefetch[1] = if arm.cpsr.thumb_mode {
            arm.mem.exec16(arm.regs[REG_PC]) as u32
        } else {
            arm.mem.exec32(arm.regs[REG_PC])
        };

        if arm.cpsr.thumb_mode {
            debug_assert!(arm.regs[REG_PC] & 1 == 0);
            let dis = disassemble_thumb_opcode(&arm.mem, arm.regs[REG_PC] - 2*step);
            println!("@{:08X}: ({:04X}): {}", arm.regs[REG_PC] - 2*step, op, dis);
            step_thumb(&mut arm, op as u16);
        } else {
            debug_assert!(arm.regs[REG_PC] & 3 == 0);
            let dis = disassemble_arm_opcode(op, arm.regs[REG_PC] - 2*step);
            println!("@{:08X}: ({:08X}): {}", arm.regs[REG_PC] - 2*step, op, dis);
            step_arm(&mut arm, op);
        }

        println!("{:?}", arm);
    }
}
