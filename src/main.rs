#![allow(dead_code)]

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

use std::io::{ Read };
use std::fs::File;

use arm7tdmi::Arm7TDMI;
use interconnect::Interconnect;
use debugger::Debugger;

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
    let bios = bios.into_boxed_slice();

    let mut interconnect = Interconnect::new(bios);
    let mut arm = Arm7TDMI::new();
    arm.signal_reset(&mut interconnect);

    let mut debugger = Debugger::new(arm, interconnect);

    debugger.run();
}
