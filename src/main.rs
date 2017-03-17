#![allow(dead_code)]

extern crate num;
#[macro_use]
extern crate enum_primitive;

#[macro_use]
mod utils;
mod arm7tdmi;
mod interconnect;
mod thumb_core;
mod arm_core;
mod disassemble;
mod debugger;

use arm7tdmi::Arm7TDMI;
use interconnect::Interconnect;
use debugger::Debugger;

const BIOS_BIN: &'static [u8] = include_bytes!("../roms/bios.bin");

fn main() {
    let mut interconnect = Interconnect::new(BIOS_BIN);
    let mut arm = Arm7TDMI::new();
    arm.signal_reset(&mut interconnect);

    let mut debugger = Debugger::new(arm, interconnect);

    debugger.run();
}
