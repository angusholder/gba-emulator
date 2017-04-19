#![allow(dead_code)]

extern crate num;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate bitflags;

#[macro_use]
mod log;
#[macro_use]
mod utils;
mod arm7tdmi;
mod interconnect;
mod disassemble;
mod debugger;
mod renderer;
mod timer;
mod gamepak;

use arm7tdmi::Arm7TDMI;
use interconnect::Interconnect;
use debugger::Debugger;

const BIOS_BIN: &'static [u8] = include_bytes!("../roms/bios.bin");
const PACMAN: &'static [u8] = include_bytes!("../roms/Pac-Man Collection.gba");

fn main() {
    let mut interconnect = Interconnect::new(BIOS_BIN, PACMAN);
    let mut arm = Arm7TDMI::new();
    arm.signal_reset(&mut interconnect);

    let mut debugger = Debugger::new(arm, interconnect);

    debugger.run();
}
