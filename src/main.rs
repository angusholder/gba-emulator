#![allow(dead_code)]

extern crate num;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate bitflags;
extern crate bitintr;
extern crate regex;
#[macro_use]
extern crate lazy_static;
extern crate core;

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
mod dma;
mod iomap;
mod bus;

use interconnect::Interconnect;
use debugger::Debugger;
use std::error::Error;
use std::fs;

fn main() -> Result<(), Box<Error>> {
    let bios = fs::read("roms/bios.bin")?;
    let rom = fs::read("roms/Pac-Man Collection.gba")?;
    let mut interconnect = Interconnect::new(&bios, &rom);
    interconnect.arm.signal_reset();

    let mut debugger = Debugger::new(interconnect);

    debugger.run();
    Ok(())
}
