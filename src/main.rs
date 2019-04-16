#![allow(dead_code)]

#[macro_use]
extern crate enum_primitive_derive;
#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate imgui;

#[macro_use]
mod log;
#[macro_use]
mod utils;
mod arm7tdmi;
mod gba;
mod disassemble;
mod debugger;
mod renderer;
mod timer;
mod gamepak;
mod dma;
mod iomap;
mod bus;

use crate::gba::Gba;
use crate::debugger::Debugger;
use crate::renderer::{PHYS_WIDTH, PHYS_HEIGHT, Framebuffer};
use std::error::Error;
use std::fs;

fn main() -> Result<(), Box<Error>> {
    let bios = fs::read("roms/bios.bin")?;
    let rom = fs::read("roms/Pac-Man Collection.gba")?;
    let mut gba = Gba::new(&bios, &rom);
    gba.arm.signal_reset();

    let mut debugger = Debugger::new(gba);

    debugger.run();
    Ok(())
}
