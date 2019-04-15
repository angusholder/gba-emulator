use num_traits::FromPrimitive;

use crate::bus::Bus;
use crate::gba::{Gba, IrqFlags };
use crate::log::*;
use crate::utils::Latched;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Primitive)]
enum DmaTransferType {
    U16 = 0,
    U32 = 1,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Primitive)]
enum DmaStartTiming {
    Immediately = 0,
    VBlank = 1,
    HBlank = 2,

    // DMA0=Prohibited, DMA1/DMA2=Sound FIFO, DMA3=Video Capture
    Special = 3,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Primitive)]
enum DmaAddrControl {
    Increment = 0,
    Decrement = 1,
    Fixed = 2,

    // Valid only for dest, not source.
    IncrementReload = 3,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DmaUnit {
    Dma0 = 0,
    Dma1 = 1,
    Dma2 = 2,
    Dma3 = 3,
}

#[derive(Clone, Copy)]
struct RunState {
    source: u32,
    dest: u32,
    words_remaining: u32,
    dest_step: u32,
    source_step: u32,
}

use self::DmaUnit::*;

#[derive(Clone, Copy)]
pub struct Dma {
    unit: DmaUnit,

    source: Latched<u32>,
    dest: Latched<u32>,
    word_count: Latched<u32>,

    running: Option<RunState>,

    enable: bool,
    dest_control: DmaAddrControl,
    source_control: DmaAddrControl,
    repeat: bool,
    transfer_type: DmaTransferType,
    irq_on_complete: bool,
    start_timing: DmaStartTiming,

    // DMA 3 only
    gamepak_drq: bool,
}

fn step_dma_transfer(gba: &mut Gba, index: usize) -> IrqFlags {
    let mut flags = IrqFlags::empty();
    let i = index;

    assert!(gba.dma[i].enable);

    // `running` must be `Some` when calling `step_dma_transfer()`
    let mut state = gba.dma[i].running.unwrap();

    if gba.dma[i].transfer_type == DmaTransferType::U16 {
        let word = gba.read16(state.source);
        gba.write16(state.dest, word);
    } else {
        let word = gba.read32(state.source);
        gba.write32(state.dest, word);
    }

    state.dest = state.dest.wrapping_add(state.dest_step);
    state.source = state.source.wrapping_add(state.source_step);
    state.words_remaining -= 1;

    // This was the last word of the transfer
    if state.words_remaining == 0 {
        if gba.dma[i].irq_on_complete {
            flags |= gba.dma[i].irq_flag();
        }

        if !gba.dma[i].repeat {
            gba.dma[i].enable = false;
        }

        gba.dma[i].running = None;
    } else {
        gba.dma[i].running = Some(state);
    }

    flags
}

pub fn step_dma_units(gba: &mut Gba) -> Option<IrqFlags> {
    for i in 0..4 {
        if gba.dma[i].running.is_some() {
            // We are mid-transfer so transfer the next word
            return Some(step_dma_transfer(gba, i));
        }
    }

    None
}

pub fn dma_on_vblank(gba: &mut Gba) {
    for i in 0..4 {
        if gba.dma[i].enable && gba.dma[i].start_timing == DmaStartTiming::VBlank {
            // TODO: What to do if a transfer is already in progress
            assert!(gba.dma[i].running.is_none());
            initialize_dma(&mut gba.dma[i]);
        }
    }
}

pub fn dma_on_hblank(gba: &mut Gba) {
    for i in 0..4 {
        if gba.dma[i].enable && gba.dma[i].start_timing == DmaStartTiming::HBlank {
            // TODO: What to do if a transfer is already in progress
            assert!(gba.dma[i].running.is_none());
            initialize_dma(&mut gba.dma[i]);
        }
    }
}

fn initialize_dma(dma: &mut Dma) {
    use self::DmaAddrControl::*;

    let step_size = if dma.transfer_type == DmaTransferType::U16 { 2 } else { 4 };

    // Repeats cause the word count to latch
    dma.word_count.latch();

    let dest_step = match dma.dest_control {
        Increment => step_size,
        Decrement => -(step_size as i32) as u32,
        Fixed => 0,
        IncrementReload => {
            dma.dest.latch();
            step_size
        }
    };

    let source_step = match dma.source_control {
        Increment => step_size,
        Decrement => -(step_size as i32) as u32,
        Fixed => 0,
        IncrementReload => unreachable!(),
    };

    dma.running = Some(RunState {
        source: dma.source.get(),
        dest: dma.dest.get(),
        words_remaining: dma.word_count.get(),
        dest_step,
        source_step,
    });
}

fn write_latched_lo(unit: DmaUnit, l: &mut Latched<u32>, value: u16) {
    let cur = l.get_next();
    l.set(map_addr(unit, (cur & !0xFFFF) | (value as u32)));
}

fn write_latched_hi(unit: DmaUnit, l: &mut Latched<u32>, value: u16) {
    let cur = l.get_next();
    l.set(map_addr(unit, (cur & 0xFFFF) | (value as u32) << 16));
}

fn map_addr(unit: DmaUnit, addr: u32) -> u32 {
    match unit {
        Dma0 => { // internal memory only
            addr & 0x07FF_FFFF
        }
        Dma1 | Dma2 | Dma3 => { // any memory
            addr & 0x0FFF_FFFF
        }
    }
}

impl Dma {
    pub fn new(unit: DmaUnit) -> Dma {
        Dma {
            unit,

            source: Latched::new(0),
            dest: Latched::new(0),
            word_count: Latched::new(0),

            running: None,

            enable: false,
            dest_control: DmaAddrControl::Increment,
            source_control: DmaAddrControl::Increment,
            repeat: false,
            transfer_type: DmaTransferType::U16,
            irq_on_complete: false,
            start_timing: DmaStartTiming::Immediately,

            gamepak_drq: false,
        }
    }

    fn log_kind(&self) -> LogKind {
        match self.unit {
            Dma0 => LogKind::DMA0,
            Dma1 => LogKind::DMA1,
            Dma2 => LogKind::DMA2,
            Dma3 => LogKind::DMA3,
        }
    }

    fn irq_flag(&self) -> IrqFlags {
        match self.unit {
            Dma0 => IrqFlags::DMA0,
            Dma1 => IrqFlags::DMA1,
            Dma2 => IrqFlags::DMA2,
            Dma3 => IrqFlags::DMA3,
        }
    }

    pub fn write_source(&mut self, addr: u32) {
        self.source.set(map_addr(self.unit, addr));
    }

    pub fn write_dest(&mut self, addr: u32) {
        self.dest.set(map_addr(self.unit, addr));
    }

    pub fn write_source_lo(&mut self, addr: u16) { write_latched_lo(self.unit, &mut self.source, addr); }
    pub fn write_source_hi(&mut self, addr: u16) { write_latched_hi(self.unit, &mut self.source, addr); }
    pub fn write_dest_lo(&mut self, addr: u16) { write_latched_lo(self.unit, &mut self.dest, addr); }
    pub fn write_dest_hi(&mut self, addr: u16) { write_latched_hi(self.unit, &mut self.dest, addr); }

    pub fn write_word_count(&mut self, count: u16) {
        // A word count of 0 is treated as the maximum value.
        match self.unit {
            Dma0 | Dma1 | Dma2 => { // [1,0x4000]
                if count == 0 {
                    self.word_count.set(0x4000);
                } else {
                    self.word_count.set((count & 0x3FFF) as u32);
                }
            }
            Dma3 => { // [1,0x10_000]
                if count == 0 {
                    self.word_count.set(0x10_000);
                } else {
                    self.word_count.set(count as u32);
                }
            }
        }
    }

    pub fn read_control(&self) -> u16 {
        DmaControlReg {
            dest_control: self.dest_control as u8,
            source_control: self.source_control as u8,
            repeat: self.repeat,
            transfer_type: self.transfer_type as u8,
            gamepak_drq: self.gamepak_drq,
            start_timing: self.start_timing as u8,
            irq_on_complete: self.irq_on_complete,
            enable: self.enable,
        }.into()
    }

    pub fn write_control(&mut self, word: u16) {
        let reg = DmaControlReg::from(word);

        if reg.enable && !self.enable {
            self.dest.latch();
            self.source.latch();
            self.word_count.latch();
        }

        self.enable = reg.enable;
        self.repeat = reg.repeat;
        self.irq_on_complete = reg.irq_on_complete;
        self.dest_control = DmaAddrControl::from_u8(reg.dest_control).unwrap();
        self.transfer_type = DmaTransferType::from_u8(reg.transfer_type).unwrap();

        let start_timing = DmaStartTiming::from_u8(reg.start_timing).unwrap();
        if self.unit == Dma0 {
            warn!(self.log_kind(), "Tried to set start_timing to Special");
        } else {
            self.start_timing = start_timing;
        }

        let source_control = DmaAddrControl::from_u8(reg.source_control).unwrap();
        if source_control == DmaAddrControl::IncrementReload {
            warn!(self.log_kind(), "Tried to set source_control to IncrementReload");
        } else {
            self.source_control = source_control;
        }

        if self.unit == DmaUnit::Dma3 {
            if self.gamepak_drq {
                assert!(self.repeat == false);
            } else {
                self.gamepak_drq = reg.gamepak_drq;
            }
        } else if reg.gamepak_drq {
            warn!(self.log_kind(), "Tried to set gamepak_drq (valid only for DMA3)");
        }

        if self.enable && self.start_timing == DmaStartTiming::Immediately {
            // We are enabled so work out if we should start a transfer this cycle.
            initialize_dma(self);
        }
    }
}

unpacked_bitfield_struct! {
struct DmaControlReg: u16 {
    (5,2) dest_control: u8,
    (7,2) source_control: u8,
    (9,1) repeat: bool,
    (10,1)transfer_type: u8,
    (11,1)gamepak_drq: bool,
    (12,2)start_timing: u8,
    (14,1)irq_on_complete: bool,
    (15,1)enable: bool,
}
}
