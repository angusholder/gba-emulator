use utils::{ Buffer, Cycle };
use renderer::{ Renderer, FrameBuffer };
use dma::{ Dma, DmaUnit, step_dma_units, dma_on_vblank, dma_on_hblank };
use timer::{ Timer, TimerUnit, step_timers };
use gamepak::GamePak;
use log::*;
use arm7tdmi::{ Arm7TDMI, StepEvent };
use iomap;

const ROM_SIZE: u32 = 0x4000;
const ROM_MASK: u32 = ROM_SIZE - 1;
const ROM_TIMING_U8: Cycle = Cycle(1);
const ROM_TIMING_U16: Cycle = Cycle(1);
const ROM_TIMING_U32: Cycle = Cycle(1);

const EWRAM_SIZE: u32 = 256 * 1024;
const EWRAM_MASK: u32 = EWRAM_SIZE - 1;
const EWRAM_TIMING_U8: Cycle = Cycle(3);
const EWRAM_TIMING_U16: Cycle = Cycle(3);
const EWRAM_TIMING_U32: Cycle = Cycle(6);

const IWRAM_SIZE: u32 = 32 * 1024;
const IWRAM_MASK: u32 = IWRAM_SIZE - 1;
const IWRAM_TIMING_U8: Cycle = Cycle(1);
const IWRAM_TIMING_U16: Cycle = Cycle(1);
const IWRAM_TIMING_U32: Cycle = Cycle(1);

const IO_TIMING_U8: Cycle = Cycle(1);
const IO_TIMING_U16: Cycle = Cycle(1);
const IO_TIMING_U32: Cycle = Cycle(1);

const BASE_ROM: u32 = 0x0;
const BASE_EWRAM: u32 = 0x2;
const BASE_IWRAM: u32 = 0x3;
const BASE_IOREGS: u32 = 0x4;
const BASE_PALETTE: u32 = 0x5;
const BASE_VRAM: u32 = 0x6;
const BASE_OAM: u32 = 0x7;
const BASE_GAMEPAK_START: u32 = 0x8;
const BASE_GAMEPAK_END: u32 = 0xD;

macro_rules! unhandled_read {
    ($self:expr, $addr:expr) => ({
        warn!(IO, "Out of range read at 0x{:04x}_{:04x}", $addr >> 16, $addr & 0xFFFF);
        (Cycle(1), $self.prefetch[1] as _)
    })
}

macro_rules! unhandled_write {
    ($addr:expr, $value:expr) => {
        error!(IO, "Out of range write at 0x{:04x}_{:04x} of {:X}", $addr >> 16, $addr & 0xFFFF, $value)
    }
}

#[derive(Clone)]
pub struct Interconnect {
    pub prefetch: [u32; 2],

    pc_inside_bios: bool,
    pub cycles: Cycle,

    pub timers: [Timer; 4],
    pub dma: [Dma; 4],
    pub(crate) renderer: Renderer,

    rom: Buffer,
    ewram: Buffer,
    iwram: Buffer,
    pub io_cache: Buffer,
    pub(crate) gamepak: GamePak,

    pub(crate) sram_wait_control: Cycle,
    pub(crate) gamepak_prefetch_buffer: bool,

    pub(crate) post_boot_flag: bool,
    pub(crate) master_interrupt_enable: bool,
    pub(crate) interrupt_enable: IrqFlags,
    pub(crate) interrupt_flags: IrqFlags,
    pub(crate) sound_bias: SoundPWMControlReg,
}

// Regions mirrored:
// - EWRam
// - IWRam
// - Palette
// - VRam
// - OAM

// TODO: Handle misaligned reads/writes
impl Interconnect {
    pub fn new(bios: &[u8], game: &[u8]) -> Interconnect {
        Interconnect {
            prefetch: Default::default(),

            pc_inside_bios: true,
            cycles: Cycle(0),

            timers: [
                Timer::new(TimerUnit::Tm0),
                Timer::new(TimerUnit::Tm1),
                Timer::new(TimerUnit::Tm2),
                Timer::new(TimerUnit::Tm3)
            ],
            dma: [
                Dma::new(DmaUnit::Dma0),
                Dma::new(DmaUnit::Dma1),
                Dma::new(DmaUnit::Dma2),
                Dma::new(DmaUnit::Dma3)
            ],
            renderer: Renderer::new(),

            rom: Buffer::new(bios),
            ewram: Buffer::with_capacity(EWRAM_SIZE),
            iwram: Buffer::with_capacity(IWRAM_SIZE),
            io_cache: Buffer::with_capacity(0x804),
            gamepak: GamePak::new(game),

            sram_wait_control: Cycle(4),
            gamepak_prefetch_buffer: false,

            post_boot_flag: false,
            master_interrupt_enable: true,
            interrupt_enable: IrqFlags::empty(),
            interrupt_flags: IrqFlags::empty(),
            sound_bias: SoundPWMControlReg {
                bias_level: 0x200,
                amplitude_resolution: 0,
            }
        }
    }

    pub fn step(&mut self, arm: &mut Arm7TDMI, buffer: &mut FrameBuffer) -> StepEvent {
        let mut flags = IrqFlags::empty();

        let start_cycle = self.cycles;

        // If a DMA engine did any work, the CPU does no work for this iteration
        if let Some(flag) = step_dma_units(self) {
            flags |= flag;
        } else {
            let event = arm.step(self);
            if event != StepEvent::None {
                return event;
            }
        }

        let current_cycle = self.cycles;
        flags |= self.renderer.step_cycles(current_cycle - start_cycle, buffer);
        flags |= step_timers(self, current_cycle);

        if flags.contains(IrqFlags::LCD_VBLANK) {
            dma_on_vblank(self);
        }

        if flags.contains(IrqFlags::LCD_HBLANK) {
            dma_on_hblank(self);
        }

        let masked = flags & self.interrupt_enable;
        if self.master_interrupt_enable && !masked.is_empty() {
            self.interrupt_flags.insert(masked);
            arm.signal_irq(self);
        }

        StepEvent::None
    }

    pub fn add_internal_cycles(&mut self, cycles: i64) {
        // TODO: Do GamePak prefetch buffer
        self.cycles += cycles;
    }

    pub fn debug_read8(&self, addr: u32) -> (Cycle, u8) {
        match addr >> 24 {
            BASE_ROM if addr < ROM_SIZE => {
                let read = if self.pc_inside_bios {
                    self.rom.read8(addr)
                } else {
                    warn!(BIOS, "Tried to read bios at 0x{:07X} while executing outside it", addr);
                    0
                };
                (ROM_TIMING_U8, read)
            }
            BASE_EWRAM => (EWRAM_TIMING_U8, self.ewram.read8(addr & EWRAM_MASK)),
            BASE_IWRAM => (IWRAM_TIMING_U8, self.iwram.read8(addr & IWRAM_MASK)),
            BASE_IOREGS => (IO_TIMING_U8, iomap::read8(self, addr)),
            BASE_PALETTE => self.renderer.palette_read8(addr),
            BASE_VRAM => self.renderer.vram_read8(addr),
            BASE_OAM => self.renderer.oam_read8(addr),
            BASE_GAMEPAK_START...BASE_GAMEPAK_END => {
                self.gamepak.read8(addr)
            }

            _ => unhandled_read!(self, addr)
        }
    }

    pub fn debug_read16(&self, addr: u32) -> (Cycle, u16) {
        assert!(addr & 1 == 0, "Unaligned halfword read at 0x{:08X}", addr);

        match addr >> 24 {
            BASE_ROM if addr < ROM_SIZE => {
                let read = if self.pc_inside_bios {
                    self.rom.read16(addr)
                } else {
                    warn!(BIOS, "Tried to read bios at 0x{:07X} while executing outside it", addr);
                    0
                };
                (ROM_TIMING_U16, read)
            }
            BASE_EWRAM => (EWRAM_TIMING_U16, self.ewram.read16(addr & EWRAM_MASK)),
            BASE_IWRAM => (IWRAM_TIMING_U16, self.iwram.read16(addr & IWRAM_MASK)),
            BASE_IOREGS => (IO_TIMING_U16, iomap::read16(self, addr)),
            BASE_PALETTE => self.renderer.palette_read16(addr),
            BASE_VRAM => self.renderer.vram_read16(addr),
            BASE_OAM => self.renderer.oam_read16(addr),
            BASE_GAMEPAK_START...BASE_GAMEPAK_END => {
                self.gamepak.read16(addr)
            }

            _ => unhandled_read!(self, addr)
        }
    }

    pub fn debug_read32(&self, addr: u32) -> (Cycle, u32) {
        assert!(addr & 3 == 0, "Unaligned word read at 0x{:08X}", addr);

        match addr >> 24 {
            BASE_ROM if addr < ROM_SIZE => {
                let read = if self.pc_inside_bios {
                    self.rom.read32(addr)
                } else {
                    warn!(BIOS, "Tried to read bios at 0x{:07X} while executing outside it", addr);
                    0
                };
                (ROM_TIMING_U32, read)
            }
            BASE_EWRAM => (EWRAM_TIMING_U32, self.ewram.read32(addr & EWRAM_MASK)),
            BASE_IWRAM => (IWRAM_TIMING_U32, self.iwram.read32(addr & IWRAM_MASK)),
            BASE_IOREGS => (IO_TIMING_U32, iomap::read32(self, addr)),
            BASE_PALETTE => self.renderer.palette_read32(addr),
            BASE_VRAM => self.renderer.vram_read32(addr),
            BASE_OAM => self.renderer.oam_read32(addr),
            BASE_GAMEPAK_START...BASE_GAMEPAK_END => {
                self.gamepak.read32(addr)
            }

            _ => unhandled_read!(self, addr)
        }
    }

    pub fn debug_write8(&mut self, addr: u32, value: u8) -> Cycle {
        match addr >> 24 {
            BASE_EWRAM => { // ewram
                self.ewram.write8(addr & EWRAM_MASK, value);
                EWRAM_TIMING_U8
            }
            BASE_IWRAM => { // iwram
                self.iwram.write8(addr & IWRAM_MASK, value);
                IWRAM_TIMING_U8
            }
            BASE_IOREGS => { // io registers
                iomap::write8(self, addr, value);
                IO_TIMING_U8
            }
            BASE_PALETTE => self.renderer.palette_write8(addr, value),
            BASE_VRAM => self.renderer.vram_write8(addr, value),
            BASE_OAM => self.renderer.oam_write8(addr, value),

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn debug_write16(&mut self, addr: u32, value: u16) -> Cycle {
        assert!(addr & 1 == 0, "Unaligned halfword write at 0x{:08X} of 0x{:08X}", addr, value);

        match addr >> 24 {
            BASE_EWRAM => { // ewram
                self.ewram.write16(addr & EWRAM_MASK, value);
                EWRAM_TIMING_U16
            }
            BASE_IWRAM => { // iwram
                self.iwram.write16(addr & IWRAM_MASK, value);
                IWRAM_TIMING_U16
            }
            BASE_IOREGS => { // io registers
                iomap::write16(self, addr, value);
                IO_TIMING_U16
            }
            BASE_PALETTE => self.renderer.palette_write16(addr, value),
            BASE_VRAM => self.renderer.vram_write16(addr, value),
            BASE_OAM => self.renderer.oam_write16(addr, value),

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn debug_write32(&mut self, addr: u32, value: u32) -> Cycle {
        assert!(addr & 3 == 0, "Unaligned word write at 0x{:08X} of 0x{:08X}", addr, value);

        match addr >> 24 {
            BASE_EWRAM => { // ewram
                self.ewram.write32(addr & EWRAM_MASK, value);
                EWRAM_TIMING_U32
            }
            BASE_IWRAM => { // iwram
                self.iwram.write32(addr & IWRAM_MASK, value);
                IWRAM_TIMING_U32
            }
            BASE_IOREGS => { // io registers
                iomap::write32(self, addr, value);
                IO_TIMING_U32
            }
            BASE_PALETTE => self.renderer.palette_write32(addr, value),
            BASE_VRAM => self.renderer.vram_write32(addr, value),
            BASE_OAM => self.renderer.oam_write32(addr, value),

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn read8(&mut self, addr: u32) -> u8 {
        let (cycle, read) = self.debug_read8(addr);
        self.cycles += cycle;
        read
    }
    pub fn read16(&mut self, addr: u32) -> u16 {
        let (cycle, read) = self.debug_read16(addr);
        self.cycles += cycle;
        read
    }
    pub fn read32(&mut self, addr: u32) -> u32 {
        let (cycle, read) = self.debug_read32(addr);
        self.cycles += cycle;
        read
    }
    pub fn read_ext_i8(&mut self, addr: u32) -> u32 {
        self.read8(addr) as i8 as u32
    }
    pub fn read_ext_i16(&mut self, addr: u32) -> u32 {
        self.read16(addr) as i16 as u32
    }
    pub fn read_ext_u8(&mut self, addr: u32) -> u32 {
        self.read8(addr) as u32
    }
    pub fn read_ext_u16(&mut self, addr: u32) -> u32 {
        self.read16(addr) as u32
    }

    pub fn write8(&mut self, addr: u32, value: u8) {
        let cycle = self.debug_write8(addr, value);
        self.cycles += cycle;
    }
    pub fn write16(&mut self, addr: u32, value: u16) {
        let cycle = self.debug_write16(addr, value);
        self.cycles += cycle;
    }
    pub fn write32(&mut self, addr: u32, value: u32) {
        let cycle = self.debug_write32(addr, value);
        self.cycles += cycle;
    }

    pub fn exec_thumb_slow(&mut self, addr: u32) -> u16 {
        self.pc_inside_bios = addr >> 24 == 0;
        self.read16(addr)
    }

    pub fn exec_arm_slow(&mut self, addr: u32) -> u32 {
        self.pc_inside_bios = addr >> 24 == 0;
        self.read32(addr)
    }
}

bitflags! {
pub struct IrqFlags: u16 {
    const LCD_VBLANK = 1 << 0;
    const LCD_HBLANK = 1 << 1;
    const LCD_VCOUNTER_MATCH = 1 << 2;
    const TIMER0_OVERFLOW = 1 << 3;
    const TIMER1_OVERFLOW = 1 << 4;
    const TIMER2_OVERFLOW = 1 << 5;
    const TIMER3_OVERFLOW = 1 << 6;
    const SERIAL_COMM = 1 << 7;
    const DMA0 = 1 << 8;
    const DMA1 = 1 << 9;
    const DMA2 = 1 << 10;
    const DMA3 = 1 << 11;
    const KEYPAD = 1 << 12;
    const GAME_PAK = 1 << 13;
}
}

unpacked_bitfield_struct! {
#[derive(Clone, Copy)]
pub struct WaitStateControlReg: u16 {
    (0, 2) sram_wait_control: usize,
    (2, 2) wait_state_0_non_seq: usize,
    (4, 1) wait_state_0_seq: usize,
    (5, 2) wait_state_1_non_seq: usize,
    (7, 1) wait_state_1_seq: usize,
    (8, 2) wait_state_2_non_seq: usize,
    (10,1) wait_state_2_seq: usize,
    (11,2) phi_terminal_output: usize,
    (14,1) gamepak_prefetch_buffer: bool,
    (15,1) gamepak_type_flag: bool,
}

#[derive(Clone, Copy)]
pub struct SoundPWMControlReg: u16 {
    (0,10) bias_level: u16,
    (14,2) amplitude_resolution: u8,
}
}


