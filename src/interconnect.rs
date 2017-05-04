use utils::{ Buffer, Cycle, sign_extend };
use renderer::{ Renderer, FrameBuffer };
use dma::{ Dma, DmaUnit, step_dma_units, dma_on_vblank, dma_on_hblank };
use timer::{ Timer, TimerUnit, step_timers };
use gamepak::GamePak;
use log::*;
use log;
use arm7tdmi::{ Arm7TDMI, StepEvent };

const ROM_START: u32 = 0x0000_0000;
const ROM_SIZE: u32 = 0x4000;
const ROM_MASK: u32 = ROM_SIZE - 1;
const ROM_TIMING_U8: Cycle = Cycle(1);
const ROM_TIMING_U16: Cycle = Cycle(1);
const ROM_TIMING_U32: Cycle = Cycle(1);

const EWRAM_START: u32 = 0x0200_0000;
const EWRAM_SIZE: u32 = 256 * 1024;
const EWRAM_MASK: u32 = EWRAM_SIZE - 1;
const EWRAM_TIMING_U8: Cycle = Cycle(3);
const EWRAM_TIMING_U16: Cycle = Cycle(3);
const EWRAM_TIMING_U32: Cycle = Cycle(6);

const IWRAM_START: u32 = 0x0300_0000;
const IWRAM_SIZE: u32 = 32 * 1024;
const IWRAM_MASK: u32 = IWRAM_SIZE - 1;
const IWRAM_TIMING_U8: Cycle = Cycle(1);
const IWRAM_TIMING_U16: Cycle = Cycle(1);
const IWRAM_TIMING_U32: Cycle = Cycle(1);

const IO_TIMING_U8: Cycle = Cycle(1);
const IO_TIMING_U16: Cycle = Cycle(1);
const IO_TIMING_U32: Cycle = Cycle(1);

macro_rules! unhandled_read {
    ($addr:expr) => {
        error!(IO, "Unhandled read at 0x{:04x}_{:04x}", $addr >> 16, $addr & 0xFFFF)
    }
}

macro_rules! unhandled_write {
    ($addr:expr, $value:expr) => {
        error!(IO, "Unhandled write at 0x{:04x}_{:04x} of {:X}", $addr >> 16, $addr & 0xFFFF, $value)
    }
}

#[derive(Clone)]
pub struct Interconnect {
    pub prefetch: [u32; 2],

    pc_inside_bios: bool,
    pub cycles: Cycle,

    pub timers: [Timer; 4],
    pub dma: [Dma; 4],
    renderer: Renderer,

    rom: Buffer,
    ewram: Buffer,
    iwram: Buffer,
    io_cache: Buffer,
    gamepak: GamePak,

    sram_wait_control: Cycle,
    gamepak_prefetch_buffer: bool,

    post_boot_flag: bool,
    master_interrupt_enable: bool,
    interrupt_enable: IrqFlags,
    interrupt_flags: IrqFlags,
    sound_bias: SoundPWMControlReg,
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

        if flags.contains(LCD_VBLANK) {
            dma_on_vblank(self);
        }

        if flags.contains(LCD_HBLANK) {
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
            0x0 | 0x1 => { // rom
                // TODO: Handle out of bounds, currently we panic
                let read = if self.pc_inside_bios {
                    self.rom.read8(addr)
                } else {
                    0
                };
                (ROM_TIMING_U8, read)
            }
            0x2 => { // ewram
                (EWRAM_TIMING_U8, self.ewram.read8((addr - EWRAM_START) & EWRAM_MASK))
            }
            0x3 => { // iwram
                (IWRAM_TIMING_U8, self.iwram.read8((addr - IWRAM_START) & IWRAM_MASK))
            }
            0x4 => { // io registers
                (IO_TIMING_U8, self.ioread8(addr))
            }
            0x5 => { // palette
                self.renderer.palette_read8(addr)
            }
            0x6 => { // vram
                self.renderer.vram_read8(addr)
            }
            0x7 => { // oam
                self.renderer.oam_read8(addr)
            }
            0x8...0xD => { // GamePak ROM
                self.gamepak.read8(addr)
            }

            _ => unhandled_read!(addr)
        }
    }

    pub fn debug_read16(&self, addr: u32) -> (Cycle, u16) {
        assert!(addr & 1 == 0, "Unaligned halfword read at 0x{:08X}", addr);

        match addr >> 24 {
            0x0 | 0x1 => { // rom
                // TODO: Handle out of bounds, currently we panic
                let read = if self.pc_inside_bios {
                    self.rom.read16(addr)
                } else {
                    0
                };
                (ROM_TIMING_U16, read)
            }
            0x2 => { // ewram
                (EWRAM_TIMING_U16, self.ewram.read16(addr & EWRAM_MASK))
            }
            0x3 => { // iwram
                (IWRAM_TIMING_U16, self.iwram.read16(addr & IWRAM_MASK))
            }
            0x4 => { // io registers
                (IO_TIMING_U16, self.ioread16(addr))
            }
            0x5 => { // palette
                self.renderer.palette_read16(addr)
            }
            0x6 => { // vram
                self.renderer.vram_read16(addr)
            }
            0x7 => { // oam
                self.renderer.oam_read16(addr)
            }
            0x8...0xD => { // GamePak ROM
                self.gamepak.read16(addr)
            }

            _ => unhandled_read!(addr)
        }
    }

    pub fn debug_read32(&self, addr: u32) -> (Cycle, u32) {
        assert!(addr & 3 == 0, "Unaligned word read at 0x{:08X}", addr);

        match addr >> 24 {
            0x0 | 0x1 => { // rom
                // TODO: Handle out of bounds, currently we panic
                let read = if self.pc_inside_bios {
                    self.rom.read32(addr)
                } else {
                    0
                };
                (ROM_TIMING_U32, read)
            }
            0x2 => { // ewram
                (EWRAM_TIMING_U32, self.ewram.read32(addr & EWRAM_MASK))
            }
            0x3 => { // iwram
                (IWRAM_TIMING_U32, self.iwram.read32(addr & IWRAM_MASK))
            }
            0x4 => { // io registers
                (IO_TIMING_U32, self.ioread32(addr))
            }
            0x5 => { // palette
                self.renderer.palette_read32(addr)
            }
            0x6 => { // vram
                self.renderer.vram_read32(addr)
            }
            0x7 => { // oam
                self.renderer.oam_read32(addr)
            }
            0x8...0xD => { // GamePak ROM
                self.gamepak.read32(addr)
            }

            _ => unhandled_read!(addr)
        }
    }

    pub fn debug_write8(&mut self, addr: u32, value: u8) -> Cycle {
        match addr >> 24 {
            0x2 => { // ewram
                self.ewram.write8((addr - EWRAM_START) & EWRAM_MASK, value);
                EWRAM_TIMING_U8
            }
            0x3 => { // iwram
                self.iwram.write8((addr - IWRAM_START) & IWRAM_MASK, value);
                IWRAM_TIMING_U8
            }
            0x4 => { // io registers
                self.iowrite8(addr, value);
                IO_TIMING_U8
            }
            0x5 => { // palette
                self.renderer.palette_write8(addr, value)
            }
            0x6 => { // vram
                self.renderer.vram_write8(addr, value)
            }
            0x7 => { // oam
                self.renderer.oam_write8(addr, value)
            }

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn debug_write16(&mut self, addr: u32, value: u16) -> Cycle {
        assert!(addr & 1 == 0, "Unaligned halfword write at 0x{:08X} of 0x{:08X}", addr, value);

        match addr >> 24 {
            0x2 => { // ewram
                self.ewram.write16((addr - EWRAM_START) & EWRAM_MASK, value);
                EWRAM_TIMING_U16
            }
            0x3 => { // iwram
                self.iwram.write16((addr - IWRAM_START) & IWRAM_MASK, value);
                IWRAM_TIMING_U16
            }
            0x4 => { // io registers
                self.iowrite16(addr, value);
                IO_TIMING_U16
            }
            0x5 => { // palette
                self.renderer.palette_write16(addr, value)
            }
            0x6 => { // vram
                self.renderer.vram_write16(addr, value)
            }
            0x7 => { // oam
                self.renderer.oam_write16(addr, value)
            }

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn debug_write32(&mut self, addr: u32, value: u32) -> Cycle {
        assert!(addr & 3 == 0, "Unaligned word write at 0x{:08X} of 0x{:08X}", addr, value);

        match addr >> 24 {
            0x2 => { // ewram
                self.ewram.write32((addr - EWRAM_START) & EWRAM_MASK, value);
                EWRAM_TIMING_U32
            }
            0x3 => { // iwram
                self.iwram.write32((addr - IWRAM_START) & IWRAM_MASK, value);
                IWRAM_TIMING_U32
            }
            0x4 => { // io registers
                self.iowrite32(addr, value);
                IO_TIMING_U32
            }
            0x5 => { // palette
                self.renderer.palette_write32(addr, value)
            }
            0x6 => { // vram
                self.renderer.vram_write32(addr, value)
            }
            0x7 => { // oam
                self.renderer.oam_write32(addr, value)
            }

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

macro_rules! unreadable {
    ($kind:expr, $reg:expr) => {
        |_ic: &Interconnect| {
            let reg_name = stringify!($reg);
            warn!($kind, "Tried to read the write-only {} (at 0x{:X}).", reg_name, $reg);
            0
        }
    }
}

macro_rules! unwriteable {
    ($kind:expr, $reg:expr) => {
        |_ic: &mut Interconnect, value| {
            let reg_name = stringify!($reg);
            warn!($kind, "Tried to write {:X} to read-only {} (at 0x{:X})", value, reg_name, $reg);
        }
    }
}

macro_rules! read_stub {
    () => {
        |_ic: &Interconnect| {
            print!("(STUB) ");
            0
        }
    }
}

macro_rules! write_stub {
    () => {
        |_ic: &mut Interconnect, _value| {
            print!("(STUB) ");
        }
    }
}

impl_io_map! {
    [Interconnect]
    (u16, REG_DISPCNT) {
        read => |ic: &Interconnect| {
            ic.renderer.read_dispcnt()
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.renderer.write_dispcnt(value);
        }
    }

    (u16, REG_DISPSTAT) {
        read => |ic: &Interconnect| {
            ic.renderer.read_dispstat()
        },
        write => |ic: &mut Interconnect, value| {
            ic.renderer.write_dispstat(value);
        }
    }

    (u32, REG_SOUNDBIAS) {
        read => |ic: &Interconnect| {
            ic.sound_bias.into()
        },
        write => |ic: &mut Interconnect, value: u32| {
            ic.sound_bias = SoundPWMControlReg::from(value);
        }
    }

    (u16, REG_IE) {
        read => |ic: &Interconnect| {
            ic.interrupt_enable.bits()
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.interrupt_enable = IrqFlags::from_bits_truncate(value);
            trace!(CPU, "Setting interrupt_enable = {:?}", ic.interrupt_enable);
        }
    }

    (u16, REG_IF) {
        read => |ic: &Interconnect| {
            ic.interrupt_flags.bits()
        },
        write => |ic: &mut Interconnect, value: u16| {
            let value = IrqFlags::from_bits_truncate(value);
            ic.interrupt_flags.remove(value);
            trace!(CPU, "Setting interrupt_flags = {:?}", ic.interrupt_flags);
        }
    }

    (u32, REG_WAITCNT) {
        read => |_ic: &Interconnect| {
            warn!(IO, "Reading stubbed WAITCNT");
            0
        },
        write => |ic: &mut Interconnect, value: u32| {
            let ctrl = WaitStateControlReg::from(value);
            ic.sram_wait_control = Cycle([4, 3, 2, 8][ctrl.sram_wait_control]);
            ic.gamepak.wait_states[0].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_0_non_seq]);
            ic.gamepak.wait_states[0].seq = Cycle([2, 1][ctrl.wait_state_0_seq]);
            ic.gamepak.wait_states[1].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_1_non_seq]);
            ic.gamepak.wait_states[1].seq = Cycle([4, 1][ctrl.wait_state_1_seq]);
            ic.gamepak.wait_states[2].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_2_non_seq]);
            ic.gamepak.wait_states[2].seq = Cycle([8, 1][ctrl.wait_state_2_seq]);
            ic.gamepak_prefetch_buffer = ctrl.gamepak_prefetch_buffer;
        }
    }

    (u32, REG_IME) {
        read => |ic: &Interconnect| {
            ic.master_interrupt_enable as u32
        },
        write => |ic: &mut Interconnect, value| {
            trace!(CPU, "Setting master_interrupt_enable = {}", ic.master_interrupt_enable);
            ic.master_interrupt_enable = (value & 1) != 0;
        }
    }

    (u8, REG_POSTFLG) {
        read => |ic: &Interconnect| {
            ic.post_boot_flag as u8
        },
        write => |ic: &mut Interconnect, value| {
            ic.post_boot_flag = value != 0;
        }
    }

    (u16, REG_VCOUNT) {
        read => |ic: &Interconnect| {
            ic.renderer.scanline as u16
        },
        write => unwriteable!(GPU, REG_VCOUNT)
    }

    (u16, REG_BG0CNT ) { // 2    R/W   BG0 Control
        read => |ic: &Interconnect| {
            ic.renderer.read_bgcnt(0)
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.renderer.write_bgcnt(0, value)
        }
    }
    (u16, REG_BG1CNT ) { // 2    R/W   BG1 Control
        read => |ic: &Interconnect| {
            ic.renderer.read_bgcnt(1)
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.renderer.write_bgcnt(1, value)
        }
    }
    (u16, REG_BG2CNT ) { // 2    R/W   BG2 Control
        read => |ic: &Interconnect| {
            ic.renderer.read_bgcnt(2)
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.renderer.write_bgcnt(2, value)
        }
    }
    (u16, REG_BG3CNT ) { // 2    R/W   BG3 Control
        read => |ic: &Interconnect| {
            ic.renderer.read_bgcnt(3)
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.renderer.write_bgcnt(3, value)
        }
    }

    (u16, REG_BG0HOFS) { // 2    W     BG0 X-Offset
        read => unreadable!(GPU, REG_BG0HOFS),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[0].x_offset = value & 0x1FF
    }
    (u16, REG_BG0VOFS) { // 2    W     BG0 Y-Offset
        read => unreadable!(GPU, REG_BG0VOFS),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[0].y_offset = value & 0x1FF
    }
    (u16, REG_BG1HOFS) { // 2    W     BG1 X-Offset
        read => unreadable!(GPU, REG_BG1HOFS),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[1].x_offset = value & 0x1FF
    }
    (u16, REG_BG1VOFS) { // 2    W     BG1 Y-Offset
        read => unreadable!(GPU, REG_BG1VOFS),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[1].y_offset = value & 0x1FF
    }
    (u16, REG_BG2HOFS) { // 2    W     BG2 X-Offset
        read => unreadable!(GPU, REG_BG2HOFS),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[2].x_offset = value & 0x1FF
    }
    (u16, REG_BG2VOFS) { // 2    W     BG2 Y-Offset
        read => unreadable!(GPU, REG_BG2VOFS),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[2].y_offset = value & 0x1FF
    }
    (u16, REG_BG3HOFS) { // 2    W     BG3 X-Offset
        read => unreadable!(GPU, REG_BG3HOFS),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[3].x_offset = value & 0x1FF
    }
    (u16, REG_BG3VOFS) { // 2    W     BG3 Y-Offset
        read => unreadable!(GPU, REG_BG3VOFS),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[3].y_offset = value & 0x1FF
    }

    (u16, REG_BG2PA  ) { // 2    W     BG2 Rotation/Scaling Parameter A (dx)
        read => unreadable!(GPU, REG_BG2PA),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[2].dx = value
    }
    (u16, REG_BG2PB  ) { // 2    W     BG2 Rotation/Scaling Parameter B (dmx)
        read => unreadable!(GPU, REG_BG2PB),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[2].dmx = value
    }
    (u16, REG_BG2PC  ) { // 2    W     BG2 Rotation/Scaling Parameter C (dy)
        read => unreadable!(GPU, REG_BG2PC),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[2].dy = value
    }
    (u16, REG_BG2PD  ) { // 2    W     BG2 Rotation/Scaling Parameter D (dmy)
        read => unreadable!(GPU, REG_BG2PD),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[2].dmy = value
    }
    (u32, REG_BG2X   ) { // 4    W     BG2 Reference Point X-Coordinate
        read => unreadable!(GPU, REG_BG2X),
        write => |ic: &mut Interconnect, value| {
            ic.renderer.bg[2].x_ref = sign_extend(value, 28);
        }
    }
    (u32, REG_BG2Y   ) { // 4    W     BG2 Reference Point Y-Coordinate
        read => unreadable!(GPU, REG_BG2Y),
        write => |ic: &mut Interconnect, value| {
            ic.renderer.bg[2].y_ref = sign_extend(value, 28);
        }
    }

    (u16, REG_BG3PA  ) { // 2    W     BG3 Rotation/Scaling Parameter A (dx)
        read => unreadable!(GPU, REG_BG3PA),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[3].dx = value
    }
    (u16, REG_BG3PB  ) { // 2    W     BG3 Rotation/Scaling Parameter B (dmx)
        read => unreadable!(GPU, REG_BG3PB),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[3].dmx = value
    }
    (u16, REG_BG3PC  ) { // 2    W     BG3 Rotation/Scaling Parameter C (dy)
        read => unreadable!(GPU, REG_BG3PC),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[3].dy = value
    }
    (u16, REG_BG3PD  ) { // 2    W     BG3 Rotation/Scaling Parameter D (dmy)
        read => unreadable!(GPU, REG_BG3PD),
        write => |ic: &mut Interconnect, value| ic.renderer.bg[3].dmy = value
    }
    (u32, REG_BG3X   ) { // 4    W     BG3 Reference Point X-Coordinate
        read => unreadable!(GPU, REG_BG3X),
        write => |ic: &mut Interconnect, value| {
            ic.renderer.bg[3].x_ref = sign_extend(value, 28);
        }
    }
    (u32, REG_BG3Y   ) { // 4    W     BG3 Reference Point Y-Coordinate
        read => unreadable!(GPU, REG_BG3Y),
        write => |ic: &mut Interconnect, value| {
            ic.renderer.bg[3].y_ref = sign_extend(value, 28);
        }
    }

    (u16, REG_TM0CNT_L) {
        read => |ic: &Interconnect| {
            ic.timers[0].get_current_value(ic.cycles)
        },
        write => |ic: &mut Interconnect, value| {
            ic.timers[0].set_reload_value(value);
        }
    }
    (u16, REG_TM1CNT_L) {
        read => |ic: &Interconnect| {
            ic.timers[1].get_current_value(ic.cycles)
        },
        write => |ic: &mut Interconnect, value| {
            ic.timers[1].set_reload_value(value);
        }
    }
    (u16, REG_TM2CNT_L) {
        read => |ic: &Interconnect| {
            ic.timers[2].get_current_value(ic.cycles)
        },
        write => |ic: &mut Interconnect, value| {
            ic.timers[2].set_reload_value(value);
        }
    }
    (u16, REG_TM3CNT_L) {
        read => |ic: &Interconnect| {
            ic.timers[3].get_current_value(ic.cycles)
        },
        write => |ic: &mut Interconnect, value| {
            ic.timers[3].set_reload_value(value);
        }
    }

    (u16, REG_TM0CNT_H) {
        read => |ic: &Interconnect| {
            ic.timers[0].read_control()
        },
        write => |ic: &mut Interconnect, value| {
            ic.timers[0].write_control(ic.cycles, value);
        }
    }
    (u16, REG_TM1CNT_H) {
        read => |ic: &Interconnect| {
            ic.timers[1].read_control()
        },
        write => |ic: &mut Interconnect, value| {
            ic.timers[1].write_control(ic.cycles, value);
        }
    }
    (u16, REG_TM2CNT_H) {
        read => |ic: &Interconnect| {
            ic.timers[2].read_control()
        },
        write => |ic: &mut Interconnect, value| {
            ic.timers[2].write_control(ic.cycles, value);
        }
    }
    (u16, REG_TM3CNT_H) {
        read => |ic: &Interconnect| {
            ic.timers[3].read_control()
        },
        write => |ic: &mut Interconnect, value| {
            ic.timers[3].write_control(ic.cycles, value);
        }
    }

    (u32, REG_DMA0SAD) {
        read => unreadable!(log::DMA0, REG_DMA0SAD),
        write => |ic: &mut Interconnect, value| {
            ic.dma[0].write_source(value);
        }
    }
    (u32, REG_DMA1SAD) {
        read => unreadable!(log::DMA1, REG_DMA1SAD),
        write => |ic: &mut Interconnect, value| {
            ic.dma[1].write_source(value);
        }
    }
    (u32, REG_DMA2SAD) {
        read => unreadable!(log::DMA2, REG_DMA2SAD),
        write => |ic: &mut Interconnect, value| {
            ic.dma[2].write_source(value);
        }
    }
    (u32, REG_DMA3SAD) {
        read => unreadable!(log::DMA3, REG_DMA3SAD),
        write => |ic: &mut Interconnect, value| {
            ic.dma[3].write_source(value);
        }
    }

    (u32, REG_DMA0DAD) {
        read => unreadable!(log::DMA0, REG_DMA0DAD),
        write => |ic: &mut Interconnect, value| {
            ic.dma[0].write_dest(value);
        }
    }
    (u32, REG_DMA1DAD) {
        read => unreadable!(log::DMA1, REG_DMA1DAD),
        write => |ic: &mut Interconnect, value| {
            ic.dma[1].write_dest(value);
        }
    }
    (u32, REG_DMA2DAD) {
        read => unreadable!(log::DMA2, REG_DMA2DAD),
        write => |ic: &mut Interconnect, value| {
            ic.dma[2].write_dest(value);
        }
    }
    (u32, REG_DMA3DAD) {
        read => unreadable!(log::DMA3, REG_DMA3DAD),
        write => |ic: &mut Interconnect, value| {
            ic.dma[3].write_dest(value);
        }
    }

    (u16, REG_DMA0CNT_L) {
        read => unreadable!(log::DMA0, REG_DMA0CNT_L),
        write => |ic: &mut Interconnect, value| {
            ic.dma[0].write_word_count(value);
        }
    }
    (u16, REG_DMA1CNT_L) {
        read => unreadable!(log::DMA1, REG_DMA1CNT_L),
        write => |ic: &mut Interconnect, value| {
            ic.dma[1].write_word_count(value);
        }
    }
    (u16, REG_DMA2CNT_L) {
        read => unreadable!(log::DMA2, REG_DMA2CNT_L),
        write => |ic: &mut Interconnect, value| {
            ic.dma[2].write_word_count(value);
        }
    }
    (u16, REG_DMA3CNT_L) {
        read => unreadable!(log::DMA3, REG_DMA3CNT_L),
        write => |ic: &mut Interconnect, value| {
            ic.dma[3].write_word_count(value);
        }
    }

    (u16, REG_DMA0CNT_H) {
        read => |ic: &Interconnect| {
            ic.dma[0].read_control()
        },
        write => |ic: &mut Interconnect, value| {
            ic.dma[0].write_control(value);
        }
    }
    (u16, REG_DMA1CNT_H) {
        read => |ic: &Interconnect| {
            ic.dma[1].read_control()
        },
        write => |ic: &mut Interconnect, value| {
            ic.dma[1].write_control(value);
        }
    }
    (u16, REG_DMA2CNT_H) {
        read => |ic: &Interconnect| {
            ic.dma[2].read_control()
        },
        write => |ic: &mut Interconnect, value| {
            ic.dma[2].write_control(value);
        }
    }
    (u16, REG_DMA3CNT_H) {
        read => |ic: &Interconnect| {
            ic.dma[3].read_control()
        },
        write => |ic: &mut Interconnect, value| {
            ic.dma[3].write_control(value);
        }
    }

    (u16, REG_GREENSWAP   ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WIN0H       ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WIN1H       ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WIN0V       ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WIN1V       ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WININ       ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WINOUT      ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_MOSAIC      ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_BLDCNT      ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_BLDALPHA    ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_BLDY        ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND1CNT_L ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND1CNT_H ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND1CNT_X ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND2CNT_L ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND2CNT_H ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND3CNT_L ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND3CNT_H ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND3CNT_X ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND4CNT_L ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUND4CNT_H ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUNDCNT_L  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUNDCNT_H  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SOUNDCNT_X  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WAVE_RAM_0  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WAVE_RAM_1  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WAVE_RAM_2  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WAVE_RAM_3  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WAVE_RAM_4  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WAVE_RAM_5  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WAVE_RAM_6  ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_WAVE_RAM_7  ) { read => read_stub!(), write => write_stub!() }
    (u32, REG_FIFO_A      ) { read => read_stub!(), write => write_stub!() }
    (u32, REG_FIFO_B      ) { read => read_stub!(), write => write_stub!() }
    (u32, REG_SIODATA32   ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SIOMULTI0   ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SIOMULTI1   ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SIOMULTI2   ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SIOMULTI3   ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SIOCNT      ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SIOMLT_SEND ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_SIODATA8    ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_KEYINPUT    ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_KEYCNT      ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_RCNT        ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_IR          ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_JOYCNT      ) { read => read_stub!(), write => write_stub!() }
    (u32, REG_JOY_RECV    ) { read => read_stub!(), write => write_stub!() }
    (u32, REG_JOY_TRANS   ) { read => read_stub!(), write => write_stub!() }
    (u16, REG_JOYSTAT     ) { read => read_stub!(), write => write_stub!() }
    (u8,  REG_HALTCNT     ) { read => read_stub!(), write => write_stub!() }
}

bitflags! {
pub flags IrqFlags: u16 {
    const LCD_VBLANK = 1 << 0,
    const LCD_HBLANK = 1 << 1,
    const LCD_VCOUNTER_MATCH = 1 << 2,
    const TIMER0_OVERFLOW = 1 << 3,
    const TIMER1_OVERFLOW = 1 << 4,
    const TIMER2_OVERFLOW = 1 << 5,
    const TIMER3_OVERFLOW = 1 << 6,
    const SERIAL_COMM = 1 << 7,
    const DMA0 = 1 << 8,
    const DMA1 = 1 << 9,
    const DMA2 = 1 << 10,
    const DMA3 = 1 << 11,
    const KEYPAD = 1 << 12,
    const GAME_PAK = 1 << 13,
}
}

unpacked_bitfield_struct! {
#[derive(Clone, Copy)]
pub struct WaitStateControlReg: u32 {
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
pub struct SoundPWMControlReg: u32 {
    (0,10) bias_level: u16,
    (14,2) amplitude_resolution: u8,
}
}

const REG_DISPCNT: u32 = 0x4000000;     // 2    R/W   LCD Control
const REG_GREENSWAP: u32 = 0x4000002;   // 2    R/W   Undocumented - Green Swap
const REG_DISPSTAT: u32 = 0x4000004;    // 2    R/W   General LCD Status (STAT,LYC)
const REG_VCOUNT: u32 = 0x4000006;      // 2    R     Vertical Counter (LY)
const REG_BG0CNT: u32 = 0x4000008;      // 2    R/W   BG0 Control
const REG_BG1CNT: u32 = 0x400000A;      // 2    R/W   BG1 Control
const REG_BG2CNT: u32 = 0x400000C;      // 2    R/W   BG2 Control
const REG_BG3CNT: u32 = 0x400000E;      // 2    R/W   BG3 Control
const REG_BG0HOFS: u32 = 0x4000010;     // 2    W     BG0 X-Offset
const REG_BG0VOFS: u32 = 0x4000012;     // 2    W     BG0 Y-Offset
const REG_BG1HOFS: u32 = 0x4000014;     // 2    W     BG1 X-Offset
const REG_BG1VOFS: u32 = 0x4000016;     // 2    W     BG1 Y-Offset
const REG_BG2HOFS: u32 = 0x4000018;     // 2    W     BG2 X-Offset
const REG_BG2VOFS: u32 = 0x400001A;     // 2    W     BG2 Y-Offset
const REG_BG3HOFS: u32 = 0x400001C;     // 2    W     BG3 X-Offset
const REG_BG3VOFS: u32 = 0x400001E;     // 2    W     BG3 Y-Offset
const REG_BG2PA: u32 = 0x4000020;       // 2    W     BG2 Rotation/Scaling Parameter A (dx)
const REG_BG2PB: u32 = 0x4000022;       // 2    W     BG2 Rotation/Scaling Parameter B (dmx)
const REG_BG2PC: u32 = 0x4000024;       // 2    W     BG2 Rotation/Scaling Parameter C (dy)
const REG_BG2PD: u32 = 0x4000026;       // 2    W     BG2 Rotation/Scaling Parameter D (dmy)
const REG_BG2X: u32 = 0x4000028;        // 4    W     BG2 Reference Point X-Coordinate
const REG_BG2Y: u32 = 0x400002C;        // 4    W     BG2 Reference Point Y-Coordinate
const REG_BG3PA: u32 = 0x4000030;       // 2    W     BG3 Rotation/Scaling Parameter A (dx)
const REG_BG3PB: u32 = 0x4000032;       // 2    W     BG3 Rotation/Scaling Parameter B (dmx)
const REG_BG3PC: u32 = 0x4000034;       // 2    W     BG3 Rotation/Scaling Parameter C (dy)
const REG_BG3PD: u32 = 0x4000036;       // 2    W     BG3 Rotation/Scaling Parameter D (dmy)
const REG_BG3X: u32 = 0x4000038;        // 4    W     BG3 Reference Point X-Coordinate
const REG_BG3Y: u32 = 0x400003C;        // 4    W     BG3 Reference Point Y-Coordinate
const REG_WIN0H: u32 = 0x4000040;       // 2    W     Window 0 Horizontal Dimensions
const REG_WIN1H: u32 = 0x4000042;       // 2    W     Window 1 Horizontal Dimensions
const REG_WIN0V: u32 = 0x4000044;       // 2    W     Window 0 Vertical Dimensions
const REG_WIN1V: u32 = 0x4000046;       // 2    W     Window 1 Vertical Dimensions
const REG_WININ: u32 = 0x4000048;       // 2    R/W   Inside of Window 0 and 1
const REG_WINOUT: u32 = 0x400004A;      // 2    R/W   Inside of OBJ Window & Outside of Windows
const REG_MOSAIC: u32 = 0x400004C;      // 2    W     Mosaic Size
const REG_BLDCNT: u32 = 0x4000050;      // 2    R/W   Color Special Effects Selection
const REG_BLDALPHA: u32 = 0x4000052;    // 2    W     Alpha Blending Coefficients
const REG_BLDY: u32 = 0x4000054;        // 2    W     Brightness (Fade-In/Out) Coefficient
const REG_SOUND1CNT_L: u32 = 0x4000060; // 2    R/W   Channel 1 Sweep register       (NR10)
const REG_SOUND1CNT_H: u32 = 0x4000062; // 2    R/W   Channel 1 Duty/Length/Envelope (NR11, NR12)
const REG_SOUND1CNT_X: u32 = 0x4000064; // 2    R/W   Channel 1 Frequency/Control    (NR13, NR14)
const REG_SOUND2CNT_L: u32 = 0x4000068; // 2    R/W   Channel 2 Duty/Length/Envelope (NR21, NR22)
const REG_SOUND2CNT_H: u32 = 0x400006C; // 2    R/W   Channel 2 Frequency/Control    (NR23, NR24)
const REG_SOUND3CNT_L: u32 = 0x4000070; // 2    R/W   Channel 3 Stop/Wave RAM select (NR30)
const REG_SOUND3CNT_H: u32 = 0x4000072; // 2    R/W   Channel 3 Length/Volume        (NR31, NR32)
const REG_SOUND3CNT_X: u32 = 0x4000074; // 2    R/W   Channel 3 Frequency/Control    (NR33, NR34)
const REG_SOUND4CNT_L: u32 = 0x4000078; // 2    R/W   Channel 4 Length/Envelope      (NR41, NR42)
const REG_SOUND4CNT_H: u32 = 0x400007C; // 2    R/W   Channel 4 Frequency/Control    (NR43, NR44)
const REG_SOUNDCNT_L: u32 = 0x4000080;  // 2    R/W   Control Stereo/Volume/Enable   (NR50, NR51)
const REG_SOUNDCNT_H: u32 = 0x4000082;  // 2    R/W   Control Mixing/DMA Control
const REG_SOUNDCNT_X: u32 = 0x4000084;  // 2    R/W   Control Sound on/off           (NR52)
const REG_SOUNDBIAS: u32 = 0x4000088;   // 2    BIOS  Sound PWM Control
// 2x16 R/W   Channel 3 Wave Pattern RAM (2 banks!!)
const REG_WAVE_RAM_0: u32 = 0x4000090;
const REG_WAVE_RAM_1: u32 = 0x4000092;
const REG_WAVE_RAM_2: u32 = 0x4000094;
const REG_WAVE_RAM_3: u32 = 0x4000096;
const REG_WAVE_RAM_4: u32 = 0x4000098;
const REG_WAVE_RAM_5: u32 = 0x400009A;
const REG_WAVE_RAM_6: u32 = 0x400009C;
const REG_WAVE_RAM_7: u32 = 0x400009E;
const REG_FIFO_A: u32 = 0x40000A0;      // 4    W     Channel A FIFO, Data 0-3
const REG_FIFO_B: u32 = 0x40000A4;      // 4    W     Channel B FIFO, Data 0-3
const REG_DMA0SAD: u32 = 0x40000B0;     // 4    W     DMA 0 Source Address
const REG_DMA0DAD: u32 = 0x40000B4;     // 4    W     DMA 0 Destination Address
const REG_DMA0CNT_L: u32 = 0x40000B8;   // 2    W     DMA 0 Word Count
const REG_DMA0CNT_H: u32 = 0x40000BA;   // 2    R/W   DMA 0 Control
const REG_DMA1SAD: u32 = 0x40000BC;     // 4    W     DMA 1 Source Address
const REG_DMA1DAD: u32 = 0x40000C0;     // 4    W     DMA 1 Destination Address
const REG_DMA1CNT_L: u32 = 0x40000C4;   // 2    W     DMA 1 Word Count
const REG_DMA1CNT_H: u32 = 0x40000C6;   // 2    R/W   DMA 1 Control
const REG_DMA2SAD: u32 = 0x40000C8;     // 4    W     DMA 2 Source Address
const REG_DMA2DAD: u32 = 0x40000CC;     // 4    W     DMA 2 Destination Address
const REG_DMA2CNT_L: u32 = 0x40000D0;   // 2    W     DMA 2 Word Count
const REG_DMA2CNT_H: u32 = 0x40000D2;   // 2    R/W   DMA 2 Control
const REG_DMA3SAD: u32 = 0x40000D4;     // 4    W     DMA 3 Source Address
const REG_DMA3DAD: u32 = 0x40000D8;     // 4    W     DMA 3 Destination Address
const REG_DMA3CNT_L: u32 = 0x40000DC;   // 2    W     DMA 3 Word Count
const REG_DMA3CNT_H: u32 = 0x40000DE;   // 2    R/W   DMA 3 Control
const REG_TM0CNT_L: u32 = 0x4000100;    // 2    R/W   Timer 0 Counter/Reload
const REG_TM0CNT_H: u32 = 0x4000102;    // 2    R/W   Timer 0 Control
const REG_TM1CNT_L: u32 = 0x4000104;    // 2    R/W   Timer 1 Counter/Reload
const REG_TM1CNT_H: u32 = 0x4000106;    // 2    R/W   Timer 1 Control
const REG_TM2CNT_L: u32 = 0x4000108;    // 2    R/W   Timer 2 Counter/Reload
const REG_TM2CNT_H: u32 = 0x400010A;    // 2    R/W   Timer 2 Control
const REG_TM3CNT_L: u32 = 0x400010C;    // 2    R/W   Timer 3 Counter/Reload
const REG_TM3CNT_H: u32 = 0x400010E;    // 2    R/W   Timer 3 Control
const REG_SIODATA32: u32 = 0x4000120;   // 4    R/W   SIO Data (Normal-32bit Mode; shared with below)
const REG_SIOMULTI0: u32 = 0x4000120;   // 2    R/W   SIO Data 0 (Parent)    (Multi-Player Mode)
const REG_SIOMULTI1: u32 = 0x4000122;   // 2    R/W   SIO Data 1 (1st Child) (Multi-Player Mode)
const REG_SIOMULTI2: u32 = 0x4000124;   // 2    R/W   SIO Data 2 (2nd Child) (Multi-Player Mode)
const REG_SIOMULTI3: u32 = 0x4000126;   // 2    R/W   SIO Data 3 (3rd Child) (Multi-Player Mode)
const REG_SIOCNT: u32 = 0x4000128;      // 2    R/W   SIO Control Register
const REG_SIOMLT_SEND: u32 = 0x400012A; // 2    R/W   SIO Data (Local of MultiPlayer; shared below)
const REG_SIODATA8: u32 = 0x400012A;    // 2    R/W   SIO Data (Normal-8bit and UART Mode)
const REG_KEYINPUT: u32 = 0x4000130;    // 2    R     Key Status
const REG_KEYCNT: u32 = 0x4000132;      // 2    R/W   Key Interrupt Control
const REG_RCNT: u32 = 0x4000134;        // 2    R/W   SIO Mode Select/General Purpose Data
const REG_IR: u32 = 0x4000136;          // -    -     Ancient - Infrared Register (Prototypes only)
const REG_JOYCNT: u32 = 0x4000140;      // 2    R/W   SIO JOY Bus Control
const REG_JOY_RECV: u32 = 0x4000150;    // 4    R/W   SIO JOY Bus Receive Data
const REG_JOY_TRANS: u32 = 0x4000154;   // 4    R/W   SIO JOY Bus Transmit Data
const REG_JOYSTAT: u32 = 0x4000158;     // 2    R/?   SIO JOY Bus Receive Status
const REG_IE: u32 = 0x4000200;          // 2    R/W   Interrupt Enable Register
const REG_IF: u32 = 0x4000202;          // 2    R/W   Interrupt Request Flags / IRQ Acknowledge
const REG_WAITCNT: u32 = 0x4000204;     // 2    R/W   Game Pak Waitstate Control
const REG_IME: u32 = 0x4000208;         // 2    R/W   Interrupt Master Enable Register
const REG_POSTFLG: u32 = 0x4000300;     // 1    R/W   Undocumented - Post Boot Flag
const REG_HALTCNT: u32 = 0x4000301;     // 1    W     Undocumented - Power Down Control
