use std::ptr;

use utils::{ Buffer, Cycle };

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

const VRAM_START: u32 = 0x0600_0000;
const VRAM_SIZE: u32 = 96 * 1024;
fn vram_mask(mut addr: u32) -> u32 {
    addr = addr & ((128 * 1024) - 1);
    if addr >= (96 * 1024) {
        addr -= 32 * 1024;
    }
    addr
}
const VRAM_TIMING_U8: Cycle = Cycle(1);
const VRAM_TIMING_U16: Cycle = Cycle(1);
const VRAM_TIMING_U32: Cycle = Cycle(2);

const IO_TIMING_U8: Cycle = Cycle(1);
const IO_TIMING_U16: Cycle = Cycle(1);
const IO_TIMING_U32: Cycle = Cycle(1);

const OAM_SIZE: u32 = 1024;
const OAM_MASK: u32 = OAM_SIZE - 1;
const OAM_TIMING_U8: Cycle = Cycle(1);
const OAM_TIMING_U16: Cycle = Cycle(1);
const OAM_TIMING_U32: Cycle = Cycle(1);

const PALETTE_SIZE: u32 = 1024;
const PALETTE_MASK: u32 = PALETTE_SIZE - 1;
const PALETTE_TIMING_U8: Cycle = Cycle(1);
const PALETTE_TIMING_U16: Cycle = Cycle(1);
const PALETTE_TIMING_U32: Cycle = Cycle(2);

const GAMEPAK_PAGE_SIZE: u32 = 128*1024;
const GAMEPAK_PAGE_MASK: u32 = GAMEPAK_PAGE_SIZE - 1;
const GAMEPAK_MAX_SIZE: u32 = 32 * 1024 * 1024;
const GAMEPAK_MAX_SIZE_MASK: u32 = GAMEPAK_MAX_SIZE - 1;

const CODE_SEGMENT_BASE_INVALID: u32 = 0x10_0000;
const CODE_SEGMENT_SIZE_INVALID: u32 = 0;

macro_rules! unhandled_read {
    ($addr:expr) => {
        panic!("Unhandled read at 0x{:04x}_{:04x}", $addr >> 16, $addr & 0xFFFF)
    }
}

macro_rules! unhandled_write {
    ($addr:expr, $value:expr) => {
        panic!("Unhandled write at 0x{:04x}_{:04x} of {:X}", $addr >> 16, $addr & 0xFFFF, $value)
    }
}

#[derive(Clone, Copy)]
pub struct PrefetchValue {
    pub op: u32,
    pub addr: u32,
}

impl Default for PrefetchValue {
    fn default() -> PrefetchValue {
        PrefetchValue {
            op: 0xDEADBEEF,
            addr: 0xDEADBEEF,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Type {
    U8,
    U16,
    U32,
}

// Enum for when we need to take special action accessing a memory region.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CodeRegion {
    // Can only be read while we are executing inside it
    Rom,
    // We need to track sequential/non-sequential accesses
    GamePak,
    // Any other region, no special action
    DontCare,
}

#[derive(Clone, Copy)]
struct WaitState {
    non_seq: Cycle,
    seq: Cycle,
}

impl WaitState {
    fn new(non_seq: i32, seq: i32) -> WaitState {
        WaitState {
            non_seq: Cycle(non_seq),
            seq: Cycle(seq),
        }
    }
}

#[derive(Clone)]
pub struct Interconnect {
    pub prefetch: [PrefetchValue; 2],

    // The code segment tracks the region of memory the PC is currently in, allowing the fast path
    // of instruciton fetching to be a simple bounds check then pointer load, falling back to the
    // fully-fledged `read32()` routine if necessary. `set_code_segment()` is used to update the
    // code segment to a different region, and `invalidate_code_segment()` when we want to force
    // falling back on the slow path.
    code_segment_ptr: *const u8,
    code_segment_size: u32,
    code_segment_base: u32,
    code_segment_timing_u16: Cycle,
    code_segment_timing_u32: Cycle,
    code_segment_region: CodeRegion,

    rom: Buffer,
    ewram: Buffer,
    iwram: Buffer,
    vram: Buffer,
    io_cache: Buffer,
    palette_cache: Buffer,
    oam_cache: Buffer,
    gamepak: Buffer,

    sram_wait_control: Cycle,
    gamepak_wait_states: [WaitState; 3],
    gamepak_prefetch_buffer: bool,
    gamepak_next_seq_addr: u32,

    post_boot_flag: bool,
    master_interrupt_enable: bool,
    interrupt_enable: IrqFlags,
    interrupt_flags: IrqFlags,
    lcd_control: LCDControl,
    sound_bias: SoundPWMControl,
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

            code_segment_ptr: ptr::null(),
            code_segment_size: 0,
            code_segment_base: 0,
            code_segment_timing_u16: Cycle(0),
            code_segment_timing_u32: Cycle(0),
            code_segment_region: CodeRegion::Rom,

            rom: Buffer::new(bios),
            ewram: Buffer::with_capacity(EWRAM_SIZE),
            iwram: Buffer::with_capacity(IWRAM_SIZE),
            vram: Buffer::with_capacity(VRAM_SIZE),
            io_cache: Buffer::with_capacity(0x804),
            palette_cache: Buffer::with_capacity(PALETTE_SIZE),
            oam_cache: Buffer::with_capacity(OAM_SIZE),
            gamepak: Buffer::new(game),

            sram_wait_control: Cycle(4),
            gamepak_wait_states: [
                WaitState::new(4, 2),
                WaitState::new(4, 4),
                WaitState::new(4, 8),
            ],
            gamepak_prefetch_buffer: false,
            gamepak_next_seq_addr: 0,

            post_boot_flag: false,
            master_interrupt_enable: true,
            interrupt_enable: Default::default(),
            interrupt_flags: Default::default(),
            lcd_control: Default::default(),
            sound_bias: SoundPWMControl {
                bias_level: 0x200,
                amplitude_resolution: 0,
            }
        }
    }

    pub fn read8(&mut self, addr: u32) -> (Cycle, u8) {
        match addr >> 24 {
            0x0 | 0x1 => { // rom
                // TODO: Handle out of bounds, currently we panic
                let read = if self.code_segment_region == CodeRegion::Rom {
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
                (PALETTE_TIMING_U8, self.palette_cache.read8(addr & PALETTE_MASK))
            }
            0x6 => { // vram
                (VRAM_TIMING_U8, self.vram.read8(vram_mask(addr - VRAM_START)))
            }
            0x7 => { // oam
                (OAM_TIMING_U8, self.oam_cache.read8(addr & OAM_MASK))
            }
            0x8...0xD => { // GamePak ROM
                self.gamepak_read8(addr)
            }

            _ => unhandled_read!(addr)
        }
    }

    pub fn read16(&mut self, addr: u32) -> (Cycle, u16) {
        if addr & 1 != 0 { panic!("Unaligned halfword read at 0x{:08X}", addr); }

        match addr >> 24 {
            0x0 | 0x1 => { // rom
                // TODO: Handle out of bounds, currently we panic
                let read = if self.code_segment_region == CodeRegion::Rom {
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
                (PALETTE_TIMING_U16, self.palette_cache.read16(addr & PALETTE_MASK))
            }
            0x6 => { // vram
                (VRAM_TIMING_U16, self.vram.read16(vram_mask(addr)))
            }
            0x7 => { // oam
                (OAM_TIMING_U16, self.oam_cache.read16(addr & OAM_MASK))
            }
            0x8...0xD => { // GamePak ROM
                self.gamepak_read16(addr)
            }

            _ => unhandled_read!(addr)
        }
    }

    pub fn read32(&mut self, addr: u32) -> (Cycle, u32) {
        if addr & 3 != 0 { panic!("Unaligned word read at 0x{:08X}", addr); }

        match addr >> 24 {
            0x0 | 0x1 => { // rom
                // TODO: Handle out of bounds, currently we panic
                let read = if self.code_segment_region == CodeRegion::Rom {
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
                (PALETTE_TIMING_U32, self.palette_cache.read32(addr & PALETTE_MASK))
            }
            0x6 => { // vram
                (VRAM_TIMING_U32, self.vram.read32(vram_mask(addr)))
            }
            0x7 => { // oam
                (OAM_TIMING_U32, self.oam_cache.read32(addr & OAM_MASK))
            }
            0x8...0xD => { // GamePak ROM
                self.gamepak_read32(addr)
            }

            _ => unhandled_read!(addr)
        }
    }

    pub fn write8(&mut self, addr: u32, value: u8) -> Cycle {
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
                self.palette_write8(addr, value)
            }
            0x6 => { // vram
                self.vram_write8(addr, value)
            }
            0x7 => { // oam
                // 8-bit writes ignored
                OAM_TIMING_U8
            }

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn write16(&mut self, addr: u32, value: u16) -> Cycle {
        if addr & 1 != 0 { panic!("Unaligned halfword write at 0x{:08X} of 0x{:08X}", addr, value); }
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
                self.palette_write16(addr, value)
            }
            0x6 => { // vram
                self.vram_write16(addr, value)
            }
            0x7 => { // oam
                self.oam_write16(addr, value)
            }

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn write32(&mut self, addr: u32, value: u32) -> Cycle {
        if addr & 3 != 0 { panic!("Unaligned word write at 0x{:08X} of 0x{:08X}", addr, value); }
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
                self.iowrite32(addr & 0xFFF, value);
                IO_TIMING_U32
            }
            0x5 => { // palette
                self.palette_write32(addr, value)
            }
            0x6 => { // vram
                self.vram_write32(addr, value)
            }
            0x7 => { // oam
                self.oam_write32(addr, value)
            }

            _ => unhandled_write!(addr, value),
        }
    }

    fn gamepak_read_timing(&self, addr: u32, ty: Type) -> Cycle {
        let upper = addr >> 24;
        assert!(0x8 <= upper && upper <= 0xD, "Invalid gamepak address {:X}", addr);
        let index = ((upper - 0x8) / 2) as usize;
        let _wait_state = self.gamepak_wait_states[index];
        match ty {
            Type::U8 | Type::U16 => {
                unimplemented!();
            }
            Type::U32 => {
                unimplemented!();
            }
        }
    }

    fn gamepak_read8(&mut self, addr: u32) -> (Cycle, u8) {
        let (cycles, read) = self.gamepak_read16(addr & !1);
        let read = (read >> (addr & 1)) as u8;
        self.gamepak_next_seq_addr = (addr + 1) & !1;
        (cycles, read)
    }

    fn gamepak_read16(&mut self, addr: u32) -> (Cycle, u16) {
        let seq = self.gamepak_next_seq_addr == addr;
        let upper = addr >> 24;
        debug_assert!(0x8 <= upper && upper <= 0xD);
        debug_assert!(addr & 1 == 0);

        let cycle = match upper {
            0x8 | 0x9 if seq => self.gamepak_wait_states[0].seq,
            0xA | 0xB if seq => self.gamepak_wait_states[1].seq,
            0xC | 0xD if seq => self.gamepak_wait_states[2].seq,
            0x8 | 0x9 if !seq => self.gamepak_wait_states[0].non_seq,
            0xA | 0xB if !seq => self.gamepak_wait_states[1].non_seq,
            0xC | 0xD if !seq => self.gamepak_wait_states[2].non_seq,
            _ => unreachable!(),
        };

        let addr = addr & GAMEPAK_MAX_SIZE_MASK;
        let read = if (addr as usize) < self.gamepak.len() {
            self.gamepak.read16(addr & GAMEPAK_MAX_SIZE_MASK)
        } else {
            (addr >> 1 & 0xFFFF) as u16
        };
        self.gamepak_next_seq_addr = (addr + 2) & !1;

        (cycle, read)
    }

    fn gamepak_read32(&mut self, addr: u32) -> (Cycle, u32) {
        let (cycles1, read1) = self.gamepak_read16(addr);
        let (cycles2, read2) = self.gamepak_read16(addr + 2);
        self.gamepak_next_seq_addr = (addr + 4) & !1;
        (cycles1 + cycles2, (read1 as u32) | ((read2 as u32) << 16))
    }

    fn palette_write8(&mut self, addr: u32, value: u8) -> Cycle {
        // Writes 8-bit value to upper and lower byte of halfword
        self.palette_write16(addr & !1, (value as u16) | ((value as u16) << 8))
    }

    fn palette_write16(&mut self, addr: u32, value: u16) -> Cycle {
        self.palette_cache.write16(addr & PALETTE_MASK, value);
        PALETTE_TIMING_U16
    }

    fn palette_write32(&mut self, addr: u32, value: u32) -> Cycle {
        self.palette_cache.write32(addr & PALETTE_MASK, value);
        PALETTE_TIMING_U32
    }

    fn oam_write16(&mut self, addr: u32, value: u16) -> Cycle {
        self.oam_cache.write16(addr & OAM_MASK, value);
        OAM_TIMING_U16
    }

    fn oam_write32(&mut self, addr: u32, value: u32) -> Cycle {
        self.oam_cache.write32(addr & OAM_MASK, value);
        OAM_TIMING_U32
    }

    fn vram_write8(&mut self, _addr: u32, _value: u8) -> Cycle {
        unimplemented!();
    }

    fn vram_write16(&mut self, addr: u32, value: u16) -> Cycle {
        self.vram.write16(vram_mask(addr), value);
        VRAM_TIMING_U16
    }

    fn vram_write32(&mut self, addr: u32, value: u32) -> Cycle {
        self.vram.write32(vram_mask(addr), value);
        VRAM_TIMING_U32
    }

    pub fn exec_thumb_slow(&mut self, addr: u32) -> (Cycle, u16) {
        let (cycles_used, read) = self.read16(addr);

        self.set_code_segment(addr);

        (cycles_used, read)
    }

    pub fn exec_thumb_fast(&mut self, addr: u32) -> Option<(Cycle, u16)> {
        let offset = addr.wrapping_sub(self.code_segment_base);
        if offset < self.code_segment_size {
            let next_op = self.prefetch[0].op as u16;
            self.prefetch[0] = self.prefetch[1];
            self.prefetch[1] = PrefetchValue {
                addr: addr,
                op: unsafe { *(self.code_segment_ptr.offset(offset as isize) as *const u16) } as u32,
            };
            Some((self.code_segment_timing_u16, next_op))
        } else {
            None
        }
    }

    pub fn exec_arm_slow(&mut self, addr: u32) -> (Cycle, u32) {
        let (cycles_used, read) = self.read32(addr);

        self.set_code_segment(addr);

        (cycles_used, read)
    }

    pub fn exec_arm_fast(&mut self, addr: u32) -> Option<(Cycle, u32)> {
        let offset = addr.wrapping_sub(self.code_segment_base);
        if offset < self.code_segment_size {
            let next_op = self.prefetch[0].op;
            self.prefetch[0] = self.prefetch[1];
            self.prefetch[1] = PrefetchValue {
                addr: addr,
                op: unsafe { *(self.code_segment_ptr.offset(offset as isize) as *const u32) },
            };
            Some((self.code_segment_timing_u32, next_op))
        } else {
            None
        }
    }

    pub fn set_code_segment(&mut self, addr: u32) {
        let upper = addr >> 24;
        let addr = addr & 0x00FF_FFFF;
        match upper {
            0 | 1 => { // rom
                self.code_segment_base = addr & ROM_MASK;
                self.code_segment_size = ROM_SIZE;
                self.code_segment_ptr = self.rom.as_ptr();
                self.code_segment_timing_u16 = ROM_TIMING_U16;
                self.code_segment_timing_u32 = ROM_TIMING_U32;
                self.code_segment_region = CodeRegion::Rom;
            }
            2 => { // ewram
                self.code_segment_base = addr & EWRAM_MASK;
                self.code_segment_size = EWRAM_SIZE;
                self.code_segment_ptr = self.ewram.as_ptr();
                self.code_segment_timing_u16 = EWRAM_TIMING_U16;
                self.code_segment_timing_u32 = EWRAM_TIMING_U32;
                self.code_segment_region = CodeRegion::DontCare;
            }
            3 => { // iwram
                self.code_segment_base = addr & IWRAM_MASK;
                self.code_segment_size = IWRAM_SIZE;
                self.code_segment_ptr = self.iwram.as_ptr();
                self.code_segment_timing_u16 = IWRAM_TIMING_U16;
                self.code_segment_timing_u32 = IWRAM_TIMING_U32;
                self.code_segment_region = CodeRegion::DontCare;
            }
            6 => { // vram
                self.code_segment_base = vram_mask(addr);
                self.code_segment_size = VRAM_SIZE;
                self.code_segment_ptr = self.vram.as_ptr();
                self.code_segment_timing_u16 = VRAM_TIMING_U16;
                self.code_segment_timing_u32 = VRAM_TIMING_U32;
                self.code_segment_region = CodeRegion::DontCare;
            }
            0x8...0xD => { // GamePak ROM
                let base = addr & !GAMEPAK_PAGE_MASK;
                self.code_segment_base = base;
                self.code_segment_size = GAMEPAK_PAGE_SIZE;
                self.code_segment_ptr = unsafe { self.gamepak.as_ptr().offset(base as isize) };
                self.code_segment_timing_u16 = self.gamepak_read_timing(addr, Type::U16);
                self.code_segment_timing_u32 = self.gamepak_read_timing(addr, Type::U32);
                self.code_segment_region = CodeRegion::GamePak;
            }
            _ => {
                self.invalidate_code_segment();
            }
        }
    }

    pub fn invalidate_code_segment(&mut self) {
        self.code_segment_base = CODE_SEGMENT_BASE_INVALID;
        self.code_segment_size = CODE_SEGMENT_SIZE_INVALID;
        self.code_segment_region = CodeRegion::DontCare;
    }
}

impl_io_map! {
    [Interconnect]
    (u16, 0x000, lcd_control) {
        read => |ic: &Interconnect| {
            Into::<u16>::into(ic.lcd_control)
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.lcd_control = value.into();
            println!("Setting lcd_control = {:?}", ic.lcd_control);
        }
    }

    (u32, 0x88, sound_bias) {
        read => |ic: &Interconnect| {
            Into::<u32>::into(ic.sound_bias)
        },
        write => |ic: &mut Interconnect, value: u32| {
            ic.sound_bias = value.into();
        }
    }

    (u16, 0x200, interrupt_enable) {
        read => |ic: &Interconnect| {
            Into::<u16>::into(ic.interrupt_enable)
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.interrupt_enable = value.into();
            println!("Setting interrupt_enable = {:?}",
                     ic.interrupt_enable);
        }
    }

    (u16, 0x202, interrupt_flags) {
        read => |ic: &Interconnect| {
            Into::<u16>::into(ic.interrupt_flags)
        },
        write => |ic: &mut Interconnect, value: u16| {
            let cur: u16 = ic.interrupt_flags.into();
            let new_state = cur & !value;
            ic.interrupt_flags = new_state.into();
            println!("Setting interrupt_flags = {:?}",
                     ic.interrupt_flags);
        }
    }

    (u32, 0x204, wait_control) {
        read => |_ic: &Interconnect| {
            0
        },
        write => |ic: &mut Interconnect, value: u32| {
            let ctrl = WaitStateControl::from(value);
            ic.sram_wait_control = Cycle([4, 3, 2, 8][ctrl.sram_wait_control]);
            ic.gamepak_wait_states[0].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_0_non_seq]);
            ic.gamepak_wait_states[0].seq = Cycle([2, 1][ctrl.wait_state_0_seq]);
            ic.gamepak_wait_states[1].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_1_non_seq]);
            ic.gamepak_wait_states[1].seq = Cycle([4, 1][ctrl.wait_state_1_seq]);
            ic.gamepak_wait_states[2].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_2_non_seq]);
            ic.gamepak_wait_states[2].seq = Cycle([8, 1][ctrl.wait_state_2_seq]);
            ic.gamepak_prefetch_buffer = ctrl.gamepak_prefetch_buffer;
        }
    }

    (u32, 0x208, master_interrupt_enable) {
        read => |ic: &Interconnect| {
            ic.master_interrupt_enable as u32
        },
        write => |ic: &mut Interconnect, value| {
            println!("Setting master_interrupt_enable = {}",
                     ic.master_interrupt_enable);
            ic.master_interrupt_enable = (value & 1) != 0;
        }
    }

    (u8, 0x300, post_boot_flag) {
        read => |ic: &Interconnect| {
            ic.post_boot_flag as u8
        },
        write => |ic: &mut Interconnect, value| {
            ic.post_boot_flag = value != 0;
        }
    }
}

unpacked_bitfield_struct! {
#[derive(Clone, Copy, Default, Debug)]
pub struct LCDControl: u16 {
    (0,3) bg_mode: u8,
    (3,1) cgb_mode: bool,
    (4,1) display_frame_select: u8,
    (5,1) hblank_interval_free: bool,
    (6,1) obj_vram_1d_map: bool,
    (7,1) forced_blank: bool,
    (8,1) display_bg0: bool,
    (9,1) display_bg1: bool,
    (10,1) display_bg2: bool,
    (11,1) display_bg3: bool,
    (12,1) display_obj: bool,
    (13,1) display_window_0: bool,
    (14,1) display_window_1: bool,
    (15,1) display_obj_window: bool,
}

#[derive(Clone, Copy, Default, Debug)]
pub struct IrqFlags: u16 {
    (0, 1) lcd_vblank: bool,
    (1, 1) lcd_hblank: bool,
    (2, 1) lcd_vcounter_match: bool,
    (3, 1) timer0_overflow: bool,
    (4, 1) timer1_overflow: bool,
    (5, 1) timer2_overflow: bool,
    (6, 1) timer3_overflow: bool,
    (7, 1) serial_comm: bool,
    (8, 1) dma0: bool,
    (9, 1) dma1: bool,
    (10, 1) dma2: bool,
    (11, 1) dma3: bool,
    (12, 1) keypad: bool,
    (13, 1) game_pak: bool,
}

#[derive(Clone, Copy)]
pub struct WaitStateControl: u32 {
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
pub struct SoundPWMControl: u32 {
    (0,10) bias_level: u16,
    (14,2) amplitude_resolution: u8,
}
}

enum TimerScale {
    X1,
    X64,
    X256,
    X1024,
}
