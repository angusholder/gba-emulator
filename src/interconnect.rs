use io::{ IORegs, IrqFlags, LCDControl };
use utils::Buffer;

const BIOS_START: u32 = 0x0000_0000;
const BIOS_SIZE: u32 = 0x4000;
const BIOS_MASK: u32 = BIOS_SIZE - 1;

const EWRAM_START: u32 = 0x0200_0000;
const EWRAM_SIZE: u32 = 256 * 1024;
const EWRAM_MASK: u32 = EWRAM_SIZE - 1;

const IWRAM_START: u32 = 0x0300_0000;
const IWRAM_SIZE: u32 = 32 * 1024;
const IWRAM_MASK: u32 = IWRAM_SIZE - 1;

const VRAM_START: u32 = 0x0600_0000;
const VRAM_SIZE: u32 = 96 * 1024;
const VRAM_MASK: u32 = VRAM_SIZE - 1;

const IO_MASK: u32 = 0xFFF;

macro_rules! unhandled_read {
    ($addr:expr) => {
        panic!("Unhandled read at 0x{:08x}", $addr)
    }
}

macro_rules! unhandled_write {
    ($addr:expr, $value:expr) => {
        panic!("Unhandled write at 0x{:08x} of {:X}", $addr, $value)
    }
}

pub struct Interconnect {
    system_rom: Buffer,
    ewram: Buffer,
    iwram: Buffer,
    vram: Buffer,
    pub prefetch: [u32; 2],

    sram_wait_control: usize,
    wait_state_0_non_seq: usize,
    wait_state_0_seq: usize,
    wait_state_1_non_seq: usize,
    wait_state_1_seq: usize,
    wait_state_2_non_seq: usize,
    wait_state_2_seq: usize,

    game_pak_prefetch_buffer: bool,

    io_regs: IORegs,
    interrupt_enable: IrqFlags,
    lcd_control: LCDControl,

    write_cache: Buffer,
}

// TODO: Handle timing and misaligned reads/writes
impl Interconnect {
    pub fn new(bios: Box<[u8]>) -> Interconnect {
        Interconnect {
            system_rom: Buffer::new(bios),
            ewram: Buffer::with_capacity(EWRAM_SIZE),
            iwram: Buffer::with_capacity(IWRAM_SIZE),
            vram: Buffer::with_capacity(VRAM_SIZE),
            prefetch: [0, 0],

            sram_wait_control: 0,
            wait_state_0_non_seq: 0,
            wait_state_0_seq: 0,
            wait_state_1_non_seq: 0,
            wait_state_1_seq: 0,
            wait_state_2_non_seq: 0,
            wait_state_2_seq: 0,

            game_pak_prefetch_buffer: false,

            io_regs: IORegs::default(),
            interrupt_enable: Default::default(),
            lcd_control: Default::default(),

            write_cache: Buffer::new(vec![0u8; 0x804].into_boxed_slice()),
        }
    }

    pub fn read8(&self, addr: u32) -> u8 {
        match addr >> 24 {
            // TODO: Handle out of bounds, currently we panic
            0x0 | 0x1 => self.system_rom.read8(addr),

            0x2 => self.ewram.read8((addr - EWRAM_START) & EWRAM_MASK),
            0x3 => self.iwram.read8((addr - IWRAM_START) & IWRAM_MASK),
            0x6 => self.vram.read8((addr - VRAM_START) & VRAM_MASK),

            0x4 => self.ioread8(addr & IO_MASK),

            _ => unhandled_read!(addr),
        }
    }

    pub fn read16(&self, addr: u32) -> u16 {
        if addr & 1 != 0 { panic!("Unaligned halfword read at 0x{:08X}", addr); }
        match addr >> 24 {
            // TODO: Handle out of bounds, currently we panic
            0x0 | 0x1 => self.system_rom.read16(addr),

            0x2 => self.ewram.read16((addr - EWRAM_START) & EWRAM_MASK),
            0x3 => self.iwram.read16((addr - IWRAM_START) & IWRAM_MASK),
            0x6 => self.vram.read16((addr - VRAM_START) & VRAM_MASK),

            0x4 => self.ioread16(addr & IO_MASK),

            _ => unhandled_read!(addr),
        }
    }

    pub fn read32(&self, addr: u32) -> u32 {
        if addr & 3 != 0 { panic!("Unaligned word read at 0x{:08X}", addr); }
        match addr >> 24 {
            // Handle out of bounds, currently we panic
            0x0 | 0x1 => self.system_rom.read32(addr),

            0x2 => self.ewram.read32((addr - EWRAM_START) & EWRAM_MASK),
            0x3 => self.iwram.read32((addr - IWRAM_START) & IWRAM_MASK),
            0x6 => self.vram.read32((addr - VRAM_START) & VRAM_MASK),

            0x4 => self.ioread32(addr & IO_MASK),

            _ => unhandled_read!(addr),
        }
    }

    pub fn exec32(&self, addr: u32) -> u32 {
        if addr < 0x0200_0000 {
            self.system_rom.read32(addr)
        } else {
            self.read32(addr)
        }
    }

    pub fn exec16(&self, addr: u32) -> u16 {
        if addr < 0x0200_0000 {
            self.system_rom.read16(addr)
        } else {
            self.read16(addr)
        }
    }

    pub fn write8(&mut self, addr: u32, value: u8) {
        match addr >> 24 {
            0x2 => self.ewram.write8((addr - EWRAM_START) & EWRAM_MASK, value),
            0x3 => self.iwram.write8((addr - IWRAM_START) & IWRAM_MASK, value),
            0x6 => self.vram.write8((addr - VRAM_START) & VRAM_MASK, value),

            0x4 => self.iowrite8(addr & IO_MASK, value),

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn write16(&mut self, addr: u32, value: u16) {
        if addr & 1 != 0 { panic!("Unaligned halfword write at 0x{:08X} of 0x{:08X}", addr, value); }
        match addr >> 24 {
            0x2 => self.ewram.write16((addr - EWRAM_START) & EWRAM_MASK, value),
            0x3 => self.iwram.write16((addr - IWRAM_START) & IWRAM_MASK, value),
            0x6 => self.vram.write16((addr - VRAM_START) & VRAM_MASK, value),

            0x4 => self.iowrite16(addr & IO_MASK, value),

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn write32(&mut self, addr: u32, value: u32) {
        if addr & 3 != 0 { panic!("Unaligned word write at 0x{:08X} of 0x{:08X}", addr, value); }
        match addr >> 24 {
            0x2 => self.ewram.write32((addr - EWRAM_START) & EWRAM_MASK, value),
            0x3 => self.iwram.write32((addr - IWRAM_START) & IWRAM_MASK, value),
            0x6 => self.vram.write32((addr - VRAM_START) & VRAM_MASK, value),

            0x4 => self.iowrite32(addr & 0xFFF, value),

            _ => unhandled_write!(addr, value),
        }
    }
}

impl_io_map! {
    [Interconnect]
    (u16, 0x000) {
        read => |ic: &Interconnect| {
            Into::<u16>::into(ic.lcd_control)
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.lcd_control = value.into();
            println!("Setting lcd_control = {:?}", ic.lcd_control);
        }
    }

    (u16, 0x200) {
        read => |ic: &Interconnect| {
            Into::<u16>::into(ic.interrupt_enable)
        },
        write => |ic: &mut Interconnect, value: u16| {
            ic.interrupt_enable = value.into();
            println!("Setting interrupt_enable = {:?}", ic.interrupt_enable);
        }
    }

    (u16, 0x208) {
        read => |ic: &Interconnect| {
            ic.io_regs.master_interrupt_enable as u16
        },
        write => |ic: &mut Interconnect, value| {
            println!("Setting master_interrupt_enable = {}", ic.io_regs.master_interrupt_enable);
            ic.io_regs.master_interrupt_enable = value != 0;
        }
    }

    (u8, 0x300) {
        read => |ic: &Interconnect| {
            ic.io_regs.post_boot_flag as u8
        },
        write => |ic: &mut Interconnect, value| {
            ic.io_regs.post_boot_flag = value != 0;
        }
    }
}
