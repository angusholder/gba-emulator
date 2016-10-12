#[derive(Clone)]
struct Buffer {
    buffer: Box<[u8]>,
}

macro_rules! check_bounds {
    ($buffer:expr, $index:expr) => {
        if ($index as usize) >= $buffer.len() {
            panic!("index out of bounds: the len is {} but the index is {}",
                   $buffer.len(), $index);
        }
    }
}

impl Buffer {
    pub fn new(buffer: Box<[u8]>) -> Buffer {
        Buffer {
            buffer: buffer
        }
    }

    pub fn with_capacity(capacity: u32) -> Buffer {
        Buffer {
            buffer: vec![0u8; capacity as usize].into_boxed_slice()
        }
    }

    pub fn read8(&self, index: u32) -> u8 {
        self.buffer[index as usize]
    }

    pub fn read16(&self, index: u32) -> u16 {
        assert!(index & 1 == 0);
        check_bounds!(self.buffer, index + 1);
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * const u16) }
    }

    pub fn read32(&self, index: u32) -> u32 {
        assert!(index & 3 == 0);
        check_bounds!(self.buffer, index + 3);
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * const u32) }
    }

    pub fn write8(&mut self, index: u32, value: u8) {
        self.buffer[index as usize] = value;
    }

    pub fn write16(&mut self, index: u32, value: u16) {
        assert!(index & 1 == 0);
        check_bounds!(self.buffer, index + 1);
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * mut u16) = value; }
    }

    pub fn write32(&mut self, index: u32, value: u32) {
        assert!(index & 3 == 0);
        check_bounds!(self.buffer, index + 3);
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * mut u32) = value; }
    }
}

use io::IORegs;

pub struct Memory {
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
}

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

macro_rules! unhandled_read {
    ($addr:expr) => {
        panic!("Unhandled read at 0x{:08x}", $addr)
    }
}

macro_rules! unhandled_write {
    ($addr:expr, $value:expr) => {
        panic!("Unhandled write at 0x{:08x} of {:02X}", $addr, $value)
    }
}

// TODO: Handle timing and misaligned reads/writes
impl Memory {
    pub fn new(bios: Box<[u8]>) -> Memory {
        Memory {
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
        }
    }

    pub fn read8(&self, addr: u32) -> u8 {
        match addr >> 24 {
            // TODO: Handle out of bounds, currently we panic
            0x0 | 0x1 => {
                self.system_rom.read8(addr)
            }

            0x2 => {
                self.ewram.read8((addr - EWRAM_START) & EWRAM_MASK)
            }
            0x3 => {
                self.iwram.read8((addr - IWRAM_START) & IWRAM_MASK)
            }
            0x6 => {
                self.vram.read8((addr - VRAM_START) & VRAM_MASK)
            }

            0x4 => {
                match addr - 0x0400_0000 {
                    0x208 => self.io_regs.master_interrupt_enable as u8,
                    0x300 => self.io_regs.post_boot_flag as u8,
                    _ => unhandled_read!(addr)
                }
            }

            _ => unhandled_read!(addr),
        }
    }

    pub fn read16(&self, addr: u32) -> u16 {
        if addr & 1 != 0 { panic!("Unaligned halfword read at 0x{:08X}", addr); }
        match addr >> 24 {
            // TODO: Handle out of bounds, currently we panic
            0x0 | 0x1 => {
                self.system_rom.read16(addr)
            }

            0x2 => {
                self.ewram.read16((addr - EWRAM_START) & EWRAM_MASK)
            }
            0x3 => {
                self.iwram.read16((addr - IWRAM_START) & IWRAM_MASK)
            }
            0x6 => {
                self.vram.read16((addr - VRAM_START) & VRAM_MASK)
            }

            0x4 => {
                match addr - 0x0400_0000 {
                    _ => panic!("Unhandled read at 0x{:08x}", addr)
                }
            }

            _ => unhandled_read!(addr),
        }
    }

    pub fn read32(&self, addr: u32) -> u32 {
        if addr & 3 != 0 { panic!("Unaligned word read at 0x{:08X}", addr); }
        match addr >> 24 {
            // Handle out of bounds, currently we panic
            0x0 | 0x1 => {
                self.system_rom.read32(addr)
            }

            0x2 => {
                self.ewram.read32((addr - EWRAM_START) & EWRAM_MASK)
            }
            0x3 => {
                self.iwram.read32((addr - IWRAM_START) & IWRAM_MASK)
            }
            0x6 => {
                self.vram.read32((addr - VRAM_START) & VRAM_MASK)
            }

            0x4 => {
                match addr - 0x0400_0000 {
                    _ => unhandled_read!(addr)
                }
            }

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
            0x2 => {
                self.ewram.write8((addr - EWRAM_START) & EWRAM_MASK, value)
            }
            0x3 => {
                self.iwram.write8((addr - IWRAM_START) & IWRAM_MASK, value)
            }
            0x6 => {
                self.vram.write8((addr - VRAM_START) & VRAM_MASK, value)
            }

            0x4 => {
                match addr - 0x0400_0000 {
                    0x208 => self.io_regs.master_interrupt_enable = value != 0,
                    0x300 => self.io_regs.post_boot_flag = value != 0,
                    _ => unhandled_write!(addr, value)
                }
            }

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn write16(&mut self, addr: u32, value: u16) {
        if addr & 1 != 0 { panic!("Unaligned halfword write at 0x{:08X} of 0x{:08X}", addr, value); }
        match addr >> 24 {
            0x2 => {
                self.ewram.write16((addr - EWRAM_START) & EWRAM_MASK, value)
            }
            0x3 => {
                self.iwram.write16((addr - IWRAM_START) & IWRAM_MASK, value)
            }
            0x6 => {
                self.vram.write16((addr - VRAM_START) & VRAM_MASK, value)
            }

            0x4 => {
                match addr - 0x0400_0000 {
                    _ => unhandled_write!(addr, value)
                }
            }

            _ => unhandled_write!(addr, value),
        }
    }

    pub fn write32(&mut self, addr: u32, value: u32) {
        if addr & 1 != 0 { panic!("Unaligned word write at 0x{:08X} of 0x{:08X}", addr, value); }
        match addr >> 24 {
            0x2 => {
                self.ewram.write32((addr - EWRAM_START) & EWRAM_MASK, value)
            }
            0x3 => {
                self.iwram.write32((addr - IWRAM_START) & IWRAM_MASK, value)
            }
            0x6 => {
                self.vram.write32((addr - VRAM_START) & VRAM_MASK, value)
            }

            0x4 => {
                match addr - 0x0400_0000 {
                    _ => unhandled_write!(addr, value)
                }
            }

            _ => unhandled_write!(addr, value),
        }
    }
}
