#[derive(Clone)]
struct Buffer {
    buffer: Box<[u8]>,
}

impl Buffer {
    pub fn new(buffer: Box<[u8]>) -> Buffer {
        Buffer {
            buffer: buffer
        }
    }

    pub fn with_capacity(capacity: usize) -> Buffer {
        Buffer {
            buffer: vec![0u8; capacity].into_boxed_slice()
        }
    }

    pub fn read8(&self, index: u32) -> u8 {
        self.buffer[index as usize]
    }

    pub fn read16(&self, index: u32) -> u16 {
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * const u16) }
    }

    pub fn read32(&self, index: u32) -> u32 {
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * const u32) }
    }

    pub fn write8(&mut self, index: u32, value: u8) {
        self.buffer[index as usize] = value;
    }

    pub fn write16(&mut self, index: u32, value: u16) {
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * mut u16) = value; }
    }

    pub fn write32(&mut self, index: u32, value: u32) {
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * mut u32) = value; }
    }
}

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
}

// macro_rules! define_range {
//     ($name:ident, $start:expr, $end:expr, $size:expr) => {
//         const concat_idents!($name, _START): usize = $start;
//         const concat_idents!($name, _END): usize = $end;
//         const concat_idents!($name, _SIZE): usize = $size;
//         const concat_idents!($name, _MASK): usize = $size - 1;
//     }
// }

// define_range!(BIOS, 0x0000_0000, 0x0000_3FFF, 0x4000);
// define_range!(EWRAM, 0x0200_0000, 0x0203_FFFF, 0x4_0000);
// define_range!(IWRAM, 0x0300_0000, 0x0300_7FFF, 0x8000);
// define_range!(IO_REGS, 0x0400_0000, 0x0400_03FE, 0x3FF);
// const BIOS_START: usize = 0x0000_0000;
// const BIOS_END: usize = 0x0000_3FFF;
// const BIOS_SIZE: usize = BIOS_END - BIOS_START + 1;
// const IWRAM_START: usize = 0x0200_0000;
// const IWRAM_END: usize = 0x0200_0000;

impl Memory {
    pub fn new(bios: Box<[u8]>) -> Memory {
        Memory {
            system_rom: Buffer::new(bios),
            ewram: Buffer::with_capacity(256 * 1024),
            iwram: Buffer::with_capacity(32 * 1024),
            vram: Buffer::with_capacity(96 * 1024),
            prefetch: [0, 0],

            sram_wait_control: 0,
            wait_state_0_non_seq: 0,
            wait_state_0_seq: 0,
            wait_state_1_non_seq: 0,
            wait_state_1_seq: 0,
            wait_state_2_non_seq: 0,
            wait_state_2_seq: 0,

            game_pak_prefetch_buffer: false,
        }
    }

    pub fn read8(&self, addr: u32) -> u8 {
        match addr >> 24 {
            _ => panic!("Unhandled read at 0x{:08x}", addr),
        }
    }

    pub fn read16(&self, addr: u32) -> u16 {
        match addr >> 24 {
            0x0 | 0x1 => {
                self.system_rom.read16(addr)
            }

            0x2 => { // EWRAM
                self.ewram.read16((addr - 0x0200_0000) & 0x3_FFFF)
            }
            _ => panic!("Unhandled read at 0x{:08x}", addr),
        }
    }

    pub fn read32(&self, addr: u32) -> u32 {
        match addr >> 24 {
            0x0 | 0x1 => {
                // BIOS is execute only, return prefetched instruction
                // self.prefetch[0]
                self.system_rom.read32(addr)
            }
            _ => panic!("Unhandled read at 0x{:08x}", addr),
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
        match addr {
            _ => panic!("Unhandled write at 0x{:08x} of {:02X}", addr, value),
        }
    }

    pub fn write16(&mut self, addr: u32, value: u16) {
        match addr {
            _ => panic!("Unhandled write at 0x{:08x} of {:04X}", addr, value),
        }
    }

    pub fn write32(&mut self, addr: u32, value: u32) {
        match addr {
            _ => panic!("Unhandled write at 0x{:08x} of {:08X}", addr, value),
        }
    }
}
