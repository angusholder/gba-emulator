use utils::{ Buffer, Cycle };
use interconnect::{ IrqFlags, LCD_VBLANK, LCD_HBLANK, LCD_VCOUNTER_MATCH };
use log::*;
use super::oam::{ ObjAttributes, ObjTransform };

const ADDR_UPPER_MASK: u32 = 0xFF00_0000;

const PALETTE_START: u32 = 0x0500_0000;
const PALETTE_SIZE: u32 = 1024;
const PALETTE_MASK: u32 = PALETTE_SIZE - 1;
const PALETTE_TIMING_U8: Cycle = Cycle(1);
const PALETTE_TIMING_U16: Cycle = Cycle(1);
const PALETTE_TIMING_U32: Cycle = Cycle(2);

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

const OAM_START: u32 = 0x0700_0000;
const OAM_SIZE: u32 = 1024;
const OAM_MASK: u32 = OAM_SIZE - 1;
const OAM_TIMING_U8: Cycle = Cycle(1);
const OAM_TIMING_U16: Cycle = Cycle(1);
const OAM_TIMING_U32: Cycle = Cycle(1);

const VIRT_HEIGHT: u8 = 226;
const PHYS_HEIGHT: u8 = 160;
const VIRT_WIDTH: u16 = 307;
const PHYS_WIDTH: u16 = 240;

const VBLANK_START: u8 = 160;
const VBLANK_END: u8 = VIRT_HEIGHT;
const HBLANK_START: u16 = 240;
const HBLANK_END: u16 = VIRT_WIDTH;

#[derive(Default, Clone, Copy)]
pub struct Background {
    // BGCNT
    priority: u8,
    char_base_addr: u32,
    mosaic: bool,
    linear_palettes: bool, // (false=16/16, true=256/1)
    map_base_addr: u32,
    screen_size: u8,

    // Used only by bg[2,3]
    display_area_overflow: bool,

    // Used exclusively in text mode (bg[0,3] in mode 0, bg[0,1] in mode 1).
    pub x_offset: u16,
    pub y_offset: u16,

    // Used only by bg[2,3] when in rotation/scaling or bitmap modes.
    pub x_ref: u32,
    pub y_ref: u32,
    pub dx: u16,
    pub dmx: u16,
    pub dy: u16,
    pub dmy: u16,
}

#[derive(Clone)]
pub struct Renderer {
    vram: Buffer,
    bg_palette: Box<[u32]>, // Always 256, boxed because [T; 256] has no Clone impl
    obj_palette: Box<[u32]>, // Always 256, boxed because [T; 256] has no Clone impl
    obj_transforms: [ObjTransform; 32],
    obj_attributes: Box<[ObjAttributes]>, // Always 128, boxed because [T; 128] has no Clone impl

    // These are accessed directly by io r/w routines in the interconnect
    pub scanline: u8,
    x: u16,
    pub bg: [Background; 4],

    control: DisplayControlReg,
    vcount_setting: u8,
    vcount_irq_enable: bool,
    hblank_irq_enable: bool,
    vblank_irq_enable: bool,

    remaining_cycles: Cycle,
}

impl Renderer {
    pub fn new() -> Renderer {
        Renderer {
            vram: Buffer::with_capacity(VRAM_SIZE),
            bg_palette: vec![0; 256].into_boxed_slice(),
            obj_palette: vec![0; 256].into_boxed_slice(),
            obj_transforms: [ObjTransform::default(); 32],
            obj_attributes: vec![ObjAttributes::default(); 128].into_boxed_slice(),

            scanline: 0,
            x: 0,
            bg: Default::default(),

            control: Default::default(),
            vcount_setting: 0,
            vcount_irq_enable: false,
            hblank_irq_enable: false,
            vblank_irq_enable: false,

            remaining_cycles: Cycle(0),
        }
    }

    pub fn step_cycles(&mut self, mut cycles: Cycle) -> IrqFlags {
        let mut flags = IrqFlags::empty();
        cycles += self.remaining_cycles;
        while cycles > Cycle(3) && flags.is_empty() {
            cycles -= 4;
            if self.x == VIRT_WIDTH {
                self.x = 0;

                if self.scanline == VIRT_HEIGHT {
                    self.scanline = 0;
                } else {
                    self.scanline += 1;
                }

                if self.scanline == VBLANK_END {
                    note!(GPU, "VBlank ended");
                }

                if self.scanline == VBLANK_START {
                    if self.vblank_irq_enable {
                        flags |= LCD_VBLANK;
                        note!(GPU, "VBlank started with IRQ signal");
                    } else {
                        note!(GPU, "VBlank started");
                    }
                }
            } else {
                self.x += 1;
            }

            if self.x == HBLANK_START {
                if self.hblank_irq_enable {
                    flags |= LCD_HBLANK;
                    note!(GPU, "HBlank started with IRQ signal");
                } else {
                    trace!(GPU, "HBlank started");
                }
            }

            if self.x == HBLANK_END {
                trace!(GPU, "HBlank ended");
            }

            if self.scanline == self.vcount_setting && self.vcount_irq_enable {
                note!(GPU, "VCounter match IRQ signal");
                flags |= LCD_VCOUNTER_MATCH;
            }
        }

        self.remaining_cycles = cycles;

        flags
    }

    pub fn write_bgcnt(&mut self, index: usize, value: u16) {
        assert!(index < 4);

        let bg = &mut self.bg[index];
        let cnt = BackgroundControlReg::from(value);

        bg.priority = cnt.priority;
        bg.char_base_addr = (cnt.char_base_block as u32) * 16 * 1024;
        bg.mosaic = cnt.mosaic;
        bg.linear_palettes = cnt.linear_palettes;
        bg.map_base_addr = (cnt.map_base_block as u32) * 2 * 1024;
        if index == 2 || index == 3 {
            bg.display_area_overflow = cnt.display_area_overflow;
        }
    }

    pub fn read_bgcnt(&self, index: usize) -> u16 {
        assert!(index < 4);

        let bg = &self.bg[index];

        BackgroundControlReg {
            priority: bg.priority,
            char_base_block: (bg.char_base_addr / (16 * 1024)) as u8,
            mosaic: bg.mosaic,
            linear_palettes: bg.linear_palettes,
            map_base_block: (bg.map_base_addr / (2 * 1024)) as u8,
            screen_size: bg.screen_size,
            display_area_overflow: if index == 2 || index == 3 {
                bg.display_area_overflow
            } else {
                false
            }
        }.into()
    }

    pub fn read_dispcnt(&self) -> u16 {
        self.control.into()
    }

    pub fn write_dispcnt(&mut self, value: u16) {
        self.control = DisplayControlReg::from(value);
        note!(GPU, "Setting DISPCNT = {:?}", self.control);

        assert!(self.control.bg_mode <= 5);
        assert!(!self.control.cgb_mode);
        assert!(!self.control.display_window_0);
        assert!(!self.control.display_window_1);
        assert!(!self.control.display_obj_window);
    }

    pub fn read_dispstat(&self) -> u16 {
        let ret = DisplayStatusReg {
            vblank_flag: 160 <= self.scanline && self.scanline <= 226,
            hblank_flag: 240 <= self.x && self.x <= 307,
            vcount_flag: self.scanline == self.vcount_setting,
            vblank_irq_enable: self.vblank_irq_enable,
            hblank_irq_enable: self.hblank_irq_enable,
            vcount_irq_enable: self.vcount_irq_enable,
            vcount_setting: self.vcount_setting,
        }.into();
        note!(GPU, "Reading DISPSTAT = {:?}", ret);
        ret
    }

    pub fn write_dispstat(&mut self, value: u16) {
        let stat = DisplayStatusReg::from(value);
        self.vblank_irq_enable = stat.vblank_irq_enable;
        self.hblank_irq_enable = stat.hblank_irq_enable;
        self.vcount_irq_enable = stat.vcount_irq_enable;
        self.vcount_setting = stat.vcount_setting;
        note!(GPU, "Setting DISPSTAT: vblank_irq={}, hblank_irq={}, vcount_irq={}, vcount_setting={}",
              self.vblank_irq_enable,
              self.hblank_irq_enable,
              self.vcount_irq_enable,
              self.vcount_setting);
    }

    pub fn palette_write8(&mut self, addr: u32, value: u8) -> Cycle {
        // Writes 8-bit value to upper and lower byte of halfword
        self.palette_write16(addr & !1, (value as u16) | ((value as u16) << 8))
    }
    pub fn palette_write16(&mut self, addr: u32, value: u16) -> Cycle {
        assert!(addr & ADDR_UPPER_MASK == PALETTE_START);
        let index = (addr & 0x1FF) >> 1;
        let value = value & 0x7FFF;
        if addr & 0x200 == 0 {
            self.bg_palette[index as usize] = rgb15_to_bgr24(value);
        } else {
            self.obj_palette[index as usize] = rgb15_to_bgr24(value);
        }
        PALETTE_TIMING_U16
    }
    pub fn palette_write32(&mut self, addr: u32, value: u32) -> Cycle {
        self.palette_write16(addr, value as u16);
        self.palette_write16(addr + 2, (value >> 16) as u16);
        PALETTE_TIMING_U32
    }

    pub fn vram_write8(&mut self, addr: u32, value: u8) -> Cycle {
        assert!(addr & ADDR_UPPER_MASK == VRAM_START);
        if addr & 0x10000 == 0 {
            // Background VRam: write to both bytes of halfword addressed by rounded down `addr`
            let value = value as u16;
            self.vram_write16(addr & !1, value | value << 8);
        } else {
            // Character VRam: write is ignored.
            warn!(GPU, "8-bit write to character vram at {:X} of {:X} ignored", addr, value);
        }
        VRAM_TIMING_U8
    }
    pub fn vram_write16(&mut self, addr: u32, value: u16) -> Cycle {
        assert!(addr & ADDR_UPPER_MASK == VRAM_START);
        self.vram.write16(vram_mask(addr), value);
        VRAM_TIMING_U16
    }
    pub fn vram_write32(&mut self, addr: u32, value: u32) -> Cycle {
        assert!(addr & ADDR_UPPER_MASK == VRAM_START);
        self.vram.write32(vram_mask(addr), value);
        VRAM_TIMING_U32
    }

    pub fn oam_write8(&mut self, addr: u32, _value: u8) -> Cycle {
        assert!(addr & ADDR_UPPER_MASK == OAM_START);
        // 8-bit writes ignored
        OAM_TIMING_U8
    }
    pub fn oam_write16(&mut self, addr: u32, value: u16) -> Cycle {
        assert!(addr & ADDR_UPPER_MASK == OAM_START);

        match addr >> 1 & 3 {
            0 => {
                let index = addr >> 3 & 127;
                self.obj_attributes[index as usize].set_reg0(value);
            }
            1 => {
                let index = addr >> 3 & 127;
                self.obj_attributes[index as usize].set_reg1(value);
            }
            2 => {
                let index = addr >> 3 & 127;
                self.obj_attributes[index as usize].set_reg2(value);
            }
            3 => {
                let index = addr >> 5 & 31;
                match addr >> 3 & 3 {
                    0 => self.obj_transforms[index as usize].pa = value,
                    1 => self.obj_transforms[index as usize].pb = value,
                    2 => self.obj_transforms[index as usize].pc = value,
                    3 => self.obj_transforms[index as usize].pd = value,
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }

        OAM_TIMING_U16
    }
    pub fn oam_write32(&mut self, addr: u32, value: u32) -> Cycle {
        assert!(addr & ADDR_UPPER_MASK == OAM_START);
        self.oam_write16(addr, value as u16);
        self.oam_write16(addr + 2, (value >> 16) as u16);
        OAM_TIMING_U32
    }

    pub fn palette_read8(&self, addr: u32) -> (Cycle, u8) {
        let read = self.palette_read16(addr).1 >> 8*(addr&1);
        (PALETTE_TIMING_U8, read as u8)
    }
    pub fn palette_read16(&self, addr: u32) -> (Cycle, u16) {
        assert!(addr & ADDR_UPPER_MASK == PALETTE_START);
        let index = (addr & 0x1FF) >> 1;
        let read = if addr & 0x200 == 0 {
            bgr24_to_rgb15(self.bg_palette[index as usize])
        } else {
            bgr24_to_rgb15(self.obj_palette[index as usize])
        };
        (PALETTE_TIMING_U16, read)
    }
    pub fn palette_read32(&self, addr: u32) -> (Cycle, u32) {
        let read = self.palette_read16(addr).1 as u32 |
                  (self.palette_read16(addr + 2).1 as u32) << 16;
        (PALETTE_TIMING_U32, read)
    }

    pub fn vram_read8(&self, addr: u32) -> (Cycle, u8) {
        assert!(addr & ADDR_UPPER_MASK == VRAM_START);
        (VRAM_TIMING_U8, self.vram.read8(vram_mask(addr - VRAM_START)))
    }
    pub fn vram_read16(&self, addr: u32) -> (Cycle, u16) {
        assert!(addr & ADDR_UPPER_MASK == VRAM_START);
        (VRAM_TIMING_U16, self.vram.read16(vram_mask(addr - VRAM_START)))
    }
    pub fn vram_read32(&self, addr: u32) -> (Cycle, u32) {
        assert!(addr & ADDR_UPPER_MASK == VRAM_START);
        (VRAM_TIMING_U32, self.vram.read32(vram_mask(addr - VRAM_START)))
    }

    pub fn oam_read8(&self, addr: u32) -> (Cycle, u8) {
        assert!(addr & ADDR_UPPER_MASK == OAM_START);
        let read = self.oam_read16(addr & OAM_MASK).1 >> 8*(addr&1);
        (OAM_TIMING_U8, read as u8)
    }
    pub fn oam_read16(&self, addr: u32) -> (Cycle, u16) {
        assert!(addr & ADDR_UPPER_MASK == OAM_START);

        let read = match addr >> 1 & 3 {
            0 => {
                let index = addr >> 3 & 127;
                self.obj_attributes[index as usize].get_reg0()
            }
            1 => {
                let index = addr >> 3 & 127;
                self.obj_attributes[index as usize].get_reg1()
            }
            2 => {
                let index = addr >> 3 & 127;
                self.obj_attributes[index as usize].get_reg2()
            }
            3 => {
                let index = addr >> 5 & 31;
                match addr >> 3 & 3 {
                    0 => self.obj_transforms[index as usize].pa,
                    1 => self.obj_transforms[index as usize].pb,
                    2 => self.obj_transforms[index as usize].pc,
                    3 => self.obj_transforms[index as usize].pd,
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };

        (OAM_TIMING_U16, read)
    }
    pub fn oam_read32(&self, addr: u32) -> (Cycle, u32) {
        assert!(addr & ADDR_UPPER_MASK == OAM_START);
        let read = self.oam_read16(addr).1 as u32 |
                  (self.oam_read16(addr + 2).1 as u32) << 16;
        (OAM_TIMING_U32, read)
    }

    pub fn vram_as_ptr(&self) -> *const u8 {
        self.vram.as_ptr()
    }
}

fn rgb15_to_bgr24(c: u16) -> u32 {
    let r = (c & 0x1F) as u32;
    let g = (c >> 5 & 0x1F) as u32;
    let b = (c >> 10 & 0x1F) as u32;
    b | (g << 8) | (r << 16)
}

fn bgr24_to_rgb15(c: u32) -> u16 {
    let b = (c & 0x1F) as u16;
    let g = (c >> 8 & 0x1F) as u16;
    let r = (c >> 16 & 0x1F) as u16;
    r | (g << 5) | (b << 10)
}

unpacked_bitfield_struct! {
#[derive(Clone, Copy, Default, Debug)]
struct DisplayControlReg: u16 {
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

struct DisplayStatusReg: u16 {
    (0,1) vblank_flag: bool,
    (1,1) hblank_flag: bool,
    (2,1) vcount_flag: bool,
    (3,1) vblank_irq_enable: bool,
    (4,1) hblank_irq_enable: bool,
    (5,1) vcount_irq_enable: bool,
    (8,8) vcount_setting: u8,
}

struct BackgroundControlReg: u16 {
    (0,2) priority: u8,
    (2,2) char_base_block: u8,
    (6,1) mosaic: bool,
    (7,1) linear_palettes: bool,
    (8,5) map_base_block: u8,
    (13,1)display_area_overflow: bool,
    (14,2)screen_size: u8,
}
}
