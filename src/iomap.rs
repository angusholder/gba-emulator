use gba::{Gba, IrqFlags, WaitStateControlReg, SoundPWMControlReg };
use log::*;
use utils::Cycle;

macro_rules! unreadable {
    ($kind:expr, $reg:expr) => {{
        warn!($kind, "Tried to read the write-only {} (at 0x{:X}).", stringify!($reg), $reg);
        0
    }}
}

macro_rules! unwriteable {
    ($kind:expr, $reg:expr, $value:expr) => {
        warn!($kind, "Tried to write {:X} to read-only {} (at 0x{:X})", $value, stringify!($reg), $reg);
    }
}

pub fn read16(gba: &Gba, addr: u32) -> u16 {
    match addr & !1 {
        REG_DISPCNT => gba.renderer.read_dispcnt(),
        REG_DISPSTAT => gba.renderer.read_dispstat(),
        REG_SOUNDBIAS => gba.sound_bias.into(),
        REG_IE => gba.interrupt_enable.bits(),
        REG_IF => gba.interrupt_flags.bits(),
        REG_WAITCNT => {
            warn!(IO, "Reading stubbed WAITCNT");
            0
        }
        REG_IME => gba.master_interrupt_enable as u16,
        // REG_POSTFLG | REG_HALTCNT
        REG_POSTFLG => (gba.post_boot_flag as u16) | (0u16 << 8),
        REG_VCOUNT => gba.renderer.scanline as u16,

        REG_BG0CNT => gba.renderer.read_bgcnt(0),
        REG_BG1CNT => gba.renderer.read_bgcnt(1),
        REG_BG2CNT => gba.renderer.read_bgcnt(2),
        REG_BG3CNT => gba.renderer.read_bgcnt(3),

        REG_BG0HOFS => unreadable!(GPU, REG_BG0HOFS),
        REG_BG0VOFS => unreadable!(GPU, REG_BG0VOFS),
        REG_BG1HOFS => unreadable!(GPU, REG_BG1HOFS),
        REG_BG1VOFS => unreadable!(GPU, REG_BG1VOFS),
        REG_BG2HOFS => unreadable!(GPU, REG_BG2HOFS),
        REG_BG2VOFS => unreadable!(GPU, REG_BG2VOFS),
        REG_BG3HOFS => unreadable!(GPU, REG_BG3HOFS),
        REG_BG3VOFS => unreadable!(GPU, REG_BG3VOFS),

        REG_BG2PA => unreadable!(GPU, REG_BG2PA),
        REG_BG2PB => unreadable!(GPU, REG_BG2PB),
        REG_BG2PC => unreadable!(GPU, REG_BG2PC),
        REG_BG2PD => unreadable!(GPU, REG_BG2PD),
        REG_BG2X_LO => unreadable!(GPU, REG_BG2X_LO),
        REG_BG2X_HI => unreadable!(GPU, REG_BG2X_HI),
        REG_BG2Y_LO => unreadable!(GPU, REG_BG2Y_LO),
        REG_BG2Y_HI => unreadable!(GPU, REG_BG2Y_HI),

        REG_BG3PA => unreadable!(GPU, REG_BG2PA),
        REG_BG3PB => unreadable!(GPU, REG_BG2PB),
        REG_BG3PC => unreadable!(GPU, REG_BG2PC),
        REG_BG3PD => unreadable!(GPU, REG_BG2PD),
        REG_BG3X_LO => unreadable!(GPU, REG_BG2X_LO),
        REG_BG3X_HI => unreadable!(GPU, REG_BG2X_HI),
        REG_BG3Y_LO => unreadable!(GPU, REG_BG2Y_LO),
        REG_BG3Y_HI => unreadable!(GPU, REG_BG2Y_HI),

        REG_TM0CNT_L => gba.timers[0].get_current_value(gba.cycles),
        REG_TM1CNT_L => gba.timers[1].get_current_value(gba.cycles),
        REG_TM2CNT_L => gba.timers[2].get_current_value(gba.cycles),
        REG_TM3CNT_L => gba.timers[3].get_current_value(gba.cycles),

        REG_TM0CNT_H => gba.timers[0].read_control(),
        REG_TM1CNT_H => gba.timers[1].read_control(),
        REG_TM2CNT_H => gba.timers[2].read_control(),
        REG_TM3CNT_H => gba.timers[3].read_control(),

        REG_DMA0SAD_LO => unreadable!(DMA0, REG_DMA0SAD_LO),
        REG_DMA0SAD_HI => unreadable!(DMA0, REG_DMA0SAD_HI),
        REG_DMA1SAD_LO => unreadable!(DMA1, REG_DMA1SAD_LO),
        REG_DMA1SAD_HI => unreadable!(DMA1, REG_DMA1SAD_HI),
        REG_DMA2SAD_LO => unreadable!(DMA2, REG_DMA2SAD_LO),
        REG_DMA2SAD_HI => unreadable!(DMA2, REG_DMA2SAD_HI),
        REG_DMA3SAD_LO => unreadable!(DMA3, REG_DMA3SAD_LO),
        REG_DMA3SAD_HI => unreadable!(DMA3, REG_DMA3SAD_HI),

        REG_DMA0DAD_LO => unreadable!(DMA0, REG_DMA0DAD_LO),
        REG_DMA0DAD_HI => unreadable!(DMA0, REG_DMA0DAD_HI),
        REG_DMA1DAD_LO => unreadable!(DMA1, REG_DMA1DAD_LO),
        REG_DMA1DAD_HI => unreadable!(DMA1, REG_DMA1DAD_HI),
        REG_DMA2DAD_LO => unreadable!(DMA2, REG_DMA2DAD_LO),
        REG_DMA2DAD_HI => unreadable!(DMA2, REG_DMA2DAD_HI),
        REG_DMA3DAD_LO => unreadable!(DMA3, REG_DMA3DAD_LO),
        REG_DMA3DAD_HI => unreadable!(DMA3, REG_DMA3DAD_HI),

        REG_DMA0CNT_L => unreadable!(DMA0, REG_DMA0CNT_L),
        REG_DMA1CNT_L => unreadable!(DMA1, REG_DMA1CNT_L),
        REG_DMA2CNT_L => unreadable!(DMA2, REG_DMA2CNT_L),
        REG_DMA3CNT_L => unreadable!(DMA3, REG_DMA3CNT_L),

        REG_DMA0CNT_H => gba.dma[0].read_control(),
        REG_DMA1CNT_H => gba.dma[1].read_control(),
        REG_DMA2CNT_H => gba.dma[2].read_control(),
        REG_DMA3CNT_H => gba.dma[3].read_control(),

        _ => {
            print!("(STUB) ");
            0
        }
    }
}

pub fn write16(gba: &mut Gba, addr: u32, value: u16) {
    match addr & !1 {
        REG_DISPCNT => gba.renderer.write_dispcnt(value),
        REG_DISPSTAT => gba.renderer.write_dispstat(value),
        REG_SOUNDBIAS => gba.sound_bias = SoundPWMControlReg::from(value),
        REG_IE => {
            gba.interrupt_enable = IrqFlags::from_bits_truncate(value);
            trace!(CPU, "Setting interrupt_enable = {:?}", gba.interrupt_enable);
        }
        REG_IF => {
            let value = IrqFlags::from_bits_truncate(value);
            gba.interrupt_flags.remove(value);
            trace!(CPU, "Setting interrupt_flags = {:?}", gba.interrupt_flags);
        }
        REG_WAITCNT => {
            let ctrl = WaitStateControlReg::from(value);
            gba.sram_wait_control = Cycle([4, 3, 2, 8][ctrl.sram_wait_control]);
            gba.gamepak.wait_states[0].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_0_non_seq]);
            gba.gamepak.wait_states[0].seq = Cycle([2, 1][ctrl.wait_state_0_seq]);
            gba.gamepak.wait_states[1].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_1_non_seq]);
            gba.gamepak.wait_states[1].seq = Cycle([4, 1][ctrl.wait_state_1_seq]);
            gba.gamepak.wait_states[2].non_seq = Cycle([4, 3, 2, 8][ctrl.wait_state_2_non_seq]);
            gba.gamepak.wait_states[2].seq = Cycle([8, 1][ctrl.wait_state_2_seq]);
            gba.gamepak_prefetch_buffer = ctrl.gamepak_prefetch_buffer;
        }
        REG_IME => {
            trace!(CPU, "Setting master_interrupt_enable = {}", gba.master_interrupt_enable);
            gba.master_interrupt_enable = (value & 1) != 0;
        }
        // TODO: REG_HALTCNT
        REG_POSTFLG => gba.post_boot_flag = value != 0,
        REG_VCOUNT => unwriteable!(GPU, REG_VCOUNT, value),

        REG_BG0CNT => gba.renderer.write_bgcnt(0, value),
        REG_BG1CNT => gba.renderer.write_bgcnt(1, value),
        REG_BG2CNT => gba.renderer.write_bgcnt(2, value),
        REG_BG3CNT => gba.renderer.write_bgcnt(3, value),

        REG_BG0HOFS => gba.renderer.bg[0].x_offset = value & 0x1FF,
        REG_BG0VOFS => gba.renderer.bg[0].y_offset = value & 0x1FF,
        REG_BG1HOFS => gba.renderer.bg[1].x_offset = value & 0x1FF,
        REG_BG1VOFS => gba.renderer.bg[1].y_offset = value & 0x1FF,
        REG_BG2HOFS => gba.renderer.bg[2].x_offset = value & 0x1FF,
        REG_BG2VOFS => gba.renderer.bg[2].y_offset = value & 0x1FF,
        REG_BG3HOFS => gba.renderer.bg[3].x_offset = value & 0x1FF,
        REG_BG3VOFS => gba.renderer.bg[3].y_offset = value & 0x1FF,

        REG_BG2PA => gba.renderer.bg[2].dx = value,
        REG_BG2PB => gba.renderer.bg[2].dmx = value,
        REG_BG2PC => gba.renderer.bg[2].dy = value,
        REG_BG2PD => gba.renderer.bg[2].dmy = value,
        REG_BG2X_LO => gba.renderer.bg[2].write_xcoord_lo(value),
        REG_BG2X_HI => gba.renderer.bg[2].write_xcoord_hi(value),
        REG_BG2Y_LO => gba.renderer.bg[2].write_ycoord_lo(value),
        REG_BG2Y_HI => gba.renderer.bg[2].write_ycoord_hi(value),

        REG_BG3PA => gba.renderer.bg[3].dx = value,
        REG_BG3PB => gba.renderer.bg[3].dmx = value,
        REG_BG3PC => gba.renderer.bg[3].dy = value,
        REG_BG3PD => gba.renderer.bg[3].dmy = value,
        REG_BG3X_LO => gba.renderer.bg[3].write_xcoord_lo(value),
        REG_BG3X_HI => gba.renderer.bg[3].write_xcoord_hi(value),
        REG_BG3Y_LO => gba.renderer.bg[3].write_ycoord_lo(value),
        REG_BG3Y_HI => gba.renderer.bg[3].write_ycoord_hi(value),

        REG_TM0CNT_L => gba.timers[0].set_reload_value(value),
        REG_TM1CNT_L => gba.timers[1].set_reload_value(value),
        REG_TM2CNT_L => gba.timers[2].set_reload_value(value),
        REG_TM3CNT_L => gba.timers[3].set_reload_value(value),

        REG_TM0CNT_H => gba.timers[0].write_control(gba.cycles, value),
        REG_TM1CNT_H => gba.timers[1].write_control(gba.cycles, value),
        REG_TM2CNT_H => gba.timers[2].write_control(gba.cycles, value),
        REG_TM3CNT_H => gba.timers[3].write_control(gba.cycles, value),

        REG_DMA0SAD_LO => gba.dma[0].write_source_lo(value),
        REG_DMA0SAD_HI => gba.dma[0].write_source_hi(value),
        REG_DMA1SAD_LO => gba.dma[1].write_source_lo(value),
        REG_DMA1SAD_HI => gba.dma[1].write_source_hi(value),
        REG_DMA2SAD_LO => gba.dma[2].write_source_lo(value),
        REG_DMA2SAD_HI => gba.dma[2].write_source_hi(value),
        REG_DMA3SAD_LO => gba.dma[3].write_source_lo(value),
        REG_DMA3SAD_HI => gba.dma[3].write_source_hi(value),

        REG_DMA0DAD_LO => gba.dma[0].write_dest_lo(value),
        REG_DMA0DAD_HI => gba.dma[0].write_dest_hi(value),
        REG_DMA1DAD_LO => gba.dma[1].write_dest_lo(value),
        REG_DMA1DAD_HI => gba.dma[1].write_dest_hi(value),
        REG_DMA2DAD_LO => gba.dma[2].write_dest_lo(value),
        REG_DMA2DAD_HI => gba.dma[2].write_dest_hi(value),
        REG_DMA3DAD_LO => gba.dma[3].write_dest_lo(value),
        REG_DMA3DAD_HI => gba.dma[3].write_dest_hi(value),

        REG_DMA0CNT_L => gba.dma[0].write_word_count(value),
        REG_DMA1CNT_L => gba.dma[1].write_word_count(value),
        REG_DMA2CNT_L => gba.dma[2].write_word_count(value),
        REG_DMA3CNT_L => gba.dma[3].write_word_count(value),

        REG_DMA0CNT_H => gba.dma[0].write_control(value),
        REG_DMA1CNT_H => gba.dma[1].write_control(value),
        REG_DMA2CNT_H => gba.dma[2].write_control(value),
        REG_DMA3CNT_H => gba.dma[3].write_control(value),

        _ => {
            print!("(STUB) ");
        }
    }
}

pub fn read8(gba: &Gba, addr: u32) -> u8 {
    let half = read16(gba, addr & !1);
    (if addr & 1 == 0 {
        half
    } else {
        half >> 8
    }) as u8
}

pub fn read32(gba: &Gba, addr: u32) -> u32 {
    (read16(gba, addr) as u32) | (read16(gba, addr + 2) as u32) << 16
}

pub fn write8(gba: &mut Gba, addr: u32, value: u8) {
    let half = read16(gba, addr & !1);
    let writeback = if addr & 1 == 0 {
        (half & !0xFF) | (value as u16)
    } else {
        (half & 0xFF) | (value as u16) << 8
    };
    write16(gba, addr, writeback);
}

pub fn write32(gba: &mut Gba, addr: u32, value: u32) {
    write16(gba, addr, value as u16);
    write16(gba, addr + 2, (value >> 16) as u16)
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
const REG_BG2X_LO: u32 = 0x4000028;     // 4    W     BG2 Reference Point X-Coordinate
const REG_BG2X_HI: u32 = 0x400002A;
const REG_BG2Y_LO: u32 = 0x400002C;     // 4    W     BG2 Reference Point Y-Coordinate
const REG_BG2Y_HI: u32 = 0x400002E;
const REG_BG3PA: u32 = 0x4000030;       // 2    W     BG3 Rotation/Scaling Parameter A (dx)
const REG_BG3PB: u32 = 0x4000032;       // 2    W     BG3 Rotation/Scaling Parameter B (dmx)
const REG_BG3PC: u32 = 0x4000034;       // 2    W     BG3 Rotation/Scaling Parameter C (dy)
const REG_BG3PD: u32 = 0x4000036;       // 2    W     BG3 Rotation/Scaling Parameter D (dmy)
const REG_BG3X_LO: u32 = 0x4000038;     // 4    W     BG3 Reference Point X-Coordinate
const REG_BG3X_HI: u32 = 0x400003A;
const REG_BG3Y_LO: u32 = 0x400003C;     // 4    W     BG3 Reference Point Y-Coordinate
const REG_BG3Y_HI: u32 = 0x400003E;
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
const REG_DMA0SAD_LO: u32 = 0x40000B0;  // 4    W     DMA 0 Source Address
const REG_DMA0SAD_HI: u32 = 0x40000B2;
const REG_DMA0DAD_LO: u32 = 0x40000B4;  // 4    W     DMA 0 Destination Address
const REG_DMA0DAD_HI: u32 = 0x40000B6;
const REG_DMA0CNT_L: u32 = 0x40000B8;   // 2    W     DMA 0 Word Count
const REG_DMA0CNT_H: u32 = 0x40000BA;   // 2    R/W   DMA 0 Control
const REG_DMA1SAD_LO: u32 = 0x40000BC;  // 4    W     DMA 1 Source Address
const REG_DMA1SAD_HI: u32 = 0x40000BE;
const REG_DMA1DAD_LO: u32 = 0x40000C0;  // 4    W     DMA 1 Destination Address
const REG_DMA1DAD_HI: u32 = 0x40000C2;
const REG_DMA1CNT_L: u32 = 0x40000C4;   // 2    W     DMA 1 Word Count
const REG_DMA1CNT_H: u32 = 0x40000C6;   // 2    R/W   DMA 1 Control
const REG_DMA2SAD_LO: u32 = 0x40000C8;  // 4    W     DMA 2 Source Address
const REG_DMA2SAD_HI: u32 = 0x40000CA;
const REG_DMA2DAD_LO: u32 = 0x40000CC;  // 4    W     DMA 2 Destination Address
const REG_DMA2DAD_HI: u32 = 0x40000CE;
const REG_DMA2CNT_L: u32 = 0x40000D0;   // 2    W     DMA 2 Word Count
const REG_DMA2CNT_H: u32 = 0x40000D2;   // 2    R/W   DMA 2 Control
const REG_DMA3SAD_LO: u32 = 0x40000D4;  // 4    W     DMA 3 Source Address
const REG_DMA3SAD_HI: u32 = 0x40000D6;
const REG_DMA3DAD_LO: u32 = 0x40000D8;  // 4    W     DMA 3 Destination Address
const REG_DMA3DAD_HI: u32 = 0x40000DA;
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