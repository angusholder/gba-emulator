use utils::MyInto;

bitfield_struct! {
    LCDControl: u16 {
        (0,3) bg_mode: u8,
        (3,1) cgb_mode: bool,
        (4,1) display_frame_select: u8,
        (5,1) hblank_interval_free: bool,
        (6,1) obj_vram_1d_map: bool,
        (7,1) forced_blank: bool,
        (8,1) display_bg0: bool,
        (9,1) display_bg1: bool,
        (10,1)display_bg2: bool,
        (11,1)display_bg3: bool,
        (12,1)display_obj: bool,
        (13,1)display_window_0: bool,
        (14,1)display_window_1: bool,
        (15,1)display_obj_window: bool,
    }

    LCDStatus: u16 {
        (0,1) vblank: bool,
        (1,1) hblank: bool,
        (2,1) vcounter: bool,
        (3,1) vblank_irq_enable: bool,
        (4,1) hblank_irq_enable: bool,
        (5,1) vcounter_irq_enable: bool,
    }

    VerticalCounter: u16 {
        (0,8) current_scanline: u8,
    }

    BackgroundControl: u16 {
        (0,2) bg_priority: u8,
        (2,2) character_base_block: u8,
        (6,1) mosaic: bool,
        (7,1) colors_palettes: u8,
        (8,5) screen_base_block: u8,
        (13,1)display_area_overflow: bool,
        (14,2)screen_size: u8,
    }

    BackgroundScroll: u16 {
        (0,9) offset: u16,
    }

    FixedPoint: u32 {
        (0,8) fraction: u8,
        (8,19)integer: u32,
        (27,1)sign: bool,
    }

    IrqFlags: u16 {
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
}

enum BackgoundMode {
    //            Rot/Scal Layers Size               Tiles Colors       Features
    Mode0, //     No       0123   256x256..512x515   1024  16/16..256/1 SFMABP
    Mode1, //     Mixed    012-   (BG0,BG1 as above Mode 0, BG2 as below Mode 2)
    Mode2, //     Yes      --23   128x128..1024x1024 256   256/1        S-MABP
    Mode3, //     Yes      --2-   240x160            1     32768        --MABP
    Mode4, //     Yes      --2-   240x160            2     256/1        --MABP
    Mode5, //     Yes      --2-   160x128            2     32768        --MABP
}
