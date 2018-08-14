use std::ops::{ Index, IndexMut };
use std::mem;

use super::renderer::{ Renderer, Background, PHYS_WIDTH, PHYS_HEIGHT };

const TILE_SIZE_4BIT: usize = 0x20;
const TILE_SIZE_8BIT: usize = 0x20;

pub type FrameLine = [u32; PHYS_WIDTH];

pub struct FrameBuffer {
    buffer: Box<[u32; PHYS_HEIGHT * PHYS_WIDTH]>,
}

impl FrameBuffer {
    pub fn new() -> Self {
        FrameBuffer {
            buffer: Box::new([0u32; PHYS_WIDTH * PHYS_HEIGHT]),
        }
    }

    pub fn as_slice(&self) -> &[u32] {
        &self.buffer[..]
    }
}

impl Clone for FrameBuffer {
    fn clone(&self) -> FrameBuffer {
        *self
    }
}

impl Index<usize> for FrameBuffer {
    type Output = FrameLine;
    fn index(&self, index: usize) -> &Self::Output {
        let slice = &self.buffer[index*PHYS_WIDTH..(index+1)*PHYS_WIDTH];
        unsafe {
            mem::transmute::<*const u32, &Self::Output>(slice.as_ptr())
        }
    }
}

impl IndexMut<usize> for FrameBuffer {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let slice = &mut self.buffer[index*PHYS_WIDTH..(index+1)*PHYS_WIDTH];
        unsafe {
            mem::transmute::<*mut u32, &mut Self::Output>(slice.as_mut_ptr())
        }
    }
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub enum Layer {
    Bg0 = 0,
    Bg1 = 1,
    Bg2 = 2,
    Bg3 = 3,
    // Obj
}

pub fn render_line_of_4bit_layer(r: &mut Renderer, layer: Layer, line: &mut FrameLine) {
    let bg: &Background = &r.bg[layer as usize];

    let (bg_width, bg_height) = bg.get_size();
    let map_entries = r.vram.slice_u16(bg.map_base_addr, 0x800 * bg.map_count() as u32);

    let bg_y = (r.scanline as u16 + bg.y_offset) as usize;
    if bg_y > bg_height {
        return;
    }

    let map_screen_index = if bg_y > 255 {
        if bg.map_count() == 4 {
            2
        } else {
            1
        }
    } else {
        0
    };

    let mut x = 0usize;
    while x < PHYS_WIDTH {
        let bg_x = (x + bg.x_offset as usize) & (bg_width - 1);
        let map_screen_index = map_screen_index + if bg_x > 255 { 1 } else { 0 };
        let map_entry_index = ((bg_y & !7) << 2) | (bg_x >> 3);
        let map_entry = map_entries[32*32*map_screen_index + map_entry_index];

        let tile_index = (map_entry & 0x3FF) as u32;
        let hflip_mask = if map_entry & 0x400 != 0 { 7 } else { 0 };
        let vflip_mask = if map_entry & 0x800 != 0 { 7 } else { 0 };
        let palette_index = (map_entry >> 8 & 0xF0) as usize;
        let palette = &r.bg_palette[palette_index..palette_index+16];

        let tile_inner_y = (bg_y & 7) ^ vflip_mask;
        let tile_inner_y_shifted = tile_inner_y << 2;

        let tile_pixels = r.vram.slice_u8(bg.tile_base_addr + tile_index, 16);

        for bg_x_lower in bg_x&7 .. 8 {
            let tile_inner_x = bg_x_lower ^ hflip_mask;
            let pixel_offset = (tile_inner_x >> 1) | tile_inner_y_shifted;
            let pixel_shift = (tile_inner_x & 1) * 4;

            let color_index = tile_pixels[pixel_offset] >> pixel_shift & 0xF;
            if color_index != 0 {
                line[x] = palette[color_index as usize];
            }

            x += 1;
        }
    }
}

pub fn render_line_of_8bit_layer(r: &mut Renderer, layer: Layer, line: &mut FrameLine) {
    let bg: &Background = &r.bg[layer as usize];

    let (bg_width, bg_height) = bg.get_size();
    let map_entries = r.vram.slice_u16(bg.map_base_addr, 0x800 * bg.map_count() as u32);

    let bg_y = (r.scanline as u16 + bg.y_offset) as usize;
    if bg_y > bg_height {
        return;
    }

    let map_screen_index = if bg_y > 255 {
        if bg.map_count() == 4 {
            2
        } else {
            1
        }
    } else {
        0
    };

    let mut x = 0usize;
    while x < PHYS_WIDTH {
        let bg_x = (x + bg.x_offset as usize) & (bg_width - 1);
        let map_screen_index = map_screen_index + if bg_x > 255 { 1 } else { 0 };
        let map_entry_index = ((bg_y & !7) << 2) | (bg_x >> 3);
        let map_entry = map_entries[32*32*map_screen_index + map_entry_index];

        let tile_index = (map_entry & 0x3FF) as u32;
        let hflip_mask = if map_entry & 0x400 != 0 { 7 } else { 0 };
        let vflip_mask = if map_entry & 0x800 != 0 { 7 } else { 0 };

        let tile_inner_y = (bg_y & 7) ^ vflip_mask;
        let tile_inner_y_shifted = tile_inner_y << 3;

        let tile_pixels = r.vram.slice_u8(bg.tile_base_addr + tile_index, 32);

        for bg_x_lower in bg_x&7 .. 8 {
            let tile_inner_x = bg_x_lower ^ hflip_mask;
            let pixel_offset = tile_inner_x | tile_inner_y_shifted;

            let color_index = tile_pixels[pixel_offset];
            if color_index != 0 {
                line[x] = r.bg_palette[color_index as usize];
            }

            x += 1;
        }
    }
}