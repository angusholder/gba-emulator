use std::ops::{ Index, IndexMut };

use super::renderer::{Background, PHYS_WIDTH};
use crate::utils::Buffer;
use crate::renderer::renderer::BgPalette;

const TILE_SIZE_4BIT: usize = 0x20;
const TILE_SIZE_8BIT: usize = 0x20;

pub type FrameLine = [u32];

#[derive(Clone)]
pub struct Framebuffer {
    buffer: Box<[u32]>,
    width: usize,
    height: usize,
}

impl Framebuffer {
    pub fn new(width: usize, height: usize) -> Self {
        Framebuffer {
            buffer: vec![0u32; width * height].into_boxed_slice(),
            width, height,
        }
    }

    pub fn as_slice(&self) -> &[u32] {
        &self.buffer[..]
    }
}
impl Index<usize> for Framebuffer {
    type Output = FrameLine;
    fn index(&self, index: usize) -> &Self::Output {
        &self.buffer[index*self.width..(index+1)*self.width]
    }
}

impl IndexMut<usize> for Framebuffer {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.buffer[index*self.width..(index+1)*self.width]
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

pub fn render_line_of_4bit_layer(
    bg: &Background,
    vram: &Buffer,
    bg_palette: &BgPalette,
    scanline: u16,
    line: &mut FrameLine
) {
    let (bg_width, bg_height) = bg.get_size();
    let map_entries = vram.slice_u16(bg.map_base_addr, 0x800 * bg.map_count() as u32);

    let bg_y = (scanline + bg.y_offset) as usize;
    // "All four BG planes wrap when they reach their right or bottom edges"
    let bg_y = bg_y & (bg_height - 1);

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
        // "All four BG planes wrap when they reach their right or bottom edges"
        let bg_x = (x + bg.x_offset as usize) & (bg_width - 1);
        let map_screen_index = map_screen_index + if bg_x > 255 { 1 } else { 0 };
        let map_entry_index = ((bg_y & !7) << 2) | (bg_x >> 3);
        let map_entry = map_entries[32*32*map_screen_index + map_entry_index];

        let tile_index = (map_entry & 0x3FF) as u32;
        let hflip_mask = if map_entry & 0x400 != 0 { 7 } else { 0 };
        let vflip_mask = if map_entry & 0x800 != 0 { 7 } else { 0 };
        let palette_index = (map_entry >> 8 & 0xF0) as usize;
        let palette = &bg_palette[palette_index..palette_index+16];

        let tile_inner_y = (bg_y & 7) ^ vflip_mask;
        let tile_inner_y_shifted = tile_inner_y << 2;

        // "Each tile occupies 32 bytes of memory, the first 4 bytes for the topmost row of the tile, and so on"
        let tile_pixels = vram.slice_u8(bg.tile_base_addr + tile_index, 32);

        for bg_x_lower in bg_x&7 .. 8 {
            let tile_inner_x = bg_x_lower ^ hflip_mask;
            let pixel_offset = (tile_inner_x >> 1) | tile_inner_y_shifted;
            let pixel_shift = (tile_inner_x & 1) * 4;

            // "Each byte representing two dots, the lower 4 bits define the color for the left
            // (!) dot, the upper 4 bits the color for the right dot"
            let color_index = tile_pixels[pixel_offset] >> pixel_shift & 0xF;
            if color_index != 0 {
                line[x] = palette[color_index as usize];
            }

            x += 1;
        }
    }
}

pub fn render_line_of_8bit_layer(
    bg: &Background,
    vram: &Buffer,
    bg_palette: &BgPalette,
    scanline: u16,
    line: &mut FrameLine
) {
    let (bg_width, bg_height) = bg.get_size();
    let map_entries = vram.slice_u16(bg.map_base_addr, 0x800 * bg.map_count() as u32);

    let bg_y = (scanline + bg.y_offset) as usize;
    // "All four BG planes wrap when they reach their right or bottom edges"
    let bg_y = bg_y & (bg_height - 1);

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
        // "All four BG planes wrap when they reach their right or bottom edges"
        let bg_x = (x + bg.x_offset as usize) & (bg_width - 1);
        let map_screen_index = map_screen_index + if bg_x > 255 { 1 } else { 0 };
        let map_entry_index = ((bg_y & !7) << 2) | (bg_x >> 3);
        let map_entry = map_entries[32*32*map_screen_index + map_entry_index];

        let tile_index = (map_entry & 0x3FF) as u32;
        let hflip_mask = if map_entry & 0x400 != 0 { 7 } else { 0 };
        let vflip_mask = if map_entry & 0x800 != 0 { 7 } else { 0 };

        let tile_inner_y = (bg_y & 7) ^ vflip_mask;
        let tile_inner_y_shifted = tile_inner_y << 3;

        // "Each tile occupies 64 bytes of memory, the first 8 bytes for the topmost row of the tile, and so on"
        let tile_pixels = vram.slice_u8(bg.tile_base_addr + tile_index, 64);

        for bg_x_lower in bg_x&7 .. 8 {
            let tile_inner_x = bg_x_lower ^ hflip_mask;
            let pixel_offset = tile_inner_x | tile_inner_y_shifted;

            let color_index = tile_pixels[pixel_offset];
            if color_index != 0 {
                line[x] = bg_palette[color_index as usize];
            }

            x += 1;
        }
    }
}
