use super::renderer::Renderer;
use super::mode_common::{Layer, Framebuffer, FrameLine, render_line_of_8bit_layer, render_line_of_4bit_layer };
use crate::renderer::renderer::{Background, BgPalette};
use crate::utils::Buffer;

fn get_draw_order(r: &mut Renderer) -> Box<[Layer]> {
    use self::Layer::*;
    let mut res: Vec<Layer> = vec![Bg0, Bg1, Bg2, Bg3];
    if !r.control.display_bg3 {
        res.remove(3);
    }
    if !r.control.display_bg2 {
        res.remove(2);
    }
    if !r.control.display_bg1 {
        res.remove(1);
    }
    if !r.control.display_bg0 {
        res.remove(0);
    }

    res.sort_by_key(|&l| r.bg[l as usize].priority);

    res.into_boxed_slice()
}

pub fn render_line(r: &mut Renderer, buffer: &mut Framebuffer) {
    let line = &mut buffer[r.scanline as usize];
    for &layer in get_draw_order(r).iter() {
        let bg = &r.bg[layer as usize];
        render_line_of_layer(bg, &r.vram, &r.bg_palette, r.scanline as u16, line);
    }
}

fn render_line_of_layer(
    bg: &Background,
    vram: &Buffer,
    bg_palette: &BgPalette,
    scanline: u16,
    line: &mut FrameLine
) {
    if bg.linear_palettes {
        render_line_of_8bit_layer(bg, vram, bg_palette, scanline, line);
    } else {
        render_line_of_4bit_layer(bg, vram, bg_palette, scanline, line);
    }
}

pub fn render_full_background(r: &mut Renderer, layer: Layer, buffer: &mut Framebuffer) {
    let bg = &r.bg[layer as usize];
    let (width, height) = bg.get_size();
    for i in 0..height {
        let line = &mut buffer[i][..width];
        render_line_of_layer(bg, &r.vram, &r.bg_palette, i as u16, line);
    }
}
