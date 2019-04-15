use super::renderer::Renderer;
use super::mode_common::{Layer, Framebuffer, FrameLine, render_line_of_8bit_layer, render_line_of_4bit_layer };

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
        render_line_of_layer(r, layer, line);
    }
}

fn render_line_of_layer(r: &mut Renderer, layer: Layer, line: &mut FrameLine) {
    if r.bg[layer as usize].linear_palettes {
        render_line_of_8bit_layer(r, layer, line);
    } else {
        render_line_of_4bit_layer(r, layer, line);
    }
}


