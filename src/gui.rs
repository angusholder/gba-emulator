use glium::{Display, Surface};
use glium::texture::{Texture2d, UncompressedFloatFormat, MipmapsOption, RawImage2d, ClientFormat};
use glutin::{EventsLoop, WindowBuilder, ContextBuilder, Event, WindowEvent};
use imgui::{ImGui, ImFontConfig, Ui, ImStr, ImTexture};
use imgui_glium_renderer::Renderer;
use std::time::Instant;
use std::ptr;

use crate::arm7tdmi::ARM_REGS;
use crate::gba::Gba;
use crate::renderer::{mode0, Framebuffer};
use crate::renderer::mode_common::Layer;
use std::borrow::Cow;

pub struct Gui {
    display: Display,
    imgui: ImGui,
    renderer: Renderer,
    pub running: bool,
    last_frame: Instant,
    hidpi_factor: f64,
    render_tile_maps: Box<FnMut(&mut Gba, &Ui, &mut Renderer, &Display)>,
}

impl Gui {
    pub fn new(events_loop: &EventsLoop) -> Gui {
        let window_builder = WindowBuilder::new()
            .with_maximized(true)
            .with_title("GBA Emulator");
        let context_builder = ContextBuilder::new()
            .with_vsync(true);
        let display = Display::new(window_builder, context_builder, &events_loop).unwrap();

        let mut imgui = ImGui::init();
        // In the examples we only use integer DPI factors, because the UI can get very blurry
        // otherwise. This might or might not be what you want in a real application.
        let hidpi_factor = display.gl_window().get_hidpi_factor().round();

        let font_size = (13.0 * hidpi_factor) as f32;

        imgui.fonts().add_default_font_with_config(
            ImFontConfig::new()
                .oversample_h(1)
                .pixel_snap_h(true)
                .size_pixels(font_size),
        );
        imgui.set_font_global_scale((1.0 / hidpi_factor) as f32);
        imgui_winit_support::configure_keys(&mut imgui);

        let renderer = Renderer::init(&mut imgui, &display).unwrap();

        let render_tile_maps = Self::render_tile_maps();

        Gui {
            display,
            imgui,
            renderer,
            running: true,
            last_frame: Instant::now(),
            hidpi_factor,
            render_tile_maps,
        }
    }

    pub fn update(&mut self, events_loop: &mut EventsLoop, gba: &mut Gba) {
        events_loop.poll_events(|event| {
            let window = self.display.gl_window();
            imgui_winit_support::handle_event(&mut self.imgui, &event, window.get_hidpi_factor(), window.get_hidpi_factor().round());

            match event {
                Event::WindowEvent {
                    event: WindowEvent::CloseRequested,
                    ..
                } => {
                    self.running = false;
                },
                _ => {},
            }
        });

        let window = self.display.gl_window();

        let now = Instant::now();
        let delta = now - self.last_frame;
        let delta_s = delta.as_secs() as f32 + delta.subsec_nanos() as f32 / 1_000_000_000.0;
        self.last_frame = now;

        imgui_winit_support::update_mouse_cursor(&self.imgui, &window);

        let frame_size = imgui_winit_support::get_frame_size(&window, self.hidpi_factor).unwrap();

        let mut frame = self.display.draw();
        frame.clear_color(0.9, 0.9, 0.9, 1.0);

        let ui = self.imgui.frame(frame_size, delta_s);
        Self::render_cpu(gba, &ui);
        (self.render_tile_maps)(gba, &ui, &mut self.renderer, &self.display);
        self.renderer.render(&mut frame, ui).unwrap();

        frame.finish().unwrap();
    }

    fn render_cpu(gba: &mut Gba, ui: &Ui) {
        ui.window(im_str!("CPU"))
            .always_auto_resize(true)
            .build(|| {
                ui.push_item_width(64.0);
                for i in 0..8 {
                    input_hex(&ui, im_str!("{}", ARM_REGS[i + 0]), &mut gba.arm.regs[i + 0]);
                    ui.same_line_spacing(0.0, 24.0);
                    input_hex(&ui, im_str!("{}", ARM_REGS[i + 8]), &mut gba.arm.regs[i + 8]);
                }
                ui.pop_item_width();
            });
    }

    fn render_tile_maps() -> Box<FnMut(&mut Gba, &Ui, &mut Renderer, &Display)> {
        use crate::renderer::mode_common::Layer::*;
        let mut buffer = Framebuffer::new(512, 512);

        let mut layers: [(Layer, Option<ImTexture>); 4] = [
            (Bg0, None),
            (Bg1, None),
            (Bg2, None),
            (Bg3, None),
        ];

        Box::new(move |gba, ui, renderer, display| {
            for (layer, texture_id) in layers.iter_mut() {
                mode0::render_full_background(&mut gba.renderer, *layer, &mut buffer);
                let buffer_u32_slice: &[u32] = buffer.as_slice();
                let buffer_u8_slice: &[u8] = unsafe {
                    std::slice::from_raw_parts(
                        buffer_u32_slice.as_ptr() as *const u8,
                        buffer_u32_slice.len() * std::mem::size_of::<u32>())
                };
                let raw = RawImage2d {
                    data: Cow::Borrowed(buffer_u8_slice),
                    width: 512,
                    height: 512,
                    format: ClientFormat::U8U8U8U8,
                };
                let texture = Texture2d::with_format(
                    display,
                    raw,
                    UncompressedFloatFormat::U8U8U8U8,
                    MipmapsOption::NoMipmap).unwrap();
                let id = if let Some(id) = *texture_id {
                    renderer.textures().replace(id, texture);
                    id
                } else {
                    let id = renderer.textures().insert(texture);
                    *texture_id = Some(id);
                    id
                };
                ui.text(&format!("Background {}", *layer as i32));
                let (w, h) = gba.renderer.bg[*layer as usize].get_size();
                let (w, h) = (w as f32, h as f32);
                ui.image(id, (w, h))
                    .uv1((512.0 / w, 512.0 / h))
                    .build();
            }
        })
    }
}



fn input_hex(_: &Ui, label: &ImStr, value: &mut u32) {
    let reg_ptr = value as *mut u32 as *mut _;
    unsafe {
        imgui::sys::igInputScalar(
            label.as_ptr(), imgui::sys::ImGuiDataType::U32, reg_ptr,
            ptr::null(), ptr::null(), im_str!("%08X").as_ptr(), imgui::ImGuiInputTextFlags::empty());
    }
}