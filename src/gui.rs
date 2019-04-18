use glium::{Display, Surface};
use glutin::{EventsLoop, WindowBuilder, ContextBuilder, Event, WindowEvent};
use imgui::{ImGui, ImFontConfig, Ui, ImStr};
use imgui_glium_renderer::Renderer;
use std::time::Instant;
use std::ptr;

use crate::arm7tdmi::ARM_REGS;
use crate::gba::Gba;

pub struct Gui {
    display: Display,
    imgui: ImGui,
    renderer: Renderer,
    pub running: bool,
    last_frame: Instant,
    hidpi_factor: f64,
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

        Gui {
            display,
            imgui,
            renderer,
            running: true,
            last_frame: Instant::now(),
            hidpi_factor,
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
}



fn input_hex(_: &Ui, label: &ImStr, value: &mut u32) {
    let reg_ptr = value as *mut u32 as *mut _;
    unsafe {
        imgui::sys::igInputScalar(
            label.as_ptr(), imgui::sys::ImGuiDataType::U32, reg_ptr,
            ptr::null(), ptr::null(), im_str!("%08X").as_ptr(), imgui::ImGuiInputTextFlags::empty());
    }
}