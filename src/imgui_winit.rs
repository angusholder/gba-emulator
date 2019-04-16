use imgui::{FrameSize, ImGui, ImGuiKey, Ui};
use std::time::Instant;
use glutin::{Event, Window};

#[derive(Debug, Clone)]
pub struct ImGuiWinit {
    pressed: [bool; 5],
    last_frame: Instant,
}

impl ImGuiWinit {
    pub fn new(imgui: &mut ImGui) -> ImGuiWinit {
        use glutin::VirtualKeyCode as Key;
        imgui.set_imgui_key(ImGuiKey::Tab, Key::Tab as u8);
        imgui.set_imgui_key(ImGuiKey::LeftArrow, Key::Left as u8);
        imgui.set_imgui_key(ImGuiKey::RightArrow, Key::Right as u8);
        imgui.set_imgui_key(ImGuiKey::UpArrow, Key::Up as u8);
        imgui.set_imgui_key(ImGuiKey::DownArrow, Key::Down as u8);
        imgui.set_imgui_key(ImGuiKey::PageUp, Key::PageUp as u8);
        imgui.set_imgui_key(ImGuiKey::PageDown, Key::PageDown as u8);
        imgui.set_imgui_key(ImGuiKey::Home, Key::Home as u8);
        imgui.set_imgui_key(ImGuiKey::End, Key::End as u8);
        imgui.set_imgui_key(ImGuiKey::Delete, Key::Delete as u8);
        imgui.set_imgui_key(ImGuiKey::Backspace, Key::Back as u8);
        imgui.set_imgui_key(ImGuiKey::Enter, Key::Return as u8);
        imgui.set_imgui_key(ImGuiKey::Escape, Key::Escape as u8);
        imgui.set_imgui_key(ImGuiKey::A, Key::A as u8);
        imgui.set_imgui_key(ImGuiKey::C, Key::C as u8);
        imgui.set_imgui_key(ImGuiKey::V, Key::V as u8);
        imgui.set_imgui_key(ImGuiKey::X, Key::X as u8);
        imgui.set_imgui_key(ImGuiKey::Y, Key::Y as u8);
        imgui.set_imgui_key(ImGuiKey::Z, Key::Z as u8);

        ImGuiWinit {
            pressed: [false; 5],
            last_frame: Instant::now(),
        }
    }

    /// Calls `ImGui::frame` with correct parameters for the window
    /// size and frame delta.
    pub fn frame<'ui>(&mut self, imgui: &'ui mut ImGui, window: &Window) -> Ui<'ui> {
        let now = Instant::now();
        let delta_time = now.duration_since(self.last_frame);
        let delta_s =
            delta_time.as_secs() as f32 + delta_time.subsec_nanos() as f32 / 1_000_000_000.0;
        self.last_frame = now;
        let physical_size = window
            .get_inner_size()
            .unwrap()
            .to_physical(window.get_hidpi_factor());
        let hidpi_factor = window.get_hidpi_factor().round();
        let logical_size = physical_size.to_logical(hidpi_factor);
        let frame_size = FrameSize {
            logical_size: logical_size.into(),
            hidpi_factor,
        };
        imgui.frame(frame_size, delta_s)
    }

    /// Passes events from `winit`'s `EventLoop` to an `ImGui` instance.
    ///
    /// This should be called from within `EventLoop::poll_events`.
    pub fn handle_event(&mut self, imgui: &mut ImGui, event: &Event) {
        use glutin::ElementState::Pressed;
        use glutin::WindowEvent::*;
        use glutin::{Event, MouseButton, MouseScrollDelta, TouchPhase};

        let scale = imgui.display_framebuffer_scale();

        if let Event::WindowEvent { event, .. } = event {
            match event {
                KeyboardInput { input, .. } => {
                    use glutin::VirtualKeyCode as Key;

                    let pressed = input.state == Pressed;
                    if let Some(key) = input.virtual_keycode {
                        imgui.set_key(key as u8, pressed);
                    }
                    match input.virtual_keycode {
                        Some(Key::LControl) | Some(Key::RControl) => imgui.set_key_ctrl(pressed),
                        Some(Key::LShift) | Some(Key::RShift) => imgui.set_key_shift(pressed),
                        Some(Key::LAlt) | Some(Key::RAlt) => imgui.set_key_alt(pressed),
                        Some(Key::LWin) | Some(Key::RWin) => imgui.set_key_super(pressed),
                        _ => {}
                    }
                }
                CursorMoved { position: pos, .. } => {
                    let (x, y): (f64, f64) = (*pos).into();
                    imgui.set_mouse_pos(x as f32 / scale.0, y as f32 / scale.1);
                }
                MouseInput { state, button, .. } => {
                    match button {
                        MouseButton::Left => self.pressed[0] = *state == Pressed,
                        MouseButton::Right => self.pressed[1] = *state == Pressed,
                        MouseButton::Middle => self.pressed[2] = *state == Pressed,
                        MouseButton::Other(idx) if *idx < 5 => {
                            self.pressed[*idx as usize] = *state == Pressed
                        }
                        _ => (),
                    };
                    imgui.set_mouse_down([
                        self.pressed[0],
                        self.pressed[1],
                        self.pressed[2],
                        self.pressed[3],
                        self.pressed[4],
                    ])
                }
                MouseWheel {
                    delta: MouseScrollDelta::LineDelta(_, y),
                    phase: TouchPhase::Moved,
                    ..
                } => imgui.set_mouse_wheel(y / scale.1),
                MouseWheel {
                    delta: MouseScrollDelta::PixelDelta(pos),
                    phase: TouchPhase::Moved,
                    ..
                } => imgui.set_mouse_wheel(pos.y as f32 / scale.1),
                MouseWheel {
                    phase: TouchPhase::Ended,
                    ..
                } => imgui.set_mouse_wheel(0.0),
                ReceivedCharacter(c) => imgui.add_input_character(*c),
                _ => (),
            }
        }
    }
}
