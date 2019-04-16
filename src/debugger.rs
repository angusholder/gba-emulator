use std::{thread, ptr};
use std::io;
use std::fmt;
use std::io::{Write, Read};
use std::sync::mpsc;
use std::collections::{ HashMap, HashSet };

use num_traits::{ Num, PrimInt };

use crate::arm7tdmi::{StepEvent, ARM_REGS};
use crate::bus::Bus;
use crate::gba::Gba;
use crate::disassemble::{ disassemble_arm_opcode, disassemble_thumb_opcode };
use crate::log::{ self, LogKind, LogLevel };
use crate::renderer::Framebuffer;
use imgui::{ImGui, Ui, ImStr, ImFontConfig};
use glutin::ContextBuilder;
use glium::Surface;
use std::time::Instant;

fn int<'a, Iter, N: PrimInt>(iter: &mut Iter) -> CommandResult<N>
        where Iter: Iterator<Item=&'a str> {
    let arg = iter.next().ok_or(CommandError::ExpectedArgument)?;
    if arg.starts_with("0x") {
        Num::from_str_radix(&arg[2..], 16)
    } else {
        Num::from_str_radix(&arg[2..], 10)
    }.map_err(|_| CommandError::InvalidInteger)
}

fn opt_int<'a, Iter, N: PrimInt>(iter: &mut Iter) -> CommandResult<Option<N>>
        where Iter: Iterator<Item=&'a str> {
    if let Some(arg) = iter.next() {
        if arg.starts_with("0x") {
            Num::from_str_radix(&arg[2..], 16)
        } else {
            Num::from_str_radix(arg, 10)
        }.map(|n| Some(n)).map_err(|_| CommandError::InvalidInteger)
    } else {
        Ok(None)
    }
}

fn ident<'a, Iter>(iter: &mut Iter) -> CommandResult<String>
        where Iter: Iterator<Item=&'a str> {
    let text = iter.next().ok_or(CommandError::ExpectedArgument)?;
    let mut iter = text.chars();

    match iter.next().unwrap() {
        'a'...'z' | 'A'...'Z' => {}
        _ => return Err(CommandError::InvalidIdentifier)
    }

    while let Some(ch) = iter.next() {
        match ch {
            'a'...'z' | 'A'...'Z' | '0'...'9' => {}
            _ => return Err(CommandError::InvalidIdentifier)
        }
    }

    Ok(text.to_string())
}

fn ident_list<'a, Iter>(iter: &mut Iter) -> CommandResult<Vec<&'a str>>
        where Iter: Iterator<Item=&'a str> {
    let values: &str = iter.next().ok_or(CommandError::ExpectedArgument)?;

    Ok(values.split(",").collect())
}

macro_rules! commands {
    ($(
        $($pat:pat)|+ => ($($var:ident: $argparser:ident),*) = $res:expr
    ),+) => {
        pub fn parse_command(cmd: &str) -> Result<Command, CommandError> {
            let mut iter = cmd.trim().split_whitespace();

            match iter.next().ok_or(CommandError::NoInput)? {
                $(
                    $($pat)|+ => {
                        $(let $var = $argparser(&mut iter)?;)*
                        if iter.next().is_some() {
                            return Err(CommandError::TooManyArguments);
                        }
                        Ok($res)
                    }
                )+

                _ => Err(CommandError::UnknownCommand)
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CommandError {
    NoInput,
    ExpectedArgument,
    UnknownCommand,
    InvalidInteger,
    TooManyArguments,
    InvalidIdentifier,
}

type CommandResult<T> = Result<T, CommandError>;

impl fmt::Display for CommandError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CommandError::*;
        let s = match *self {
            NoInput => "no input",
            ExpectedArgument => "expected argument",
            UnknownCommand => "unknown command",
            InvalidInteger => "invalid integer",
            TooManyArguments => "too many arguments",
            InvalidIdentifier => "invalid identifier",
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    AddBreakpoint(u32),
    AddTempBreakpoint(u32),
    DelBreakpoint(u32),
    ListBreakpoints,

    AddWatchpoint(u32),
    AddTempWatchpoint(u32),
    DelWatchpoint(u32),
    ListWatchpoints,

    Step(usize),
    Continue,
    Goto(u32),
    SaveState(String),
    RestoreState(String),

    LoadB(u32),
    LoadH(u32),
    LoadW(u32),
    StoreB(u32, u8),
    StoreH(u32, u16),
    StoreW(u32, u32),

    Log(LogLevel, Vec<LogKind>),
    LogAll(LogLevel),

    ListRegs,

    Exit,
}

commands! {
    "b" | "break" => (n: int) = Command::AddBreakpoint(n),
    "tb" | "tbreak" => (n: int) = Command::AddTempBreakpoint(n),
    "delbreak" => (n: int) = Command::DelBreakpoint(n),
    "lbreak" => () = Command::ListBreakpoints,

    "w" | "watch" => (n: int) = Command::AddWatchpoint(n),
    "tw" | "twatch" => (n: int) = Command::AddTempWatchpoint(n),
    "delwatch" => (n: int) = Command::DelWatchpoint(n),
    "lwatch" => () = Command::ListWatchpoints,

    "s" | "step" => (n: opt_int) = Command::Step(n.unwrap_or(1usize)),
    "c" | "continue" => () = Command::Continue,
    "goto" => (n: int) = Command::Goto(n),
    "save" => (name: ident) = Command::SaveState(name),
    "restore" => (name: ident) = Command::RestoreState(name),

    "loadb" | "lb" => (addr: int) = Command::LoadB(addr),
    "loadh" | "lh" => (addr: int) = Command::LoadH(addr),
    "loadw" | "lw" => (addr: int) = Command::LoadW(addr),
    "storeb" | "sb" => (addr: int, val: int) = Command::StoreB(addr, val),
    "storeh" | "sh" => (addr: int, val: int) = Command::StoreH(addr, val),
    "storew" | "sw" => (addr: int, val: int) = Command::StoreW(addr, val),

    "lregs" => () = Command::ListRegs,

    "log" => (level_ident: ident, kind_idents: ident_list) = {
        let level = level_ident.parse::<LogLevel>().map_err(|_| CommandError::InvalidIdentifier)?;
        let kinds = kind_idents.iter().map(|s| {
            s.parse().map_err(|_| CommandError::InvalidIdentifier)
        }).collect::<CommandResult<Vec<LogKind>>>()?;
        Command::Log(level, kinds)
    },
    "logall" => (level_ident: ident) = {
        let level = level_ident.parse::<LogLevel>().map_err(|_| CommandError::InvalidIdentifier)?;
        Command::LogAll(level)
    },

    "quit" | "exit" => () = Command::Exit
}

#[derive(PartialEq, Clone, Copy)]
enum State {
    Running,
    Paused,
    Stepping(usize),
    Terminate,
}

#[derive(Clone)]
struct EmulationState {
    gba: Box<Gba>,
}

pub struct Debugger {
    gba: Box<Gba>,
    buffer: Framebuffer,
    save_states: HashMap<String, EmulationState>,
    temp_breakpoints: HashSet<u32>,
    temp_watchpoints: HashSet<u32>,
}

impl Debugger {
    pub fn new(gba: Box<Gba>) -> Debugger {
        Debugger {
            gba,
            buffer: Framebuffer::new(),
            save_states: HashMap::new(),
            temp_breakpoints: HashSet::new(),
            temp_watchpoints: HashSet::new(),
        }
    }

    fn disassemble(&mut self) {
        let thumb_mode = self.gba.arm.cpsr.get_thumb_mode();
        let addr = self.gba.arm.current_pc();
        if thumb_mode {
            let op = self.gba.exec_thumb_slow(addr);
            let dis = disassemble_thumb_opcode(op as u32, addr);
            println!("0x{:07X}: ({:04X}) {}", addr, op, dis);
        } else {
            let op = self.gba.exec_arm_slow(addr);
            let dis = disassemble_arm_opcode(op, addr);
            println!("0x{:07X}: ({:08X}) {}", addr, op, dis);
        }
    }

    fn step(&mut self, print_state: bool) -> bool {
        self.gba.step(&mut self.buffer);

        let event = StepEvent::None;
        if print_state {
            println!("{:?}\n", self.gba.arm);
        }
        match event {
            StepEvent::TriggerWatchpoint(addr) => {
                println!("Watchpoint triggered at {:06X}", addr);
                if self.temp_watchpoints.contains(&addr) {
                    self.temp_watchpoints.remove(&addr);
                    self.gba.arm.watchpoints.remove(addr);
                }
                true
            }
            StepEvent::TriggerBreakpoint(addr) => {
                println!("Breakpoint triggered at {:06X}", addr);
                if self.temp_breakpoints.contains(&addr) {
                    self.temp_breakpoints.remove(&addr);
                    self.gba.arm.breakpoints.remove(addr);
                }
                true
            }
            StepEvent::None => {
                false
            }
        }
    }

    pub fn run(&mut self) {
        let (tx, rx) = mpsc::channel();

        thread::Builder::new().name("stdin poll thread".to_string()).spawn(move || {
            let mut line = String::new();
            let mut running = true;
            while running {
                io::stdin().read_line(&mut line).unwrap();
                let cmd = parse_command(&line);
                if cmd == Ok(Command::Exit) {
                    running = false;
                }
                tx.send(cmd).unwrap();
                line.clear();
            }
        }).expect("failed to spawn thread");

        let mut state = State::Paused;
        let mut last_cmd: Option<Command> = None;
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut events_loop = glutin::EventsLoop::new();
        let window_builder = glutin::WindowBuilder::new()
            .with_maximized(true)
            .with_title("GBA Emulator");
        let context_builder = ContextBuilder::new()
            .with_vsync(true);
        let display = glium::Display::new(window_builder, context_builder, &events_loop).unwrap();
        let window = display.gl_window();

        let mut imgui = ImGui::init();
        // In the examples we only use integer DPI factors, because the UI can get very blurry
        // otherwise. This might or might not be what you want in a real application.
        let hidpi_factor = window.get_hidpi_factor().round();

        let font_size = (13.0 * hidpi_factor) as f32;

        imgui.fonts().add_default_font_with_config(
            ImFontConfig::new()
                .oversample_h(1)
                .pixel_snap_h(true)
                .size_pixels(font_size),
        );
        imgui.set_font_global_scale((1.0 / hidpi_factor) as f32);
        imgui_winit_support::configure_keys(&mut imgui);

        let mut imgui_renderer = imgui_glium_renderer::Renderer::init(&mut imgui, &display).unwrap();

        let mut running = true;
        let mut last_frame = Instant::now();
        while running {
            events_loop.poll_events(|event| {
                imgui_winit_support::handle_event(&mut imgui, &event, window.get_hidpi_factor(), window.get_hidpi_factor().round());

                match event {
                    glutin::Event::WindowEvent {
                        event: glutin::WindowEvent::CloseRequested,
                        ..
                    } => {
                        running = false;
                    },
                    _ => {},
                }
            });

            let now = Instant::now();
            let delta = now - last_frame;
            let delta_s = delta.as_secs() as f32 + delta.subsec_nanos() as f32 / 1_000_000_000.0;
            last_frame = now;

            imgui_winit_support::update_mouse_cursor(&imgui, &window);

            let frame_size = imgui_winit_support::get_frame_size(&window, hidpi_factor).unwrap();

            match state {
                State::Paused => {
                    match rx.try_recv() {
                        Err(mpsc::TryRecvError::Empty) => {}
                        Err(mpsc::TryRecvError::Disconnected) => unimplemented!(),
                        Ok(Err(CommandError::NoInput)) => {
                            if let Some(cmd) = last_cmd.clone() {
                                self.execute_command(cmd);
                            }
                        }
                        Ok(Err(error)) => {
                            println!("Error: {}", error);
                        }
                        Ok(Ok(cmd)) => {
                            state = self.execute_command(cmd.clone());
                            last_cmd = Some(cmd);
                        }
                    }
                }

                State::Running => {
                    loop {
                        let event_occurred = self.step(false);
                        if event_occurred {
                            self.disassemble();
                            state = State::Paused;
                            print!(">> ");
                            io::stdout().flush().unwrap();
                            break;
                        }
                    }
                }

                State::Stepping(n) => {
                    for i in 1..n + 1 {
                        println!("Stepping... {}", i);
                        self.disassemble();
                        let event_occurred = self.step(true);
                        if event_occurred {
                            break;
                        }
                    }

                    state = State::Paused;
                    print!(">> ");
                    io::stdout().flush().unwrap();
                }

                State::Terminate => break,
            }

            let mut frame = display.draw();
            frame.clear_color(0.9, 0.9, 0.9, 1.0);

            let ui = imgui.frame(frame_size, delta_s);
            ui.window(im_str!("CPU"))
                .always_auto_resize(true)
                .build(|| {
                    ui.push_item_width(64.0);
                    for i in 0..8 {
                        input_hex(&ui, im_str!("{}", ARM_REGS[i + 0]), &mut self.gba.arm.regs[i + 0]);
                        ui.same_line_spacing(0.0, 24.0);
                        input_hex(&ui, im_str!("{}", ARM_REGS[i + 8]), &mut self.gba.arm.regs[i + 8]);
                    }
                    ui.pop_item_width();
                });
            imgui_renderer.render(&mut frame, ui).unwrap();

            frame.finish().unwrap();
        }
    }

    fn execute_command(&mut self, cmd: Command) -> State {
        use self::Command::*;
        match cmd {
            AddBreakpoint(addr) => {
                self.gba.arm.breakpoints.insert(addr);
            }
            AddTempBreakpoint(addr) => {
                self.gba.arm.breakpoints.insert(addr);
                self.temp_breakpoints.insert(addr);
            }
            DelBreakpoint(addr) => {
                self.gba.arm.breakpoints.remove(addr);
            }
            AddWatchpoint(addr) => {
                self.gba.arm.watchpoints.insert(addr);
            }
            AddTempWatchpoint(addr) => {
                self.gba.arm.watchpoints.insert(addr);
                self.temp_watchpoints.insert(addr);
            }
            DelWatchpoint(addr) => {
                self.gba.arm.watchpoints.remove(addr);
            }

            ListBreakpoints => {
                for (i, b) in self.gba.arm.breakpoints.iter().enumerate() {
                    println!("  {}: {:06X}", i, b);
                }
            }

            ListWatchpoints => {
                for (i, w) in self.gba.arm.watchpoints.iter().enumerate() {
                    println!("  {}: {:06X}", i, w);
                }
            }

            Step(n) => {
                return State::Stepping(n);
            }
            Continue => {
                return State::Running;
            }
            Goto(addr) => {
                self.gba.arm.branch_to(addr);
            }

            SaveState(name) => {
                self.save_states.insert(name, EmulationState {
                    gba: self.gba.clone(),
                });
            }
            RestoreState(name) => {
                if let Some(mut state) = self.save_states.get(&name).cloned() {
                    state.gba.arm.watchpoints = self.gba.arm.watchpoints.clone();
                    state.gba.arm.breakpoints = self.gba.arm.breakpoints.clone();
                    self.gba = state.gba;
                } else {
                    println!("No save state exists named '{}'", name);
                }
            }

            LoadB(addr) => {
                let val = self.gba.debug_read8(addr).1;
                println!("{} ({:08X})", val, val);
            }
            LoadH(addr) => {
                let val = self.gba.debug_read16(addr).1;
                println!("{} ({:08X})", val, val);
            }
            LoadW(addr) => {
                let val = self.gba.debug_read32(addr).1;
                println!("{} ({:08X})", val, val);
            }

            StoreB(addr, val) => {
                self.gba.debug_write8(addr, val);
            }
            StoreH(addr, val) => {
                self.gba.debug_write16(addr, val);
            }
            StoreW(addr, val) => {
                self.gba.debug_write32(addr, val);
            }

            ListRegs => {
                println!("{:?}", self.gba.arm);
            }

            Log(level, kinds) => {
                for kind in kinds {
                    log::set_log_level(kind, level);
                }
            }
            LogAll(level) => {
                log::set_all_log_levels(level);
            }

            Exit => {
                return State::Terminate;
            }
        }

        State::Paused
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
