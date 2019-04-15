use std::thread;
use std::io;
use std::fmt;
use std::io::Write;
use std::sync::mpsc;
use std::collections::{ HashMap, HashSet };

use num::{ Num, PrimInt };

use arm7tdmi::StepEvent;
use bus::Bus;
use interconnect::Interconnect;
use disassemble::{ disassemble_arm_opcode, disassemble_thumb_opcode };
use log::{ self, LogKind, LogLevel };
use renderer::FrameBuffer;

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
    interconnect: Box<Interconnect>,
}

pub struct Debugger {
    interconnect: Box<Interconnect>,
    buffer: FrameBuffer,
    save_states: HashMap<String, EmulationState>,
    temp_breakpoints: HashSet<u32>,
    temp_watchpoints: HashSet<u32>,
}

impl Debugger {
    pub fn new(interconnect: Box<Interconnect>) -> Debugger {
        Debugger {
            interconnect,
            buffer: FrameBuffer::new(),
            save_states: HashMap::new(),
            temp_breakpoints: HashSet::new(),
            temp_watchpoints: HashSet::new(),
        }
    }

    fn disassemble(&mut self) {
        let thumb_mode = self.interconnect.arm.cpsr.get_thumb_mode();
        let addr = self.interconnect.arm.current_pc();
        if thumb_mode {
            let op = self.interconnect.exec_thumb_slow(addr);
            let dis = disassemble_thumb_opcode(op as u32, addr);
            println!("0x{:07X}: ({:04X}) {}", addr, op, dis);
        } else {
            let op = self.interconnect.exec_arm_slow(addr);
            let dis = disassemble_arm_opcode(op, addr);
            println!("0x{:07X}: ({:08X}) {}", addr, op, dis);
        }
    }

    fn step(&mut self, print_state: bool) -> bool {
        self.interconnect.step(&mut self.buffer);

        let event = StepEvent::None;
        if print_state {
            println!("{:?}\n", self.interconnect.arm);
        }
        match event {
            StepEvent::TriggerWatchpoint(addr) => {
                println!("Watchpoint triggered at {:06X}", addr);
                if self.temp_watchpoints.contains(&addr) {
                    self.temp_watchpoints.remove(&addr);
                    self.interconnect.arm.watchpoints.remove(addr);
                }
                true
            }
            StepEvent::TriggerBreakpoint(addr) => {
                println!("Breakpoint triggered at {:06X}", addr);
                if self.temp_breakpoints.contains(&addr) {
                    self.temp_breakpoints.remove(&addr);
                    self.interconnect.arm.breakpoints.remove(addr);
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

        loop {
            match state {
                State::Paused => {
                    print!(">> ");
                    io::stdout().flush().unwrap();

                    match rx.recv().unwrap() {
                        Err(CommandError::NoInput) => {
                            if let Some(cmd) = last_cmd.take() {
                                self.execute_command(cmd.clone());
                                last_cmd = Some(cmd);
                            }
                        }
                        Err(error) => {
                            println!("Error: {}", error);
                        }
                        Ok(cmd) => {
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
                            break;
                        }
                    }
                }

                State::Stepping(n) => {
                    for i in 1..n+1 {
                        println!("Stepping... {}", i);
                        self.disassemble();
                        let event_occurred = self.step(true);
                        if event_occurred {
                            break;
                        }
                    }

                    state = State::Paused;
                }

                State::Terminate => break,
            }
        }
    }

    fn execute_command(&mut self, cmd: Command) -> State {
        use self::Command::*;
        match cmd {
            AddBreakpoint(addr) => {
                self.interconnect.arm.breakpoints.insert(addr);
            }
            AddTempBreakpoint(addr) => {
                self.interconnect.arm.breakpoints.insert(addr);
                self.temp_breakpoints.insert(addr);
            }
            DelBreakpoint(addr) => {
                self.interconnect.arm.breakpoints.remove(addr);
            }
            AddWatchpoint(addr) => {
                self.interconnect.arm.watchpoints.insert(addr);
            }
            AddTempWatchpoint(addr) => {
                self.interconnect.arm.watchpoints.insert(addr);
                self.temp_watchpoints.insert(addr);
            }
            DelWatchpoint(addr) => {
                self.interconnect.arm.watchpoints.remove(addr);
            }

            ListBreakpoints => {
                for (i, b) in self.interconnect.arm.breakpoints.iter().enumerate() {
                    println!("  {}: {:06X}", i, b);
                }
            }

            ListWatchpoints => {
                for (i, w) in self.interconnect.arm.watchpoints.iter().enumerate() {
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
                self.interconnect.arm.branch_to(addr);
            }

            SaveState(name) => {
                self.save_states.insert(name, EmulationState {
                    interconnect: self.interconnect.clone(),
                });
            }
            RestoreState(name) => {
                if let Some(mut state) = self.save_states.get(&name).cloned() {
                    state.interconnect.arm.watchpoints = self.interconnect.arm.watchpoints.clone();
                    state.interconnect.arm.breakpoints = self.interconnect.arm.breakpoints.clone();
                    self.interconnect = state.interconnect;
                } else {
                    println!("No save state exists named '{}'", name);
                }
            }

            LoadB(addr) => {
                let val = self.interconnect.debug_read8(addr).1;
                println!("{} ({:08X})", val, val);
            }
            LoadH(addr) => {
                let val = self.interconnect.debug_read16(addr).1;
                println!("{} ({:08X})", val, val);
            }
            LoadW(addr) => {
                let val = self.interconnect.debug_read32(addr).1;
                println!("{} ({:08X})", val, val);
            }

            StoreB(addr, val) => {
                self.interconnect.debug_write8(addr, val);
            }
            StoreH(addr, val) => {
                self.interconnect.debug_write16(addr, val);
            }
            StoreW(addr, val) => {
                self.interconnect.debug_write32(addr, val);
            }

            ListRegs => {
                println!("{:?}", self.interconnect.arm);
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
