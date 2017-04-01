use std::str::FromStr;
use std::thread;
use std::io;
use std::fmt;
use std::io::Write;
use std::sync::mpsc;
use std::collections::{ HashMap, HashSet };

use num::Num;

use arm7tdmi::{ StepEvent, Arm7TDMI };
use interconnect::Interconnect;
use disassemble::{ disassemble_arm_opcode, disassemble_thumb_opcode };

macro_rules! parse_argument {
    ($iter:expr, int) => {{
        let arg = $iter.next().ok_or(CommandError::ExpectedArgument)?;
        if arg.starts_with("0x") {
            Num::from_str_radix(&arg[2..], 16).map_err(|_| CommandError::InvalidInteger)?
        } else {
            FromStr::from_str(arg).map_err(|_| CommandError::InvalidInteger)?
        }
    }};
    ($iter:expr, opt_int) => {{
        if let Some(arg) = $iter.next() {
            Some(if arg.starts_with("0x") {
                Num::from_str_radix(&arg[2..], 16).map_err(|_| CommandError::InvalidInteger)?
            } else {
                FromStr::from_str(arg).map_err(|_| CommandError::InvalidInteger)?
            })
        } else {
            None
        }
    }};
    ($iter:expr, ident) => {{
        let text = $iter.next().ok_or(CommandError::ExpectedArgument)?;
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

        text.to_string()
    }};
}

macro_rules! commands {
    ($(
        $($pat:pat)|+ => ($($var:ident: $argparser:ident),*) = $res:expr
    ),+) => {
        pub fn parse_command(cmd: &str) -> Result<Command, CommandError> {
            let mut iter = cmd.trim().split_whitespace();
            let cmd = match iter.next().ok_or(CommandError::NoInput)? {
                $($($pat)|+ => {
                    $(let $var = parse_argument!(iter, $argparser);)*
                    if iter.next().is_some() {
                        return Err(CommandError::TooManyArguments);
                    }
                    $res
                })+

                _ => {
                    return Err(CommandError::UnknownCommand);
                }
            };

            Ok(cmd)
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

    Disassemble(u32),
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

    "dis" | "disassemble" => (n: opt_int) = Command::Disassemble(n.unwrap_or(4)),
    "lregs" => () = Command::ListRegs,

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
    arm: Arm7TDMI,
    interconnect: Interconnect,
}

pub struct Debugger {
    arm: Arm7TDMI,
    interconnect: Interconnect,
    save_states: HashMap<String, EmulationState>,
    temp_breakpoints: HashSet<u32>,
    temp_watchpoints: HashSet<u32>,
}

impl Debugger {
    pub fn new(arm: Arm7TDMI, interconnect: Interconnect) -> Debugger {
        Debugger {
            arm: arm,
            interconnect: interconnect,
            save_states: HashMap::new(),
            temp_breakpoints: HashSet::new(),
            temp_watchpoints: HashSet::new(),
        }
    }

    fn dis(&mut self) {
        let thumb_mode = self.arm.cpsr.thumb_mode;
        let addr = self.arm.current_pc();
        if thumb_mode {
            let (_, op) = self.interconnect.exec_thumb_slow(addr);
            let dis = disassemble_thumb_opcode(op as u32, addr);
            println!("0x{:07X}: ({:04X}) {}", addr, op, dis);
        } else {
            let (_, op) = self.interconnect.exec_arm_slow(addr);
            let dis = disassemble_arm_opcode(op, addr);
            println!("0x{:07X}: ({:08X}) {}", addr, op, dis);
        };

    }

    fn step(&mut self, print_state: bool) -> bool {
        let event = self.arm.step(&mut self.interconnect);
        if print_state {
            println!("{:?}\n", self.arm);
        }
        match event {
            StepEvent::TriggerWatchpoint(addr) => {
                println!("Watchpoint triggered at {:06X}", addr);
                if self.temp_watchpoints.contains(&addr) {
                    self.temp_watchpoints.remove(&addr);
                    self.arm.watchpoints.remove(addr);
                }
                true
            }
            StepEvent::TriggerBreakpoint(addr) => {
                println!("Breakpoint triggered at {:06X}", addr);
                if self.temp_breakpoints.contains(&addr) {
                    self.temp_breakpoints.remove(&addr);
                    self.arm.breakpoints.remove(addr);
                }
                true
            }
            StepEvent::None => {
                false
            }
        }
    }

    pub fn run(&mut self) {
        let mut state = State::Paused;

        let (tx, rx) = mpsc::channel();
        thread::Builder::new().name("stdin poll thread".to_string()).spawn(move || {
            let stdin = io::stdin();
            let mut line = String::new();
            let mut running = true;
            while running {
                stdin.read_line(&mut line).unwrap();
                let cmd = parse_command(&line);
                if cmd == Ok(Command::Exit) {
                    running = false;
                }
                tx.send(cmd).unwrap();
                line.clear();
            }
        }).expect("failed to spawn thread");

        let mut last_cmd: Option<Command> = None;

        loop {
            match state {
                State::Paused => {
                    while state == State::Paused {
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
                }

                State::Running => {
                    loop {
                        if self.step(false) {
                            self.dis();
                            state = State::Paused;
                            break;
                        }
                    }
                }

                State::Stepping(n) => {
                    for i in 1..n+1 {
                        println!("Stepping... {}", i);
                        self.dis();
                        if self.step(true) {
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
                self.arm.breakpoints.insert(addr);
            }
            AddTempBreakpoint(addr) => {
                self.arm.breakpoints.insert(addr);
                self.temp_breakpoints.insert(addr);
            }
            DelBreakpoint(addr) => {
                self.arm.breakpoints.remove(addr);
            }
            AddWatchpoint(addr) => {
                self.arm.watchpoints.insert(addr);
            }
            AddTempWatchpoint(addr) => {
                self.arm.watchpoints.insert(addr);
                self.temp_watchpoints.insert(addr);
            }
            DelWatchpoint(addr) => {
                self.arm.watchpoints.remove(addr);
            }

            ListBreakpoints => {
                for (i, b) in self.arm.breakpoints.iter().enumerate() {
                    println!("  {}: {:06X}", i, b);
                }
            }

            ListWatchpoints => {
                for (i, w) in self.arm.watchpoints.iter().enumerate() {
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
                self.arm.branch_to(&mut self.interconnect, addr);
            }

            SaveState(name) => {
                self.save_states.insert(name, EmulationState {
                    arm: self.arm.clone(),
                    interconnect: self.interconnect.clone(),
                });
            }
            RestoreState(name) => {
                if let Some(mut state) = self.save_states.get(&name).cloned() {
                    state.arm.watchpoints = self.arm.watchpoints.clone();
                    state.arm.breakpoints = self.arm.breakpoints.clone();
                    self.arm = state.arm;
                    self.interconnect = state.interconnect;
                } else {
                    println!("No save state exists named '{}'", name);
                }
            }

            LoadB(addr) => {
                let val = self.interconnect.read8(addr).1;
                println!("{} ({:08X})", val, val);
            }
            LoadH(addr) => {
                let val = self.interconnect.read16(addr).1;
                println!("{} ({:08X})", val, val);
            }
            LoadW(addr) => {
                let val = self.interconnect.read32(addr).1;
                println!("{} ({:08X})", val, val);
            }

            StoreB(addr, val) => {
                self.interconnect.write8(addr, val);
            }
            StoreH(addr, val) => {
                self.interconnect.write16(addr, val);
            }
            StoreW(addr, val) => {
                self.interconnect.write32(addr, val);
            }

            Disassemble(_count) => {
                unimplemented!();
            }
            ListRegs => {
                println!("{:?}", self.arm);
            }

            Exit => {
                return State::Terminate;
            }
        }

        State::Paused
    }
}
