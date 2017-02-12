use std::str::FromStr;
use num::Num;

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

#[derive(Debug)]
pub enum CommandError {
    NoInput,
    ExpectedArgument,
    UnknownCommand,
    InvalidInteger,
    TooManyArguments,
    InvalidIdentifier,
}

#[derive(Debug)]
pub enum Command {
    AddBreakpoint(u32),
    AddTempBreakpoint(u32),
    DelBreakpoint(u32),
    ListBreakpoints,

    AddWatchpoint(u32),
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
}

commands! {
    "b" | "break" => (n: int) = Command::AddBreakpoint(n),
    "tbreak" => (n: int) = Command::AddTempBreakpoint(n),
    "delbreak" => (n: int) = Command::DelBreakpoint(n),
    "lbreak" => () = Command::ListBreakpoints,

    "w" | "watch" => (n: int) = Command::AddWatchpoint(n),
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
    "lregs" => () = Command::ListRegs
}
