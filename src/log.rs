use std::fmt;
use std::str::FromStr;

pub use self::LogLevel::{ Trace, Note, Warn, Error };
pub use self::LogKind::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LogLevel {
    Trace = 0,
    Note = 1,
    Warn = 2,
    Error = 3,
    None = 4,
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            LogLevel::None => "NONE",
            LogLevel::Trace => "TRACE",
            LogLevel::Note => "NOTE",
            LogLevel::Warn => "WARN",
            LogLevel::Error => "ERROR",
        })
    }
}

impl FromStr for LogLevel {
    type Err = ();
    fn from_str(s: &str) -> Result<LogLevel, Self::Err> {
        let s = s.to_ascii_uppercase();
        Ok(match &s[..] {
            "NONE" => LogLevel::None,
            "TRACE" => LogLevel::Trace,
            "NOTE" => LogLevel::Note,
            "WARN" => LogLevel::Warn,
            "ERROR" => LogLevel::Error,
            _ => return Err(())
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LogKind {
    CPU,
    GPU,
    SPU,
    IO,
    DMA0,
    DMA1,
    DMA2,
    DMA3,
    TM0,
    TM1,
    TM2,
    TM3,
    BIOS,
    GDB,
}

impl fmt::Display for LogKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            LogKind::CPU => "CPU",
            LogKind::GPU => "GPU",
            LogKind::SPU => "SPU",
            LogKind::IO => "IO",
            LogKind::DMA0 => "DMA0",
            LogKind::DMA1 => "DMA1",
            LogKind::DMA2 => "DMA2",
            LogKind::DMA3 => "DMA3",
            LogKind::TM0 => "TM0",
            LogKind::TM1 => "TM1",
            LogKind::TM2 => "TM2",
            LogKind::TM3 => "TM3",
            LogKind::BIOS => "BIOS",
            LogKind::GDB => "GDB",
        })
    }
}

impl FromStr for LogKind {
    type Err = ();
    fn from_str(s: &str) -> Result<LogKind, Self::Err> {
        let s = s.to_ascii_uppercase();
        Ok(match &s[..] {
            "CPU" => LogKind::CPU,
            "GPU" => LogKind::GPU,
            "SPU" => LogKind::SPU,
            "IO" => LogKind::IO,
            "DMA0" => LogKind::DMA0,
            "DMA1" => LogKind::DMA1,
            "DMA2" => LogKind::DMA2,
            "DMA3" => LogKind::DMA3,
            "TM0" => LogKind::TM0,
            "TM1" => LogKind::TM1,
            "TM2" => LogKind::TM2,
            "TM3" => LogKind::TM3,
            "BIOS" => LogKind::BIOS,
            _ => return Err(())
        })
    }
}

macro_rules! log_ {
    ($level:expr, $kind:expr, $fmt:expr, $($arg:expr),*) => {
        if $level >= crate::log::get_log_level($kind) {
            println!("{:>5}[{:<4}]: {}", $level, $kind, format_args!($fmt, $($arg),*));
        }
    }
}

macro_rules! trace {
    ($kind:expr, $fmt:expr) => {
        trace!($kind, $fmt,);
    };
    ($kind:expr, $fmt:expr, $($arg:expr),*) => {
        log_!(crate::log::LogLevel::Trace, $kind, $fmt, $($arg),*);
    }
}

macro_rules! note {
    ($kind:expr, $fmt:expr) => {
        note!($kind, $fmt,);
    };
    ($kind:expr, $fmt:expr, $($arg:expr),*) => {
        log_!(crate::log::LogLevel::Note, $kind, $fmt, $($arg),*);
    }
}

macro_rules! warn {
    ($kind:expr, $fmt:expr) => {
        warn!($kind, $fmt,);
    };
    ($kind:expr, $fmt:expr, $($arg:expr),*) => {
        log_!(crate::log::LogLevel::Warn, $kind, $fmt, $($arg),*);
    }
}

macro_rules! error {
    ($kind:expr, $fmt:expr) => {
        error!($kind, $fmt,);
    };
    ($kind:expr, $fmt:expr, $($arg:expr),*) => {{
        log_!(crate::log::LogLevel::Error, $kind, $fmt, $($arg),*);
        unreachable!();
    }}
}

static mut MINIMUM_LOG_LEVELS: [LogLevel; 14] = [LogLevel::Trace; 14];

pub fn get_log_level(kind: LogKind) -> LogLevel {
    unsafe {
        MINIMUM_LOG_LEVELS[kind as usize]
    }
}

pub fn set_log_level(kind: LogKind, level: LogLevel) {
    unsafe {
        MINIMUM_LOG_LEVELS[kind as usize] = level;
    }
}

pub fn set_all_log_levels(level: LogLevel) {
    unsafe {
        for l in MINIMUM_LOG_LEVELS.iter_mut() {
            *l = level;
        }
    }
}
