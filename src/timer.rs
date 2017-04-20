use std::fmt;
use num::FromPrimitive;
use utils::Cycle;
use interconnect::{ IrqFlags, TIMER0_OVERFLOW, TIMER1_OVERFLOW, TIMER2_OVERFLOW, TIMER3_OVERFLOW };
use log::*;

enum_from_primitive! {
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TimerScale {
    Div1 = 0,
    Div64 = 1,
    Div256 = 2,
    Div1024 = 3,
}
}

impl fmt::Display for TimerScale {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match *self {
            TimerScale::Div1 => "16.8Mhz",
            TimerScale::Div64 => "262.5Khz",
            TimerScale::Div256 => "65.6Khz",
            TimerScale::Div1024 => "16.4Khz",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TimerUnit {
    Tm0,
    Tm1,
    Tm2,
    Tm3,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TimerState {
    Disabled {
        current_value: u16,
    },
    Enabled {
        initial_cycle: Cycle,
        initial_value: u16,
        end_cycle: Cycle,
    },
    Cascade {
        current_value: u16,
    }
}

impl TimerState {
    fn is_disabled(&self) -> bool {
        match *self {
            TimerState::Disabled { .. } => true,
            _ => false,
        }
    }

    fn is_enabled(&self) -> bool {
        !self.is_disabled()
    }
}

#[derive(Clone, Copy)]
pub struct Timer {
    unit: TimerUnit,

    scale: TimerScale,
    pub reload_value: u16,
    pub cascade_timing: bool,
    pub irq_on_overflow: bool,
    pub state: TimerState,
}

impl Timer {
    pub fn new(unit: TimerUnit) -> Timer {
        Timer {
            unit: unit,

            reload_value: 0,

            scale: TimerScale::Div1,
            cascade_timing: false,
            irq_on_overflow: false,
            state: TimerState::Disabled { current_value: 0 },
        }
    }

    pub fn cycles_per_tick(&self) -> Cycle {
        match self.scale {
            TimerScale::Div1 => Cycle(1),
            TimerScale::Div64 => Cycle(64),
            TimerScale::Div256 => Cycle(256),
            TimerScale::Div1024 => Cycle(1024),
        }
    }

    pub fn irq_flag(&self) -> IrqFlags {
        match self.unit {
            TimerUnit::Tm0 => TIMER0_OVERFLOW,
            TimerUnit::Tm1 => TIMER1_OVERFLOW,
            TimerUnit::Tm2 => TIMER2_OVERFLOW,
            TimerUnit::Tm3 => TIMER3_OVERFLOW,
        }
    }

    pub fn log_kind(&self) -> LogKind {
        match self.unit {
            TimerUnit::Tm0 => TM0,
            TimerUnit::Tm1 => TM1,
            TimerUnit::Tm2 => TM2,
            TimerUnit::Tm3 => TM3,
        }
    }

    pub fn restart(&mut self, cycles: Cycle) {
        self.state = if self.cascade_timing {
            TimerState::Cascade { current_value: self.reload_value }
        } else {
            TimerState::Enabled {
                initial_value: self.reload_value,
                initial_cycle: cycles,
                end_cycle: cycles + Cycle((self.reload_value + 1) as i64 * self.cycles_per_tick().0),
            }
        };
    }

    pub fn pause(&mut self, cycles: Cycle) {
        self.state = TimerState::Disabled {
            current_value: self.get_current_value(cycles )
        };
    }

    pub fn write_cnt(&mut self, cycles: Cycle, value: u16) {
        let reg = TimerControlReg::from(value);

        if reg.count_up_timing {
            assert!(self.unit != TimerUnit::Tm0);
            unimplemented!();
        }

        self.scale = TimerScale::from_u8(reg.scale).unwrap();
        self.cascade_timing = reg.count_up_timing;
        self.irq_on_overflow = reg.irq_enable;

        if reg.start && self.state.is_disabled() {
            self.restart(cycles);
        }
        if !reg.start {
            self.pause(cycles);
        }
    }

    pub fn read_cnt(&self) -> u16 {
        TimerControlReg {
            scale: self.scale as u8,
            count_up_timing: self.cascade_timing,
            irq_enable: self.irq_on_overflow,
            start: self.state.is_enabled(),
        }.into()
    }

    pub fn set_reload_value(&mut self, value: u16) {
        self.reload_value = value;
    }

    pub fn get_current_value(&self, cycle: Cycle) -> u16 {
        match self.state {
            TimerState::Enabled { initial_cycle, initial_value, .. } => {
                assert!(initial_cycle <= cycle);
                let cycle_delta = cycle - initial_cycle;
                assert!(cycle_delta.0 / self.cycles_per_tick().0 < u16::max_value() as i64);
                let count_delta = (cycle_delta.0 / self.cycles_per_tick().0) as u16;
                initial_value.saturating_sub(count_delta)
            }
            TimerState::Cascade { current_value } => current_value,
            TimerState::Disabled { current_value } => current_value,
        }
    }
}

unpacked_bitfield_struct! {
struct TimerControlReg: u16 {
    (0,2) scale: u8,
    (2,1) count_up_timing: bool,
    (6,1) irq_enable: bool,
    (7,1) start: bool,
}
}