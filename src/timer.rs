use std::fmt;
use num::FromPrimitive;
use utils::Cycle;
use interconnect::{ IrqFlags, TIMER0_OVERFLOW, TIMER1_OVERFLOW, TIMER2_OVERFLOW, TIMER3_OVERFLOW };

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
    Disabled,
    Enabled {
        value: u16,
        remaining: Cycle,
    }
}

#[derive(Clone, Copy)]
pub struct Timer {
    unit: TimerUnit,

    scale: TimerScale,
    reload_value: u16,
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
            state: TimerState::Disabled,
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

    pub fn write_cnt(&mut self, value: u16) {
        let reg = TimerControlReg::from(value);

        if reg.count_up_timing {
            assert!(self.unit != TimerUnit::Tm0);
        }

        self.scale = TimerScale::from_u8(reg.scale).unwrap();
        self.cascade_timing = reg.count_up_timing;
        self.irq_on_overflow = reg.irq_enable;

        if reg.start && self.state == TimerState::Disabled {
            self.state = TimerState::Enabled {
                value: self.reload_value,
                remaining: self.cycles_per_tick(),
            };
        }
    }

    pub fn read_cnt(&self) -> u16 {
        TimerControlReg {
            scale: self.scale as u8,
            count_up_timing: self.cascade_timing,
            irq_enable: self.irq_on_overflow,
            start: self.state != TimerState::Disabled,
        }.into()
    }

    pub fn set_reload_value(&mut self, value: u16) {
        self.reload_value = value;
    }

    pub fn get_current_value(&self) -> u16 {
        if let TimerState::Enabled { value, .. } = self.state {
            value
        } else {
            self.reload_value
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