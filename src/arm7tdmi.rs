use std::fmt;

use num::FromPrimitive;
use interconnect::{ PrefetchValue, Interconnect };
use utils::{ OrderedSet, Cycle };

enum_from_primitive! {
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConditionCode {
    Eq = 0b0000, // Z=1:            equal
    Ne = 0b0001, // Z=0:            not equal
    Cs = 0b0010, // C=1:            unsigned higher or same
    Cc = 0b0011, // C=0:            unsigned lower
    Mi = 0b0100, // N=1:            negative
    Pl = 0b0101, // N=0:            positive or zero
    Vs = 0b0110, // V=1:            overflow
    Vc = 0b0111, // V=0:            no overflow
    Hi = 0b1000, // C=1 AND Z=0:    unsigned higher
    Ls = 0b1001, // C=0 OR Z=1:     unsigned lower or same
    Ge = 0b1010, // N=V:            greater or equal
    Lt = 0b1011, // N!=V:           less than
    Gt = 0b1100, // Z=0 AND (N=V):  greater than
    Le = 0b1101, // Z=1 OR (N!=V):  less than or equal
    Al = 0b1110, // 1:              always
}
}

enum_from_primitive! {
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OperatingMode {
    None       = 0b00000, // spsr.mode is set to 0 in bios
    User       = 0b10000,
    Irq        = 0b10010,
    Supervisor = 0b10011,
    Undefined  = 0b11011,
    System     = 0b11111,
}
}

impl fmt::Display for ConditionCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ConditionCode::*;
        let s = match *self {
            Eq => "eq",
            Ne => "ne",
            Cs => "cs",
            Cc => "cc",
            Mi => "mi",
            Pl => "pl",
            Vs => "vs",
            Vc => "vc",
            Hi => "hi",
            Ls => "ls",
            Ge => "ge",
            Lt => "lt",
            Gt => "gt",
            Le => "le",
            Al => "al",
        };

        write!(f, "{}", s)
    }
}

impl OperatingMode {
    pub fn is_privileged(self) -> bool {
        self != OperatingMode::User
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StatusRegister {
    pub n: bool,
    pub z: bool,
    pub c: bool,
    pub v: bool,

    pub irq_disable: bool,
    pub fiq_disable: bool,
    pub thumb_mode: bool,
    pub mode: OperatingMode,
}

impl From<u32> for StatusRegister {
    fn from(bits: u32) -> StatusRegister {
        StatusRegister {
            n: (1 << 31) & bits != 0,
            z: (1 << 30) & bits != 0,
            c: (1 << 29) & bits != 0,
            v: (1 << 28) & bits != 0,
            irq_disable: (1 << 7) & bits != 0,
            fiq_disable: (1 << 6) & bits != 0,
            thumb_mode: (1 << 5) & bits != 0,
            mode: OperatingMode::from_u32(bits & 0x1F).unwrap()
        }
    }
}

impl From<StatusRegister> for u32 {
    fn from(sr: StatusRegister) -> u32 {
        let mut bits = 0;
        bits |= (sr.n as u32) << 31;
        bits |= (sr.z as u32) << 30;
        bits |= (sr.c as u32) << 29;
        bits |= (sr.v as u32) << 28;
        bits |= (sr.irq_disable as u32) << 7;
        bits |= (sr.fiq_disable as u32) << 6;
        bits |= (sr.thumb_mode as u32) << 5;
        bits |= sr.mode as u32;
        bits
    }
}

impl Default for StatusRegister {
    fn default() -> StatusRegister {
        StatusRegister {
            n: false,
            z: false,
            c: false,
            v: false,

            irq_disable: false,
            fiq_disable: false,
            thumb_mode: false,
            mode: OperatingMode::None,
        }
    }
}

impl fmt::Display for StatusRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({n}{z}{c}{v}{t}{irq}{fiq}, {mode:?})",
            n = if self.n {"n"} else {"-"},
            z = if self.z {"z"} else {"-"},
            c = if self.c {"c"} else {"-"},
            v = if self.v {"v"} else {"-"},
            t = if self.thumb_mode {"t"} else {"-"},
            irq = if self.irq_disable {""} else {", irq"},
            fiq = if self.fiq_disable {""} else {", fiq"},
            mode = self.mode,
        )
    }
}

impl StatusRegister {
    pub fn set_flags(&mut self, bits: u32) {
        self.n = bits & (1 << 31) != 0;
        self.z = bits & (1 << 30) != 0;
        self.c = bits & (1 << 29) != 0;
        self.v = bits & (1 << 28) != 0;
    }
}

pub enum StepEvent {
    None,
    TriggerBreakpoint(u32),
    TriggerWatchpoint(u32),
}

pub struct StepInfo {
    pub op: u32,
    pub op_addr: u32,
    pub thumb_mode: bool,
    pub event: StepEvent,
}

pub const REG_SP: usize = 13;
pub const REG_LR: usize = 14;
pub const REG_PC: usize = 15;

#[derive(Clone)]
pub struct Arm7TDMI {
    pub regs: [u32; 16],
    pub cpsr: StatusRegister,
    pub cycles: Cycle,

    // Supervisor Mode:
    svc_sp: u32,
    svc_lr: u32,
    svc_spsr: StatusRegister,

    // IRQ Mode:
    irq_sp: u32,
    irq_lr: u32,
    irq_spsr: StatusRegister,

    // Undefined Mode:
    und_sp: u32,
    und_lr: u32,
    und_spsr: StatusRegister,

    // For use by the debugger
    pub breakpoints: OrderedSet<u32>,
    pub watchpoints: OrderedSet<u32>,
}

impl fmt::Debug for Arm7TDMI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        static ARM_REGS: [&'static str; 16] = [
            "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7",
            "R8", "R9", "R10", "R11", "R12", "SP", "LR", "PC"
        ];

        for (i, r) in self.regs.iter().enumerate() {
            write!(f, "{}:{:08X} ", ARM_REGS[i], r)?;
        }
        write!(f, "CPSR:{}\n\n", self.cpsr)
    }
}

impl Arm7TDMI {
    pub fn new() -> Arm7TDMI {
        Arm7TDMI {
            regs: Default::default(),
            cpsr: Default::default(),
            cycles: Cycle(0),

            svc_sp: 0,
            svc_lr: 0,
            svc_spsr: Default::default(),

            irq_sp: 0,
            irq_lr: 0,
            irq_spsr: Default::default(),

            und_sp: 0,
            und_lr: 0,
            und_spsr: Default::default(),

            breakpoints: OrderedSet::new(),
            watchpoints: OrderedSet::new(),
        }
    }

    pub fn bank_swap(&mut self, mode: OperatingMode) {
        use std::mem;
        fn helper(regs: &mut [u32], lr: &mut u32, sp: &mut u32) {
            mem::swap(&mut regs[REG_LR], lr);
            mem::swap(&mut regs[REG_SP], sp);
        }

        match mode {
            OperatingMode::Supervisor => helper(&mut self.regs, &mut self.svc_lr, &mut self.svc_sp),
            OperatingMode::Irq        => helper(&mut self.regs, &mut self.irq_lr, &mut self.irq_sp),
            OperatingMode::Undefined  => helper(&mut self.regs, &mut self.und_lr, &mut self.und_sp),
            OperatingMode::None => {}
            OperatingMode::User | OperatingMode::System => {}
        }
    }

    pub fn switch_mode(&mut self, to_mode: OperatingMode) {
        let from_mode = self.cpsr.mode;
        self.bank_swap(from_mode);
        self.bank_swap(to_mode);
        self.cpsr.mode = to_mode;
    }

    pub fn eval_condition_code(&self, code: ConditionCode) -> bool {
        match code {
            ConditionCode::Eq => self.cpsr.z,
            ConditionCode::Ne => !self.cpsr.z,
            ConditionCode::Cs => self.cpsr.c,
            ConditionCode::Cc => !self.cpsr.c,
            ConditionCode::Mi => self.cpsr.n,
            ConditionCode::Pl => !self.cpsr.n,
            ConditionCode::Vs => self.cpsr.v,
            ConditionCode::Vc => !self.cpsr.v,
            ConditionCode::Hi => self.cpsr.c && !self.cpsr.z,
            ConditionCode::Ls => !self.cpsr.c || self.cpsr.z,
            ConditionCode::Ge => self.cpsr.n == self.cpsr.v,
            ConditionCode::Lt => self.cpsr.n != self.cpsr.v,
            ConditionCode::Gt => !self.cpsr.z && (self.cpsr.n == self.cpsr.v),
            ConditionCode::Le => self.cpsr.z || (self.cpsr.n != self.cpsr.v),
            ConditionCode::Al => true,
        }
    }

    pub fn set_cpsr(&mut self, new: StatusRegister) {
        assert!(new.thumb_mode == self.cpsr.thumb_mode);
        if new.mode != self.cpsr.mode {
            let cur_mode = self.cpsr.mode;
            self.bank_swap(cur_mode);
            self.bank_swap(new.mode);
        }
        self.cpsr = new;
    }

    pub fn get_spsr(&self) -> StatusRegister {
        match self.cpsr.mode {
            OperatingMode::User => panic!("SPSR doesn't exist in User mode."),
            OperatingMode::System => panic!("SPSR doesn't exist in System mode."),
            OperatingMode::None => unreachable!(),
            OperatingMode::Supervisor => self.svc_spsr,
            OperatingMode::Irq        => self.irq_spsr,
            OperatingMode::Undefined  => self.und_spsr,
        }
    }

    pub fn get_spsr_mut(&mut self) -> &mut StatusRegister {
        match self.cpsr.mode {
            OperatingMode::User => panic!("SPSR doesn't exist in User mode."),
            OperatingMode::System => panic!("SPSR doesn't exist in System mode."),
            OperatingMode::None => unreachable!(),
            OperatingMode::Supervisor => &mut self.svc_spsr,
            OperatingMode::Irq        => &mut self.irq_spsr,
            OperatingMode::Undefined  => &mut self.und_spsr,
        }
    }

    pub fn set_spsr(&mut self, psr: StatusRegister) {
        match self.cpsr.mode {
            OperatingMode::User | OperatingMode::System | OperatingMode::None => {},
            OperatingMode::Supervisor => self.svc_spsr = psr,
            OperatingMode::Irq        => self.irq_spsr = psr,
            OperatingMode::Undefined  => self.und_spsr = psr,
        }
    }

    pub fn has_spsr(&self) -> bool {
        match self.cpsr.mode {
            OperatingMode::User |
            OperatingMode::System |
            OperatingMode::None => false,
            _ => true,
        }
    }

    pub fn branch_to(&mut self, interconnect: &mut Interconnect, addr: u32) {
        let step = self.get_op_size();
        let mut cycles = Cycle(0);
        if self.cpsr.thumb_mode {
            debug_assert!(addr & 1 == 0);
            interconnect.prefetch[0] = PrefetchValue {
                op: add_cycles!(cycles, interconnect.exec_thumb_slow(addr)) as u32,
                addr: addr,
            };
            interconnect.prefetch[1] = PrefetchValue {
                op: add_cycles!(cycles, interconnect.exec_thumb_slow(addr + step)) as u32,
                addr: addr + step,
            };
            self.regs[REG_PC] = addr + step;
        } else {
            debug_assert!(addr & 3 == 0);
            interconnect.prefetch[0] = PrefetchValue {
                op: add_cycles!(cycles, interconnect.exec_arm_slow(addr)),
                addr: addr,
            };
            interconnect.prefetch[1] = PrefetchValue {
                op: add_cycles!(cycles, interconnect.exec_arm_slow(addr + step)),
                addr: addr + step,
            };
            self.regs[REG_PC] = addr + step;
        }
        self.cycles += cycles;
    }

    pub fn branch_exchange(&mut self, interconnect: &mut Interconnect, addr: u32) {
        self.cpsr.thumb_mode = (addr & 1) != 0;
        self.branch_to(interconnect, addr & !1u32);
    }

    pub fn get_op_size(&self) -> u32 {
        if self.cpsr.thumb_mode {
            2
        } else {
            4
        }
    }

    pub fn signal_reset(&mut self, interconnect: &mut Interconnect) {
        const INTVEC_RESET: u32 = 0x0;

        self.switch_mode(OperatingMode::Supervisor);
        self.cpsr.fiq_disable = true;
        self.cpsr.irq_disable = true;
        self.cpsr.thumb_mode = false;
        self.branch_to(interconnect, INTVEC_RESET);
    }

    pub fn signal_undef(&mut self, interconnect: &mut Interconnect) {
        const INTVEC_UNDEFINED: u32 = 0x4;

        self.und_spsr = self.cpsr;
        self.und_lr = self.regs[REG_PC] + self.get_op_size();
        self.switch_mode(OperatingMode::Undefined);
        self.cpsr.thumb_mode = false;
        self.branch_to(interconnect, INTVEC_UNDEFINED);
    }

    pub fn signal_swi(&mut self, interconnect: &mut Interconnect) {
        const INTVEC_SWI: u32 = 0x8;

        self.svc_spsr = self.cpsr;
        self.svc_lr = self.regs[REG_PC] + self.get_op_size();
        self.switch_mode(OperatingMode::Supervisor);
        self.cpsr.thumb_mode = false;
        self.branch_to(interconnect, INTVEC_SWI);
    }

    pub fn signal_irq(&mut self, interconnect: &mut Interconnect) {
        const INTVEC_IRQ: u32 = 0x18;

        if self.cpsr.irq_disable {
            return;
        }
        self.irq_spsr = self.cpsr;
        self.irq_lr = self.regs[REG_PC] + 4;
        self.switch_mode(OperatingMode::Irq);
        self.cpsr.thumb_mode = false;
        self.branch_to(interconnect, INTVEC_IRQ);
    }

    pub fn step(&mut self, interconnect: &mut Interconnect) -> StepInfo {
        use thumb_core::step_thumb;
        use arm_core::step_arm;

        let PrefetchValue { op, addr } = interconnect.prefetch[0];

        let last_thumb_mode = self.cpsr.thumb_mode;

        if self.breakpoints.contains(addr) {
            return StepInfo {
                op: op,
                op_addr: addr,
                thumb_mode: last_thumb_mode,
                event: StepEvent::TriggerBreakpoint(addr)
            };
        }

        let step = self.get_op_size();
        self.regs[REG_PC] += step;

        interconnect.prefetch[0] = interconnect.prefetch[1];
        let next_op = if self.cpsr.thumb_mode {
            interconnect.exec_thumb_slow(self.regs[REG_PC]).1 as u32
        } else {
            interconnect.exec_arm_slow(self.regs[REG_PC]).1
        };

        interconnect.prefetch[1] = PrefetchValue {
            op: next_op,
            addr: self.regs[REG_PC],
        };

        let event = if self.cpsr.thumb_mode {
            step_thumb(self, interconnect, op as u16)
        } else {
            step_arm(self, interconnect, op)
        };

        StepInfo {
            op: op,
            op_addr: addr,
            thumb_mode: last_thumb_mode,
            event: event
        }
    }
}
