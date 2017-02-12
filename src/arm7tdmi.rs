use std::fmt;
use interconnect::Interconnect;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConditionCode {
    Eq, // Z=1:            equal
    Ne, // Z=0:            not equal
    Cs, // C=1:            unsigned higher or same
    Cc, // C=0:            unsigned lower
    Mi, // N=1:            negative
    Pl, // N=0:            positive or zero
    Vs, // V=1:            overflow
    Vc, // V=0:            no overflow
    Hi, // C=1 AND Z=0:    unsigned higher
    Ls, // C=0 OR Z=1:     unsigned lower or same
    Ge, // N=V:            greater or equal
    Lt, // N!=V:           less than
    Gt, // Z=0 AND (N=V):  greater than
    Le, // Z=1 OR (N!=V):  less than or equal
    Al, // 1:              always
}

impl From<u32> for ConditionCode {
    fn from(bits: u32) -> Self {
        use self::ConditionCode::*;
        match bits {
            0b0000 => Eq,
            0b0001 => Ne,
            0b0010 => Cs,
            0b0011 => Cc,
            0b0100 => Mi,
            0b0101 => Pl,
            0b0110 => Vs,
            0b0111 => Vc,
            0b1000 => Hi,
            0b1001 => Ls,
            0b1010 => Ge,
            0b1011 => Lt,
            0b1100 => Gt,
            0b1101 => Le,
            0b1110 => Al,
            _ => unreachable!(),
        }
    }
}

impl From<ConditionCode> for u32 {
    fn from(cc: ConditionCode) -> u32 {
        use self::ConditionCode::*;
        match cc {
            Eq => 0b0000,
            Ne => 0b0001,
            Cs => 0b0010,
            Cc => 0b0011,
            Mi => 0b0100,
            Pl => 0b0101,
            Vs => 0b0110,
            Vc => 0b0111,
            Hi => 0b1000,
            Ls => 0b1001,
            Ge => 0b1010,
            Lt => 0b1011,
            Gt => 0b1100,
            Le => 0b1101,
            Al => 0b1110,
        }
    }
}

impl fmt::Display for ConditionCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        static CC_NAMES: [&'static str; 15] = [
            "eq","ne","cs","cc","mi","pl","vs","vc","hi","ls","ge","lt","gt","le",""
        ];
        let i = *self as u32;
        if i > 0b1110 {
            Err(fmt::Error)
        } else {
            write!(f, "{}", CC_NAMES[i as usize])
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
 #[repr(u32)]
pub enum OperatingMode {
    // BIOS sets SPSR.mode to 0, so we allow invalid mode bitpatterns but
    // assert whenever in an invalid mode.
    Invalid(u8),
    User,
    Irq,
    Supervisor,
    Undefined, // Undefined instruction executed
    System,
}

impl OperatingMode {
    pub fn is_privileged(&self) -> bool {
        *self != OperatingMode::User
    }
}

impl From<u32> for OperatingMode {
    fn from(bits: u32) -> Self {
        use self::OperatingMode::*;
        match bits & 0b11111 {
            0b10000 => User,
            0b10001 => {
                panic!("ERROR: Tried to enter FIQ mode");
            }
            0b10010 => Irq,
            0b10011 => Supervisor,
            0b10111 => {
                panic!("ERROR: Tried to enter ABORT mode");
            }
            0b11011 => Undefined,
            0b11111 => System,
            other => {
                println!("WARNING: Entered invalid mode 0b{:05b}", other);
                Invalid(other as u8)
            }
        }
    }
}

impl From<OperatingMode> for u32 {
    fn from(mode: OperatingMode) -> Self {
        match mode {
            OperatingMode::User        => 0b10000,
            OperatingMode::Irq         => 0b10010,
            OperatingMode::Supervisor  => 0b10011,
            OperatingMode::Undefined   => 0b11011,
            OperatingMode::System      => 0b11111,
            OperatingMode::Invalid(other) => other as u32,
        }
    }
}

unpacked_bitfield_struct! {
    #[derive(Debug, Clone, Copy)]
    pub struct StatusRegister: u32 {
        (31,1) n: bool,
        (30,1) z: bool,
        (29,1) c: bool,
        (28,1) v: bool,

        (7,1) irq_disable: bool,
        (6,1) fiq_disable: bool,
        (5,1) thumb_mode: bool,
        (0,5) mode: OperatingMode,
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
            mode: OperatingMode::User,
        }
    }
}

impl fmt::Display for StatusRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
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

pub struct StepResult {
    pub op: u32,
    pub thumb_mode: bool,
}

pub const REG_SP: usize = 13;
pub const REG_LR: usize = 14;
pub const REG_PC: usize = 15;

pub struct Arm7TDMI {
    // System & User Mode:
    pub cpsr: StatusRegister,
    pub regs: [u32; 16],

    pub mem: Interconnect,

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
}

impl fmt::Debug for Arm7TDMI {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        static ARM_REGS: [&'static str; 16] = [
            "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7",
            "R8", "R9", "R10", "R11", "R12", "SP", "LR", "PC"
        ];

        for (i, r) in self.regs.iter().enumerate() {
            try!(write!(f, "{}:{:08X} ", ARM_REGS[i], r));
        }
        write!(f, "CPSR:{}\n\n", self.cpsr)
    }
}

static SWI_FN_NAMES: [&'static str; 43] = [
    "SoftReset",
    "RegisterRamReset",
    "Halt",
    "Stop",
    "IntrWait",
    "VBlankIntrWait",
    "Div",
    "DivArm",
    "Sqrt",
    "ArcTan",
    "ArcTan2",
    "CPUSet",
    "CPUFastSet",
    "BiosChecksum",
    "BgAffineSet",
    "ObjAffineSet",
    "BitUnPack",
    "LZ77UnCompWRAM",
    "LZ77UnCompVRAM",
    "HuffUnComp",
    "RLUnCompWRAM",
    "RLUnCompVRAM",
    "Diff8bitUnFilterWRAM",
    "Diff8bitUnFilterVRAM",
    "Diff16bitUnFilter",
    "SoundBiasChange",
    "SoundDriverInit",
    "SoundDriverMode",
    "SoundDriverMain",
    "SoundDriverVSync",
    "SoundChannelClear",
    "MIDIKey2Freq",
    "MusicPlayerOpen",
    "MusicPlayerStart",
    "MusicPlayerStop",
    "MusicPlayerContinue",
    "MusicPlayerFadeOut",
    "MultiBoot",
    "HardReset", // Undocumented
    "CustomHalt", // Undocumented
    "SoundDriverVSyncOff",
    "SoundDriverVSyncOn",
    "GetJumpList", // Undocumented
];

impl Arm7TDMI {
    pub fn new(bios: Box<[u8]>) -> Arm7TDMI {
        let mut result = Arm7TDMI {
            mem: Interconnect::new(bios),
            cpsr: Default::default(),
            regs: Default::default(),

            svc_sp: 0x03007FE0,
            svc_lr: 0,

            irq_sp: 0x03007FA0,
            irq_lr: 0,

            und_sp: 0,
            und_lr: 0,

            svc_spsr: Default::default(),
            irq_spsr: Default::default(),
            und_spsr: Default::default(),
        };

        result.regs[REG_SP] = 0x03007F00;

        result
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
            OperatingMode::Invalid(_) => unreachable!(),
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

    pub fn get_spsr(&self) -> StatusRegister {
        match self.cpsr.mode {
            OperatingMode::User => panic!("SPSR doesn't exist in User mode."),
            OperatingMode::System => panic!("SPSR doesn't exist in System mode."),
            OperatingMode::Invalid(_) => unreachable!(),
            OperatingMode::Supervisor => self.svc_spsr,
            OperatingMode::Irq        => self.irq_spsr,
            OperatingMode::Undefined  => self.und_spsr,
        }
    }

    pub fn get_spsr_mut(&mut self) -> &mut StatusRegister {
        match self.cpsr.mode {
            OperatingMode::User => panic!("SPSR doesn't exist in User mode."),
            OperatingMode::System => panic!("SPSR doesn't exist in System mode."),
            OperatingMode::Invalid(_) => unreachable!(),
            OperatingMode::Supervisor => &mut self.svc_spsr,
            OperatingMode::Irq        => &mut self.irq_spsr,
            OperatingMode::Undefined  => &mut self.und_spsr,
        }
    }

    pub fn set_spsr(&mut self, psr: StatusRegister) {
        match self.cpsr.mode {
            OperatingMode::User | OperatingMode::System => {},
            OperatingMode::Invalid(_) => unreachable!(),
            OperatingMode::Supervisor => self.svc_spsr = psr,
            OperatingMode::Irq        => self.irq_spsr = psr,
            OperatingMode::Undefined  => self.und_spsr = psr,
        }
    }

    pub fn has_spsr(&self) -> bool {
        match self.cpsr.mode {
            OperatingMode::User |
            OperatingMode::System |
            OperatingMode::Invalid(_) => false,
            _ => true,
        }
    }

    pub fn branch_to(&mut self, addr: u32) {
        let step = self.get_op_size();
        if self.cpsr.thumb_mode {
            assert!(addr & 1 == 0);
            self.mem.prefetch[0] = self.mem.exec16(addr) as u32;
            self.mem.prefetch[1] = self.mem.exec16(addr + step) as u32;
            self.regs[REG_PC] = addr + step;
        } else {
            assert!(addr & 3 == 0);
            self.mem.prefetch[0] = self.mem.exec32(addr);
            self.mem.prefetch[1] = self.mem.exec32(addr + step);
            self.regs[REG_PC] = addr + step;
        }
    }

    pub fn branch_exchange(&mut self, addr: u32) {
        self.cpsr.thumb_mode = (addr & 1) != 0;
        self.branch_to(addr & !1u32);
    }

    pub fn get_op_size(&self) -> u32 {
        if self.cpsr.thumb_mode {
            2
        } else {
            4
        }
    }

    pub fn signal_reset(&mut self) {
        const INTVEC_RESET: u32 = 0x0;

        self.svc_lr = 0xDEADBEEF; // State is unpredictable on reset
        self.switch_mode(OperatingMode::Supervisor);
        self.cpsr.fiq_disable = true;
        self.cpsr.irq_disable = true;
        self.cpsr.thumb_mode = false;
        self.branch_to(INTVEC_RESET);
    }

    pub fn signal_undef(&mut self) {
        const INTVEC_UNDEFINED: u32 = 0x4;

        self.und_spsr = self.cpsr;
        self.und_lr = self.regs[REG_PC] + self.get_op_size();
        self.switch_mode(OperatingMode::Undefined);
        self.cpsr.thumb_mode = false;
        self.branch_to(INTVEC_UNDEFINED);
    }

    pub fn signal_swi(&mut self) {
        const INTVEC_SWI: u32 = 0x8;

        self.svc_spsr = self.cpsr;
        self.svc_lr = self.regs[REG_PC] + self.get_op_size();
        self.switch_mode(OperatingMode::Supervisor);
        self.cpsr.thumb_mode = false;
        self.branch_to(INTVEC_SWI);
    }

    pub fn signal_irq(&mut self) {
        const INTVEC_IRQ: u32 = 0x18;

        if self.cpsr.irq_disable {
            return;
        }
        self.irq_spsr = self.cpsr;
        self.irq_lr = self.regs[REG_PC] + 4;
        self.switch_mode(OperatingMode::Irq);
        self.cpsr.thumb_mode = false;
        self.branch_to(INTVEC_IRQ);
    }

    pub fn step(&mut self) -> StepResult {
        use thumb_core::step_thumb;
        use arm_core::step_arm;

        let step = self.get_op_size();
        self.regs[REG_PC] += step;

        let result = StepResult {
            op: self.mem.prefetch[0],
            thumb_mode: self.cpsr.thumb_mode,
        };

        self.mem.prefetch[0] = self.mem.prefetch[1];
        self.mem.prefetch[1] = if self.cpsr.thumb_mode {
            self.mem.exec16(self.regs[REG_PC]) as u32
        } else {
            self.mem.exec32(self.regs[REG_PC])
        };

        if self.cpsr.thumb_mode {
            step_thumb(self, result.op as u16);
        } else {
            step_arm(self, result.op);
        }

        result
    }
}
