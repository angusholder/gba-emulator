use std::cmp::Ord;
use std::ops::{ Add, AddAssign };

macro_rules! unpack {
    (u8,    $n:expr) => { $n as u8 };
    (u16,   $n:expr) => { $n as u16 };
    (u32,   $n:expr) => { $n as u32 };
    (u64,   $n:expr) => { $n as u64 };
    (usize, $n:expr) => { $n as usize };

    (bool, $n:expr) => { $n != 0 };

    ($T:ident, $n:expr) => { Into::<$T>::into($n) };
}

macro_rules! to_primitive {
    ($packed_type:ty, u8,    $n:expr) => { $n as $packed_type };
    ($packed_type:ty, u16,   $n:expr) => { $n as $packed_type };
    ($packed_type:ty, u32,   $n:expr) => { $n as $packed_type };
    ($packed_type:ty, u64,   $n:expr) => { $n as $packed_type };
    ($packed_type:ty, usize, $n:expr) => { $n as $packed_type };

    ($packed_type:ty, bool, $n:expr) => { $n as $packed_type };

    ($packed_type:ty, $T:ident, $n:expr) => { Into::<$packed_type>::into($n) };
}

macro_rules! unpacked_bitfield_struct {
    { $(
        $(#[$attr:meta])*
        pub struct $unpacked_type:ident: $packed_type:ty {
            $( ($start:expr, $width:expr) $field_name:ident: $field_type:ident,)+
        }
    )+ } => { $(
        $(#[$attr])*
        pub struct $unpacked_type {
            $(pub $field_name: $field_type),+
        }

        impl From<$packed_type> for $unpacked_type {
            fn from(n: $packed_type) -> Self {
                $unpacked_type { $(
                    $field_name: unpack!(
                        $field_type,
                        ((1 << $width) - 1) & (n >> $start)
                    )
                ),+ }
            }
        }

        impl From<$unpacked_type> for $packed_type {
            fn from(s: $unpacked_type) -> Self {
                $(to_primitive!($packed_type, $field_type, s.$field_name) << $start)|+
            }
        }
    )+ };

    { $(
        $(#[$attr:meta])*
        struct $unpacked_type:ident: $packed_type:ty {
            $( ($start:expr, $width:expr) $field_name:ident: $field_type:ident,)+
        }
    )+ } => { $(
        $(#[$attr])*
        struct $unpacked_type {
            $(pub $field_name: $field_type),+
        }

        impl From<$packed_type> for $unpacked_type {
            fn from(n: $packed_type) -> Self {
                $unpacked_type { $(
                    $field_name: convert!(
                        $field_type,
                        ((1 << $width) - 1) & (n >> $start)
                    )
                ),+ }
            }
        }

        impl From<$unpacked_type> for $packed_type {
            fn from(s: $unpacked_type) -> Self {
                $((s.$field_name as $packed_type) << $start)|+
            }
        }
    )+ };
}

macro_rules! size {
    (u8) => { 1u32 };
    (u16) => { 2u32 };
    (u32) => { 4u32 };
}

macro_rules! foo {
    (from: u8,  to: u8,  $val:expr, $offset:expr) => ($val);
    (from: u16, to: u16, $val:expr, $offset:expr) => ($val);
    (from: u32, to: u32, $val:expr, $offset:expr) => ($val);

    (from: u8,  to: u16, $val:expr, $offset:expr) => (($val as u16) << ($offset*8));
    (from: u8,  to: u32, $val:expr, $offset:expr) => (($val as u32) << ($offset*8));

    (from: u16, to: u8,  $val:expr, $offset:expr) => (($val >> ($offset*8)) as u8);
    (from: u16, to: u32, $val:expr, $offset:expr) => (($val as u32) << ($offset*8));

    (from: u32, to: u8,  $val:expr, $offset:expr) => (($val >> ($offset*8)) as u8);
    (from: u32, to: u16, $val:expr, $offset:expr) => (($val >> ($offset*8)) as u16);
}

macro_rules! read_cache {
    ($cache:expr, u8, $addr:expr) => { $cache.read8($addr) };
    ($cache:expr, u16, $addr:expr) => { $cache.read16($addr) };
    ($cache:expr, u32, $addr:expr) => { $cache.read32($addr) };
}

macro_rules! write_cache {
    ($cache:expr, u8, $addr:expr, $value:expr) => { $cache.write8($addr, $value); };
    ($cache:expr, u16, $addr:expr, $value:expr) => { $cache.write16($addr, $value); };
    ($cache:expr, u32, $addr:expr, $value:expr) => { $cache.write32($addr, $value); };
}

macro_rules! read_io_method {
    ($fn_name:ident, $io_type:ident, $($T:ident, $case_addr:expr, $getter:expr,)+) => {
        pub fn $fn_name(&self, addr: u32) -> $io_type {
            use std::cmp;
            let mut result: $io_type = 0;
            let mut bytes_handled = 0;
            let addr = addr & 0xFFF;

            $({
                let type_size = cmp::max(size!($T), size!($io_type));
                let mask: u32 = !(type_size - 1);
                if (addr & mask) == ($case_addr & mask) {
                    bytes_handled += size!($T);
                    let _offset = $case_addr&!mask | addr&!mask;
                    result |= foo!(from: $T, to: $io_type, $getter(self), _offset);
                }
            })+

            if bytes_handled < size!($io_type) {
                println!("WARNING: Unhandled ioread{} at 0x0400_0{:03X}", 8*size!($io_type), addr);
            }

            result
        }
    }
}

macro_rules! write_io_method {
    ($fn_name:ident, $io_type:ident, $($T:ident, $case_addr:expr, $setter:expr,)+) => {
        fn $fn_name(&mut self, addr: u32, value: $io_type) {
            use std::cmp;
            let mut bytes_handled = 0;
            let addr = addr & 0xFFF;

            $({
                let type_size = cmp::max(size!($T), size!($io_type));
                let mask: u32 = !(type_size - 1);
                if (addr & mask) == ($case_addr & mask) {
                    bytes_handled += size!($T);
                    let _offset = ($case_addr&!mask | addr&!mask) as $T;
                    let addr = if size!($io_type) > size!($T) { addr } else { addr & mask };
                    let whole = read_cache!(self.io_cache, $T, addr);
                    let new: $T = if size!($io_type) > size!($T) {
                        (value >> (_offset*8)) as $T
                    } else {
                        #[allow(unused_imports)]
                        use std::{ u8, u16, u32 };
                        whole & !(($io_type::max_value() as $T) << (_offset*8)) | ((value as $T) << (_offset*8))
                    };
                    write_cache!(self.io_cache, $T, addr, new);
                    $setter(self, new);
                }
            })+

            if bytes_handled < size!($io_type) {
                println!("WARNING: Unhandled iowrite{} at 0x0400_0{:03X} of {:X}", 8*size!($io_type), addr, value);
            }
        }
    }
}

macro_rules! impl_io_map {
    (
        [$struct_type:ty]
        $(($T:ident, $case_addr:expr, $debug_name:ident) {
            read => $getter:expr,
            write => $setter:expr
        })+
    ) => {
        impl $struct_type {
            read_io_method!(ioread8, u8, $($T, $case_addr, $getter,)+);
            read_io_method!(ioread16, u16, $($T, $case_addr, $getter,)+);
            read_io_method!(ioread32, u32, $($T, $case_addr, $getter,)+);

            write_io_method!(iowrite8, u8, $($T, $case_addr, $setter,)+);
            write_io_method!(iowrite16, u16, $($T, $case_addr, $setter,)+);
            write_io_method!(iowrite32, u32, $($T, $case_addr, $setter,)+);
        }
    }
}

#[test]
fn memory_map() {
    struct Foo {
        write_cache: Buffer,
        a: u32,
        b: u16,
        c: u8,
        d: u8,
    }

    impl_io_map! {
        [Foo]
        (u32, 0u32, a) {
            read => |this: &Foo| {
                this.a
            },
            write => |this: &mut Foo, value| {
                this.a = value;
            }
        }
        (u16, 6u32, b) {
            read => |this: &Foo| {
                this.b
            },
            write => |this: &mut Foo, value| {
                this.b = value;
            }
        }
        (u8, 4u32, c) {
            read => |this: &Foo| {
                this.c
            },
            write => |this: &mut Foo, value| {
                this.c = value;
            }
        }
        (u8, 5u32, d) {
            read => |this: &Foo| {
                this.d
            },
            write => |this: &mut Foo, value| {
                this.d = value;
            }
        }
    }

    let mut foo = Foo {
        write_cache: Buffer::new(vec![0xEF, 0xBE, 0xAD, 0xDE, 0xFE, 0xCA, 0xAF, 0xDE].into_boxed_slice()),
        a: 0xDEADBEEF,
        b: 0xCAFE,
        c: 0xAF,
        d: 0xDE,
    };

    assert_eq!(foo.ioread8(0), 0xEF);
    assert_eq!(foo.ioread8(1), 0xBE);
    assert_eq!(foo.ioread8(2), 0xAD);
    assert_eq!(foo.ioread8(3), 0xDE);

    assert_eq!(foo.ioread32(4), 0xCAFEDEAF);

    assert_eq!(foo.ioread32(0), 0xDEADBEEF);

    foo.iowrite8(0, 0x78);
    foo.iowrite8(1, 0x56);
    foo.iowrite8(2, 0x34);
    foo.iowrite8(3, 0x12);

    assert_eq!(foo.ioread32(0), 0x12345678);

    foo.iowrite32(4, 0x12345678);

    assert_eq!(foo.ioread8(4), 0x78);
    assert_eq!(foo.ioread8(5), 0x56);
    assert_eq!(foo.ioread8(6), 0x34);
    assert_eq!(foo.ioread8(7), 0x12);
}

#[derive(Clone)]
pub struct Buffer {
    buffer: Box<[u8]>,
}

macro_rules! check_bounds {
    ($buffer:expr, $index:expr) => {
        if ($index as usize) >= $buffer.len() {
            panic!("index out of bounds: the len is {} but the index is {}",
                   $buffer.len(), $index);
        }
    }
}

impl Buffer {
    pub fn new(buffer: &[u8]) -> Buffer {
        Buffer {
            buffer: buffer.to_vec().into_boxed_slice()
        }
    }

    pub fn with_capacity(capacity: u32) -> Buffer {
        Buffer {
            buffer: vec![0u8; capacity as usize].into_boxed_slice()
        }
    }

    pub fn read8(&self, index: u32) -> u8 {
        self.buffer[index as usize]
    }

    pub fn read16(&self, index: u32) -> u16 {
        assert!(index & 1 == 0);
        check_bounds!(self.buffer, index + 1);
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * const u16) }
    }

    pub fn read32(&self, index: u32) -> u32 {
        assert!(index & 3 == 0);
        check_bounds!(self.buffer, index + 3);
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * const u32) }
    }

    pub fn write8(&mut self, index: u32, value: u8) {
        self.buffer[index as usize] = value;
    }

    pub fn write16(&mut self, index: u32, value: u16) {
        assert!(index & 1 == 0);
        check_bounds!(self.buffer, index + 1);
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * mut u16) = value; }
    }

    pub fn write32(&mut self, index: u32, value: u32) {
        assert!(index & 3 == 0);
        check_bounds!(self.buffer, index + 3);
        unsafe { *(self.buffer.as_ptr().offset(index as isize) as * mut u32) = value; }
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.buffer.as_ptr()
    }

    pub fn len(&self) -> usize {
        self.buffer.len()
    }
}

use std::slice::Iter;
#[derive(Clone)]
pub struct OrderedSet<T> {
    vec: Vec<T>,
}

impl<T: Ord + Copy> OrderedSet<T> {
    pub fn new() -> OrderedSet<T> {
        OrderedSet {
            vec: Vec::new()
        }
    }

    pub fn insert(&mut self, item: T) -> bool {
        if let Err(idx) = self.vec.binary_search(&item) {
            self.vec.insert(idx, item);
            true
        } else {
            false
        }
    }

    pub fn contains(&self, item: T) -> bool {
        self.vec.binary_search(&item).is_ok()
    }

    pub fn remove(&mut self, item: T) -> bool {
        if let Ok(idx) = self.vec.binary_search(&item) {
            self.vec.remove(idx);
            true
        } else {
            false
        }
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn iter(&self) -> Iter<T> {
        self.vec.iter()
    }
}

#[derive(Clone, Copy)]
pub struct Cycle(pub i32);

macro_rules! add_cycles {
    ($cycles:expr, $load_expr:expr) => {{
        let (cycles_used, read) = $load_expr;
        $cycles += cycles_used;
        read
    }}
}

impl Add for Cycle {
    type Output = Cycle;
    fn add(self, right: Cycle) -> Cycle {
        Cycle(self.0 + right.0)
    }
}

impl Add<i32> for Cycle {
    type Output = Cycle;
    fn add(self, right: i32) -> Cycle {
        Cycle(self.0 + right)
    }
}

impl AddAssign for Cycle {
    fn add_assign(&mut self, right: Cycle) {
        self.0 += right.0;
    }
}

impl AddAssign<i32> for Cycle {
    fn add_assign(&mut self, right: i32) {
        self.0 += right;
    }
}

macro_rules! check_watchpoint {
    ($arm:expr, $addr:expr) => {
        if $arm.watchpoints.contains($addr) {
            return StepEvent::TriggerWatchpoint($addr);
        }
    }
}

use arm7tdmi::Arm7TDMI;
#[inline(always)]
pub fn set_zn(arm: &mut Arm7TDMI, value: u32) {
    arm.cpsr.z = value == 0;
    arm.cpsr.n = (value as i32) < 0;
}

#[inline(always)]
pub fn add_set_vc(arm: &mut Arm7TDMI, a: u32, b: u32) {
    arm.cpsr.v = (a as i32).overflowing_add(b as i32).1;
    arm.cpsr.c = a.overflowing_add(b).1;
}

#[inline(always)]
pub fn sub_set_vc(arm: &mut Arm7TDMI, a: u32, b: u32) {
    arm.cpsr.v = (a as i32).overflowing_sub(b as i32).1;
    arm.cpsr.c = a.overflowing_sub(b).1;
}

#[inline(always)]
pub fn sign_extend(n: u32, n_bits: usize) -> u32 {
    let shift = 32 - n_bits;
    (((n << shift) as i32) >> shift) as u32
}

pub fn barrel_shift_lsl(_arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount >= 32 {
        0
    } else {
        rm << shift_amount
    }
}
pub fn barrel_shift_lsl_set_flags(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount == 32 {
        arm.cpsr.c = (rm & 1) != 0;
        0
    } else if shift_amount > 32 {
        arm.cpsr.c = false;
        0
    } else {
        if shift_amount != 0 {
            arm.cpsr.c = ((rm >> (32 - shift_amount)) & 1) != 0;
        }
        rm << shift_amount
    }
}

pub fn barrel_shift_lsr(_arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount >= 32 || shift_amount == 0 {
        0
    } else {
        rm >> shift_amount
    }
}
pub fn barrel_shift_lsr_set_flags(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount == 32 || shift_amount == 0 {
        arm.cpsr.c = (rm >> 31) != 0;
        0
    } else if shift_amount > 32 {
        arm.cpsr.c = false;
        0
    } else {
        arm.cpsr.c = ((rm >> (shift_amount - 1)) & 1) != 0;
        rm >> shift_amount
    }
}

pub fn barrel_shift_asr(_arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount >= 32 || shift_amount == 0 {
        // Return sign extension of rm
        if (rm >> 31) != 0 {
            0xFFFF_FFFF
        } else {
            0
        }
    } else {
        ((rm as i32) >> shift_amount) as u32
    }
}
pub fn barrel_shift_asr_set_flags(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount >= 32 || shift_amount == 0 { // Return sign extension of rm
        if (rm >> 31) != 0 {
            arm.cpsr.c = true;
            0xFFFF_FFFF
        } else {
            arm.cpsr.c = false;
            0
        }
    } else {
        arm.cpsr.c = (((rm as i32) >> (shift_amount - 1)) & 1) != 0;
        ((rm as i32) >> shift_amount) as u32
    }
}

pub fn barrel_shift_ror(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount == 0 { // RRX
        let carry_in = arm.cpsr.c as u32;
        (rm >> 1) | (carry_in << 31)
    } else {
        rm.rotate_right(shift_amount)
    }
}
pub fn barrel_shift_ror_set_flags(arm: &mut Arm7TDMI, rm: u32, shift_amount: u32) -> u32 {
    if shift_amount == 0 { // RRX
        let carry_in = arm.cpsr.c as u32;
        arm.cpsr.c = (rm & 1) != 0;
        (rm >> 1) | (carry_in << 31)
    } else {
        arm.cpsr.c = ((rm >> ((shift_amount - 1) & 0x1F)) & 1) != 0;
        rm.rotate_right(shift_amount)
    }
}
