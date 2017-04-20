use std::cmp::Ord;
use std::ops::{ Add, AddAssign, Sub, SubAssign };
use std::fmt;

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
            debug_assert!(addr & 0xFF00_0000 == 0x0400_0000);

            $({
                let type_size = cmp::max(size!($T), size!($io_type));
                let mask: u32 = !(type_size - 1);
                if (addr & mask) == ($case_addr & mask) {
                    bytes_handled += size!($T);
                    // May be unused by foo!()
                    let _offset = $case_addr&!mask | addr&!mask;
                    let value: $T = $getter(self);
                    result |= foo!(from: $T, to: $io_type, value, _offset);
                    trace!(IO, "Reading 0x{:X} from {} (0x{:X})",
                           value, stringify!($case_addr), $case_addr);
                }
            })+

            if bytes_handled < size!($io_type) {
                warn!(IO, "Unhandled ioread{} at 0x400_0{:03X}", 8*size!($io_type), addr & 0xFFF);
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
            debug_assert!(addr & 0xFF00_0000 == 0x0400_0000);

            $({
                let type_size = cmp::max(size!($T), size!($io_type));
                let mask: u32 = !(type_size - 1);
                if (addr & mask) == ($case_addr & mask) {
                    bytes_handled += size!($T);
                    let offset = ($case_addr&!mask | addr&!mask) as $T;
                    let addr = if size!($io_type) > size!($T) { addr } else { addr & mask };
                    let whole = read_cache!(self.io_cache, $T, addr & 0xFFF);
                    let new: $T = if size!($io_type) > size!($T) {
                        (value >> (offset*8)) as $T
                    } else {
                        whole & !(($io_type::max_value() as $T) << (offset*8)) | ((value as $T) << (offset*8))
                    };
                    write_cache!(self.io_cache, $T, addr & 0xFFF, new);
                    $setter(self, new);
                    trace!(IO, "Writing 0x{:X} to {} (0x{:X})", new, stringify!($case_addr), $case_addr);
                }
            })+

            if bytes_handled < size!($io_type) {
                warn!(IO, "Unhandled iowrite{} at 0x400_0{:03X} of {:X}", 8*size!($io_type), addr & 0xFFF, value);
            }
        }
    }
}

macro_rules! impl_io_map {
    (
        [$struct_type:ty]
        $(($T:ident, $case_addr:expr) {
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
        io_cache: Buffer,
        a: u32,
        b: u16,
        c: u8,
        d: u8,
        s: u16,
        t: u16,
    }

    const A: u32 = 0;
    const B: u32 = 6;
    const C: u32 = 4;
    const D: u32 = 5;
    const S: u32 = 0x10;
    const T: u32 = 0x12;

    impl_io_map! {
        [Foo]
        (u32, A) {
            read => |this: &Foo| {
                this.a
            },
            write => |this: &mut Foo, value| {
                this.a = value;
            }
        }
        (u16, B) {
            read => |this: &Foo| {
                this.b
            },
            write => |this: &mut Foo, value| {
                this.b = value;
            }
        }
        (u8, C) {
            read => |this: &Foo| {
                this.c
            },
            write => |this: &mut Foo, value| {
                this.c = value;
            }
        }
        (u8, D) {
            read => |this: &Foo| {
                this.d
            },
            write => |this: &mut Foo, value| {
                this.d = value;
            }
        }
        (u16, S) {
            read => |this: &Foo| {
                this.s
            },
            write => |this: &mut Foo, value| {
                this.s = value;
            }
        }
        (u16, T) {
            read => |this: &Foo| {
                this.t
            },
            write => |this: &mut Foo, value| {
                this.t = value;
            }
        }
    }

    let mut foo = Foo {
        io_cache: Buffer::new(&[0xEF, 0xBE, 0xAD, 0xDE, 0xFE, 0xCA, 0xAF, 0xDE, 0,0,0,0,0,0,0,0, 0,0, 0,0]),
        a: 0xDEADBEEF,
        b: 0xCAFE,
        c: 0xAF,
        d: 0xDE,
        s: 0, t: 0,
    };

    assert_eq!(foo.ioread32(0x10), 0);
    foo.iowrite16(0x10, 0x1234);
    assert_eq!(foo.ioread32(0x10), 0x0000_1234);
    foo.iowrite32(0x10, 0xDEADBEEF);
    assert_eq!(foo.ioread8(0x10), 0xEF);
    assert_eq!(foo.ioread8(0x11), 0xBE);
    assert_eq!(foo.ioread8(0x12), 0xAD);
    assert_eq!(foo.ioread8(0x13), 0xDE);

    assert_eq!(foo.ioread16(0x10), 0xBEEF);
    assert_eq!(foo.ioread16(0x12), 0xDEAD);
/*
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
    assert_eq!(foo.ioread8(7), 0x12);*/
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Cycle(pub i64);

impl Cycle {
    pub fn max() -> Cycle {
        Cycle(i64::max_value())
    }
}

impl fmt::Display for Cycle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}cy", self.0)
    }
}

impl Add for Cycle {
    type Output = Cycle;
    fn add(self, right: Cycle) -> Cycle {
        Cycle(self.0 + right.0)
    }
}

impl Add<i64> for Cycle {
    type Output = Cycle;
    fn add(self, right: i64) -> Cycle {
        Cycle(self.0 + right)
    }
}

impl AddAssign for Cycle {
    fn add_assign(&mut self, right: Cycle) {
        self.0 += right.0;
    }
}

impl AddAssign<i64> for Cycle {
    fn add_assign(&mut self, right: i64) {
        self.0 += right;
    }
}

impl Sub for Cycle {
    type Output = Cycle;
    fn sub(self, right: Cycle) -> Cycle {
        Cycle(self.0 - right.0)
    }
}

impl Sub<i64> for Cycle {
    type Output = Cycle;
    fn sub(self, right: i64) -> Cycle {
        Cycle(self.0 - right)
    }
}

impl SubAssign for Cycle {
    fn sub_assign(&mut self, right: Cycle) {
        self.0 -= right.0;
    }
}

impl SubAssign<i64> for Cycle {
    fn sub_assign(&mut self, right: i64) {
        self.0 -= right;
    }
}

#[inline(always)]
pub fn sign_extend(n: u32, n_bits: usize) -> u32 {
    let shift = 32 - n_bits;
    (((n << shift) as i32) >> shift) as u32
}
