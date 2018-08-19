use std::cmp::Ord;
use std::ops::{ Add, AddAssign, Sub, SubAssign };
use std::fmt;
use std::fmt::UpperHex;
use std::slice;

use interconnect::Interconnect;

use num::PrimInt;

#[allow(unused_imports)]
use log::LogKind::*;

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

fn foo<T: PrimInt, IO: PrimInt>(val: T, offset: u32) -> IO {
    use std::mem::size_of;
    match (size_of::<T>(), size_of::<IO>()) {
        (1, 1) | (2, 2) | (4, 4) => IO::from(val).unwrap(),
        (1, 2) | (1, 4) | (2, 4) => IO::from(val).unwrap() << (offset * 8) as usize,
        (2, 1) | (4, 1) | (4, 2) => IO::from(val >> (offset * 8) as usize).unwrap(),
        _ => unreachable!()
    }
}

pub trait Cache {
    const READ: fn(&Buffer, u32) -> Self;
    const WRITE: fn(&mut Buffer, u32, Self);
}

impl Cache for u8 {
    const READ: fn(&Buffer, u32) -> Self = Buffer::read8;
    const WRITE: fn(&mut Buffer, u32, Self) = Buffer::write8;
}

impl Cache for u16 {
    const READ: fn(&Buffer, u32) -> Self = Buffer::read16;
    const WRITE: fn(&mut Buffer, u32, Self) = Buffer::write16;
}

impl Cache for u32 {
    const READ: fn(&Buffer, u32) -> Self = Buffer::read32;
    const WRITE: fn(&mut Buffer, u32, Self) = Buffer::write32;
}

pub fn handle_read<T: PrimInt + UpperHex, IO: PrimInt>(
    ic: &Interconnect,
    addr: u32,
    case_addr_name: &str,
    case_addr: u32,
    getter: &Fn(&Interconnect) -> T
) -> (u32, IO) {
    use std::cmp;
    use std::mem::size_of;

    let size_of_t = size_of::<T>() as u32;
    let size_of_io_type = size_of::<IO>() as u32;

    let type_size = cmp::max(size_of_t, size_of_io_type);
    let mask: u32 = !(type_size - 1);
    if (addr & mask) == (case_addr & mask) {
        let offset = case_addr&!mask | addr&!mask;
        let value: T = getter(ic);
        trace!(IO, "Reading 0x{:X} from {} (0x{:X})",
                           value, case_addr_name, case_addr);
        (size_of_t, foo(value, offset))
    } else {
        (0, IO::zero())
    }
}

macro_rules! read_io_method {
    ($fn_name:ident, $io_type:ident, $($T:ident, $case_addr:expr, $getter:expr,)+) => {
        pub fn $fn_name(&self, addr: u32) -> $io_type {
            use utils::handle_read;
            use std::mem::size_of;

            let mut result: $io_type = 0;
            let mut bytes_handled = 0;
            debug_assert!(addr & 0xFF00_0000 == 0x0400_0000);

            $(
                let (bytes, res) = handle_read::<$T, $io_type>(self, addr, stringify!($case_addr), $case_addr, &$getter);
                result |= res;
                bytes_handled += bytes;
            )+

            let size_of_io_type = size_of::<$io_type>();
            if bytes_handled < size_of_io_type as u32 {
                warn!(IO, "Unhandled ioread{} at 0x400_0{:03X}", 8*size_of_io_type, addr & 0xFFF);
            }

            result
        }
    }
}

pub fn handle_write<T: Cache + PrimInt + UpperHex, IO: PrimInt>(
    ic: &mut Interconnect,
    addr: u32,
    value: IO,
    case_addr_name: &str,
    case_addr: u32,
    setter: &Fn(&mut Interconnect, T)
) -> u32 {
    use std::cmp;
    use std::mem::size_of;

    let size_of_t = size_of::<T>() as u32;
    let size_of_io_type = size_of::<IO>() as u32;

    let value: T = T::from(value).unwrap();

    let type_size: u32 = cmp::max(size_of_t, size_of_io_type);
    let mask: u32 = !(type_size - 1);
    if (addr & mask) == (case_addr & mask) {
        let offset: T = T::from(case_addr&!mask | addr&!mask).unwrap();
        let offset_t8: usize = offset.to_usize().unwrap() * 8;
        let addr: u32 = if size_of_io_type > size_of_t { addr } else { addr & mask };
        let whole: T = T::READ(&ic.io_cache, addr & 0xFFF);
        let new: T = if size_of_io_type > size_of_t {
            value >> offset_t8
        } else {
            whole & !(T::from(IO::max_value()).unwrap() << offset_t8) | (value << offset_t8)
        };
        T::WRITE(&mut ic.io_cache, addr & 0xFFF, new);
        setter(ic, new);
        trace!(IO, "Writing 0x{:X} to {} (0x{:X})", new, case_addr_name, case_addr);
        size_of_t
    } else {
        0
    }
}

macro_rules! write_io_method {
    ($fn_name:ident, $io_type:ident, $($T:ident, $case_addr:expr, $setter:expr,)+) => {
        fn $fn_name(&mut self, addr: u32, value: $io_type) {
            use utils::handle_write;
            use std::mem::size_of;
            let mut bytes_handled = 0;
            debug_assert!(addr & 0xFF00_0000 == 0x0400_0000);

            $(
                bytes_handled += handle_write::<$T, $io_type>(self, addr, value, stringify!($case_addr), $case_addr, &$setter);
            )+

            let size_of_io_type = size_of::<$io_type>();
            if bytes_handled < size_of_io_type as u32 {
                warn!(IO, "Unhandled iowrite{} at 0x400_0{:03X} of {:X}", 8*size_of_io_type, addr & 0xFFF, value);
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

/*
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
}*/

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

    pub fn slice_u8(&self, base: u32, size: u32) -> &[u8] {
        let base = base as usize;
        let size = size as usize;
        &self.buffer[base..base + size]
    }

    pub fn slice_u16(&self, base: u32, size: u32) -> &[u16] {
        let base = base as usize;
        let size = size as usize;
        assert!(base & 1 == 0);
        assert!(base + size + 1 < self.buffer.len());
        unsafe {
            slice::from_raw_parts(self.buffer[base..base+size].as_ptr() as *const u16, size)
        }
    }

    pub fn slice_u32(&self, base: u32, size: u32) -> &[u32] {
        let base = base as usize;
        let size = size as usize;
        assert!(base & 3 == 0);
        unsafe {
            slice::from_raw_parts(self.buffer[base..base+size].as_ptr() as *const u32, size)
        }
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

// TODO: This is only needed because Latched is used inside Dma, which is forced to be Copy by the
// poor support for fixed length arrays currently. Hopefully soon we'll get const parameters in
// generics.
#[derive(Clone, Copy)]
pub struct Latched<T: Copy> {
    latched: T,
    next: T,
}

impl<T: Copy> Latched<T> {
    pub fn new(initial: T) -> Latched<T> {
        Latched {
            latched: initial,
            next: initial,
        }
    }

    pub fn get(&self) -> T {
        self.latched
    }

    pub fn set(&mut self, next: T) {
        self.next = next;
    }

    pub fn latch(&mut self) {
        self.latched = self.next;
    }
}
