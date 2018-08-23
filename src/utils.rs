use std::cmp::Ord;
use std::ops::{ Add, AddAssign, Sub, SubAssign };
use std::fmt;
use std::slice;

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

    pub fn get_next(&self) -> T { self.next }

    pub fn latch(&mut self) {
        self.latched = self.next;
    }
}
