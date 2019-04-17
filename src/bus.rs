use std::ops::{ Deref, DerefMut };
use std::mem;

pub trait Bus {
    fn read8(&mut self, addr: u32) -> u8;
    fn read16(&mut self, addr: u32) -> u16;
    fn read32(&mut self, addr: u32) -> u32;

    fn read_ext_i8(&mut self, addr: u32) -> u32 {
        self.read8(addr) as i8 as u32
    }

    fn read_ext_i16(&mut self, addr: u32) -> u32 {
        self.read16(addr) as i16 as u32
    }

    fn read_ext_u8(&mut self, addr: u32) -> u32 {
        self.read8(addr) as u32
    }

    fn read_ext_u16(&mut self, addr: u32) -> u32 {
        self.read16(addr) as u32
    }

    fn write8(&mut self, addr: u32, value: u8);
    fn write16(&mut self, addr: u32, value: u16);
    fn write32(&mut self, addr: u32, value: u32);

    fn exec_thumb_slow(&mut self, addr: u32) -> u16;

    fn exec_arm_slow(&mut self, addr: u32) -> u32;

    fn add_internal_cycles(&mut self, cycles: i64);
}

#[derive(Clone)]
pub struct BusPtr(*mut dyn Bus);

impl BusPtr {
    pub fn null() -> BusPtr {
        BusPtr(unsafe { mem::zeroed() })
    }

    pub fn new(bus: *mut dyn Bus) -> Self {
        Self(bus)
    }
}

impl Default for BusPtr {
    fn default() -> Self {
        Self::null()
    }
}

impl Deref for BusPtr {
    type Target = Bus;

    fn deref(&self) -> &Self::Target {
        unsafe {
            &*self.0
        }
    }
}

impl DerefMut for BusPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            &mut *self.0
        }
    }
}
