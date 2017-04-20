use std::cell::Cell;
use utils::{ Buffer, Cycle };

const GAMEPAK_PAGE_SIZE: u32 = 128*1024;
const GAMEPAK_PAGE_MASK: u32 = GAMEPAK_PAGE_SIZE - 1;
const GAMEPAK_MAX_SIZE: u32 = 32 * 1024 * 1024;
const GAMEPAK_MAX_SIZE_MASK: u32 = GAMEPAK_MAX_SIZE - 1;

#[derive(Clone, Copy)]
pub struct WaitState {
    pub non_seq: Cycle,
    pub seq: Cycle,
}

impl WaitState {
    fn new(non_seq: i64, seq: i64) -> WaitState {
        WaitState {
            non_seq: Cycle(non_seq),
            seq: Cycle(seq),
        }
    }
}

#[derive(Clone)]
pub struct GamePak {
    buffer: Buffer,
    next_seq_addr: Cell<u32>,
    // TODO: These are currently accessed in the Interconnect, should they be encapsulated?
    pub wait_states: [WaitState; 3],
}

impl GamePak {
    pub fn new(rom: &[u8]) -> GamePak {
        GamePak {
            buffer: Buffer::new(rom),
            next_seq_addr: Cell::new(0),
            wait_states: [
                WaitState::new(4, 2),
                WaitState::new(4, 4),
                WaitState::new(4, 8),
            ],
        }
    }

    pub fn read8(&self, addr: u32) -> (Cycle, u8) {
        let (cycles, read) = self.read16(addr & !1);
        let read = (read >> (addr & 1)) as u8;
        self.next_seq_addr.set((addr + 1) & !1);
        (cycles, read)
    }

    pub fn read16(&self, addr: u32) -> (Cycle, u16) {
        // If we are loading from the address that comes after the previous one and aren't
        // crossing a page boundary, we get the faster sequential load timing.
        let seq = self.next_seq_addr.get() == addr && addr & GAMEPAK_PAGE_MASK != 0;
        assert!(addr & 1 == 0);

        let cycle = match addr >> 24 {
            0x8 | 0x9 if seq => self.wait_states[0].seq,
            0xA | 0xB if seq => self.wait_states[1].seq,
            0xC | 0xD if seq => self.wait_states[2].seq,
            0x8 | 0x9 if !seq => self.wait_states[0].non_seq,
            0xA | 0xB if !seq => self.wait_states[1].non_seq,
            0xC | 0xD if !seq => self.wait_states[2].non_seq,
            _ => unreachable!(),
        };

        let read = if (addr as usize) < self.buffer.len() {
            self.buffer.read16(addr & GAMEPAK_MAX_SIZE_MASK)
        } else {
            // When out of bounds, the data read is just the last thing on the bus, ie the address.
            (addr >> 1 & 0xFFFF) as u16
        };
        self.next_seq_addr.set(addr + 2);

        (cycle, read)
    }

    pub fn read32(&self, addr: u32) -> (Cycle, u32) {
        let (cycles1, read1) = self.read16(addr);
        let (cycles2, read2) = self.read16(addr + 2);
        self.next_seq_addr.set((addr + 4) & !1);
        (cycles1 + cycles2, (read1 as u32) | ((read2 as u32) << 16))
    }
}