use std::io::{self, Read, Write};
use std::net::{TcpListener, TcpStream, ToSocketAddrs};
use num_traits::PrimInt;
use crate::log::LogKind::GDB;
use std::fmt::Arguments;
use crate::gba::Gba;
use crate::bus::{BusPtr, Bus};
use crate::utils::OrderedSet;

type GResult = Result<(), failure::Error>;

pub struct GdbStub {
    listener: Option<TcpListener>,
    stream: Option<TcpStream>,
    blocking: bool,
    no_ack_mode: bool,
    gba: Box<Gba>,
    bus_snooper: Box<BusDebugSnooper>,
}

impl GdbStub {
    pub fn new(blocking: bool, mut gba: Box<Gba>) -> GdbStub {
        let mut bus_snooper = BusDebugSnooper::wrap(gba.arm.bus.clone());
        gba.arm.bus = BusPtr::new(bus_snooper.as_mut() as *mut dyn Bus);
        GdbStub {
            listener: None,
            stream: None,
            blocking,
            no_ack_mode: false,
            gba,
            bus_snooper,
        }
    }

    pub fn listen(&mut self, addr: impl ToSocketAddrs) -> GResult {
        let listener = TcpListener::bind(addr)?;
        listener.set_nonblocking(!self.blocking)?;
        self.listener = Some(listener);
        Ok(())
    }

    pub fn update(&mut self) -> GResult {
        if self.listener.is_none() {
            return Ok(());
        }
        let listener = self.listener.as_mut().unwrap();

        if self.stream.is_none() {
            let (stream, addr) = if let Some(t) = transpose_would_block(listener.accept())? {
                t
            } else {
                return Ok(());
            };
            note!(GDB, "TcpListener accepted a connection from {}", addr);
            stream.set_nonblocking(!self.blocking)?;
            self.stream = Some(stream);
        }

        // Unwrapping because we ensured it's Some above
        let stream = self.stream.as_mut().unwrap();

        let mut bytes = [0u8; 1200];
        let msg: &[u8];
        if let Some(amount) = transpose_would_block(stream.read(&mut bytes[..]))? {
            if amount == 0 {
                trace!(GDB, "Received 0 bytes, closing TcpStream..");
                self.stream = None;
                return Ok(());
            } else {
                let ascii_string = bytes_as_ascii(&bytes[..amount]);
                trace!(GDB, "Received {} bytes: {:?}", amount, ascii_string);
                msg = &bytes[..amount];
            }
        } else {
            return Ok(());
        }

        match msg[0] {
            b'+' => return Ok(()), // ack
            b'-' => return Ok(()), // nak
            b'$' => {
                // Continue on to process this command
            }
            0x03 => {
                // Enter debugger
                return Ok(());
            }
            first => {
                warn!(GDB, "packet error; first byte = '{:02X}'", first);
                stream.write(&[b'-'])?;
                return Ok(())
            }
        }

        if msg[msg.len() - 3] != b'#' {
            return self.nak()
        }

        let (message_body, their_checksum_str) = split_at(&msg[1..], b'#')?;

        let our_checksum = checksum(message_body);
        let their_checksum = hex_to_int(their_checksum_str)?;

        if our_checksum != their_checksum {
            warn!(GDB, "incorrect checksum: our_checksum = {}, their_checksum = {}", our_checksum, their_checksum);
            return self.nak();
        }

        self.ack()?;

        let message_type = msg[1];
        let message_body = &message_body[1..];
        match message_type {
            b'?' => {
                // Let's say we halted due to SIGINT
                self.send(b"S02")?;
            }
            b'g' => {
                self.read_gprs()?;
            }
            b'G' => {
                self.write_gprs(message_body)?;
            }
            b'H' => {
                // Sets the thread to use for subsequent invocations of a particular command.
                // We only have 1 thread, so acknowledge and do nothing.
                self.send(b"OK")?;
            }
            b'm' => {
                self.read_memory(message_body)?;
            }
            b'M' => {
                self.write_memory(message_body)?;
            }
            b'p' => {
                self.read_gpr(message_body)?;
            }
            b'P' => {
                self.write_gpr(message_body)?;
            }
            b'q' => {
                self.process_qread_command(message_body)?;
            }
            b'Q' => {
                self.process_qwrite_command(message_body)?;
            }
            b'z' => {
                self.process_z_command(message_body, false)?;
            }
            b'Z' => {
                self.process_z_command(message_body, true)?;
            }
            _ => {
                self.unrecognised_command()?;
            }
        }

        Ok(())
    }

    fn process_qread_command(&mut self, msg: &[u8]) -> GResult {
        match msg {
            b"TStatus" => {
                // No trace is running, we don't support tracepoints
                self.send(b"T0")
            }
            b"fThreadInfo" => {
                // First thread in list: thread ID 1
                self.send(b"m1")
            }
            b"sThreadInfo" => {
                // End of list, thread ID 1 is the only thread
                self.send(b"l")
            }
            b"C" => {
                // The current thread is thread 1, we only have 1 thread..
                self.send(b"QC1")
            }
            b"Attached" => {
                // We, the GDB server, are always already attached to a process
                self.send(b"1")
            }
            b"HostInfo" => {
                const MACH_O_ARM: u32 = 12;
                const MACH_O_ARM_V4T: u32 = 5;
                self.send_fmt(format_args!("cputype:{};cpusubtype:{};ostype:none;vendor:none;endian:little;ptrsize:4;", MACH_O_ARM, MACH_O_ARM_V4T))
            }
            _ => {
                if let Some(tail) = strip_prefix(msg, b"Supported:") {
                    self.process_qsupported_command(tail)
                } else {
                    self.unrecognised_command()
                }
            }
        }
    }

    fn process_qsupported_command(&mut self, msg: &[u8]) -> GResult {
        for capability in msg.split(|&b| b == b';') {
            match capability {
                b"swbreak+" => {
                }
                b"hwbreak+" => {
                }
                // TODO: Support "vContSupported+"?
                _ => {}
            }
        }

        self.send(b"swbreak+;hwbreak+;arm+")
    }

    fn process_qwrite_command(&mut self, msg: &[u8]) -> GResult {
        match msg {
            b"StartNoAckMode" => {
                self.no_ack_mode = true;
                self.send(b"OK")
            }
            _ => {
                self.unrecognised_command()
            }
        }
    }

    fn read_gprs(&mut self) -> GResult {
        let mut reg_string = Vec::with_capacity(16 * 8);
        for reg in self.gba.arm.regs.iter() {
            reg_string.write(&int_to_hex_le(*reg))?;
        }
        self.send(&reg_string)
    }

    fn write_gprs(&mut self, msg: &[u8]) -> GResult {
        for (i, value) in msg.chunks_exact(8).map(hex_to_int_le).enumerate() {
            self.gba.arm.set_reg(i, value?);
        }
        self.send(b"OK")
    }

    fn read_gpr(&mut self, msg: &[u8]) -> GResult {
        let reg_index: usize = hex_to_int(msg)?;
        let reg = if reg_index == 25 {
            self.gba.arm.cpsr.into()
        } else if reg_index < 16 {
            self.gba.arm.regs[reg_index]
        } else {
            return self.send(b"E00");
        };
        self.send(&int_to_hex_le(reg))
    }

    fn write_gpr(&mut self, msg: &[u8]) -> GResult {
        let (reg_index_str, value_str) = split_at(msg, b'=')?;
        let reg_index = hex_to_int(reg_index_str)?;
        let value = hex_to_int_le(value_str)?;
        self.gba.arm.set_reg(reg_index, value);
        self.send(b"OK")
    }

    fn read_memory(&mut self, msg: &[u8]) -> GResult {
        let (addr_str, len_str) = split_at(msg, b',')?;
        let addr: u32 = hex_to_int(addr_str)?;
        let len: u32 = hex_to_int(len_str)?;
        let mut result = Vec::<u8>::with_capacity(2 * len as usize);
        for i in addr..addr + len {
            let (_, byte) = self.gba.debug_read8(i);
            result.write_fmt(format_args!("{:02X}", byte))?;
        }
        self.send(&result)
    }

    fn write_memory(&mut self, msg: &[u8]) -> GResult {
        let (addr_str, len_str) = split_at(msg, b',')?;
        let (len_str, data_str) = split_at(len_str, b':')?;
        let start_addr: u32 = hex_to_int(addr_str)?;
        let len: u32 = hex_to_int(len_str)?;
        let data = data_str
            .chunks(2)
            .map(hex_to_int)
            .collect::<Result<Vec<u8>, failure::Error>>()?;

        for (addr, byte) in (start_addr..start_addr+len).zip(data) {
            self.gba.debug_write8(addr, byte);
        }

        self.send(b"OK")
    }

    fn process_z_command(&mut self, msg: &[u8], is_insert: bool) -> GResult {
        let (type_str, addr_str) = split_at(msg, b',')?;
        let (addr_str, kind_str) = split_at(addr_str, b',')?;
        let kind: u32 = hex_to_int(kind_str)?;
        let start_addr = hex_to_int(addr_str)?;

        let addr_set: &mut OrderedSet<u32> = match type_str {
            b"0" | b"1" if kind != 2 && kind != 4 => {
                return self.unrecognised_command();
            }
            b"0" => { // software breakpoint
                // TODO: Implement this?
                return self.unrecognised_command();
            }
            b"1" => { // hardware breakpoint
                &mut self.bus_snooper.breakpoints
            }
            b"2" => { // write watchpoint
                &mut self.bus_snooper.write_watchpoints
            }
            b"3" => { // read watchpoint
                &mut self.bus_snooper.read_watchpoints
            }
            b"4" => { // access watchpoint
                &mut self.bus_snooper.access_watchpoints
            }
            _ => {
                return self.unrecognised_command();
            }
        };

        for addr in start_addr..start_addr+kind {
            if is_insert {
                addr_set.insert(addr);
            } else {
                addr_set.remove(addr);
            }
        }

        self.send(b"OK")
    }

    fn send_fmt(&mut self, args: Arguments) -> GResult {
        let mut bytes = Vec::<u8>::new();
        bytes.write_fmt(args)?;
        self.send(&bytes)
    }

    fn send(&mut self, message: &[u8]) -> GResult {
        let mut response = Vec::new();
        response.push(b'$');
        response.extend_from_slice(message);
        response.push(b'#');
        let checksum = checksum(message);
        write!(response, "{:02X}", checksum)?;
        self.send_raw(&response)
    }

    fn ack(&mut self) -> GResult {
        if self.no_ack_mode {
            return Ok(());
        }
        self.send_raw(b"+")
    }

    fn nak(&mut self) -> GResult {
        if self.no_ack_mode {
            return Ok(());
        }
        self.send_raw(b"-")
    }

    fn unrecognised_command(&mut self) -> GResult {
        // https://www-zeuthen.desy.de/unix/unixguide/infohtml/gdb/Overview.html
        // The empty response "$#00" indicates to the GDB client that the command is not supported
        self.send(&[])
    }

    fn write_fmt(&mut self, args: Arguments) -> GResult {
        use std::io::Write;
        let mut v = Vec::new();
        v.write_fmt(args)?;
        Ok(())
    }

    fn send_raw(&mut self, bytes: &[u8]) -> GResult {
        if let Some(stream) = self.stream.as_mut() {
            let amount = stream.write(bytes);
            trace!(GDB, "wrote {:?} bytes of {} ({:?})", amount, bytes.len(), bytes_as_ascii(bytes));
            amount?;
        } else {
            trace!(GDB, "tried to send {} bytes but stream was None", bytes.len());
        }
        Ok(())
    }
}

enum StopReason {
    ReadWatchpoint(u32),
    WriteWatchpoint(u32),
    AccessWatchpoint(u32),
    Breakpoint(u32),
}

pub struct BusDebugSnooper {
    delegate: BusPtr,
    breakpoints: OrderedSet<u32>,
    read_watchpoints: OrderedSet<u32>,
    write_watchpoints: OrderedSet<u32>,
    access_watchpoints: OrderedSet<u32>,
    stop_reason: Option<StopReason>,
}

impl BusDebugSnooper {
    pub fn wrap(delegate: BusPtr) -> Box<BusDebugSnooper> {
        Box::new(BusDebugSnooper {
            delegate,
            breakpoints: OrderedSet::new(),
            read_watchpoints: OrderedSet::new(),
            write_watchpoints: OrderedSet::new(),
            access_watchpoints: OrderedSet::new(),
            stop_reason: None,
        })
    }

    fn check_read(&mut self, addr: u32) {
        if self.read_watchpoints.contains(addr) {
            self.stop_reason = Some(StopReason::ReadWatchpoint(addr));
        } else if self.access_watchpoints.contains(addr) {
            self.stop_reason = Some(StopReason::AccessWatchpoint(addr));
        }
    }

    fn check_write(&mut self, addr: u32) {
        if self.write_watchpoints.contains(addr) {
            self.stop_reason = Some(StopReason::WriteWatchpoint(addr));
        }
    }

    fn check_exec(&mut self, addr: u32) {
        if self.breakpoints.contains(addr) {
            self.stop_reason = Some(StopReason::Breakpoint(addr));
        }
    }
}

impl Bus for BusDebugSnooper {
    fn read8(&mut self, addr: u32) -> u8 {
        self.check_read(addr);
        self.delegate.read8(addr)
    }

    fn read16(&mut self, addr: u32) -> u16 {
        self.check_read(addr);
        self.delegate.read16(addr)
    }

    fn read32(&mut self, addr: u32) -> u32 {
        self.check_read(addr);
        self.delegate.read32(addr)
    }

    fn write8(&mut self, addr: u32, value: u8) {
        self.check_write(addr);
        self.delegate.write8(addr, value);
    }

    fn write16(&mut self, addr: u32, value: u16) {
        self.check_write(addr);
        self.delegate.write16(addr, value);
    }

    fn write32(&mut self, addr: u32, value: u32) {
        self.check_write(addr);
        self.delegate.write32(addr, value);
    }

    fn exec_thumb_slow(&mut self, addr: u32) -> u16 {
        self.check_exec(addr);
        self.delegate.exec_thumb_slow(addr)
    }

    fn exec_arm_slow(&mut self, addr: u32) -> u32 {
        self.check_exec(addr);
        self.delegate.exec_arm_slow(addr)
    }

    fn add_internal_cycles(&mut self, cycles: i64) {
        self.delegate.add_internal_cycles(cycles);
    }
}

fn hex_to_int<T: PrimInt>(bstr: &[u8]) -> Result<T, failure::Error> {
    // TODO: Error handle
    if let Ok(result) = T::from_str_radix(std::str::from_utf8(bstr)?, 16) {
        Ok(result)
    } else {
        Err(failure::err_msg("Failed to parse hex"))
    }
}

fn hex_to_int_le(bstr: &[u8]) -> Result<u32, failure::Error> {
    if bstr.len() != 8 {
        return Err(failure::err_msg(""))
    }
    let mut bytes = [0u8; 4];
    for (i, value) in bstr.chunks(2).map(hex_to_int).enumerate() {
        bytes[i] = value?;
    }
    Ok(u32::from_le_bytes(bytes))
}

fn int_to_hex_le(reg: u32) -> [u8; 8] {
    const CHARS: &[u8] = b"012346789ABCDEF";
    let mut result = [0u8; 8];
    for (i, &b) in reg.to_le_bytes().iter().enumerate() {
        result[2*i + 0] = CHARS[(b >> 4) as usize];
        result[2*i + 1] = CHARS[(b & 0xF) as usize];
    }
    result
}

fn transpose_would_block<T>(r: Result<T, io::Error>) -> Result<Option<T>, io::Error> {
    match r {
        Ok(t) => Ok(Some(t)),
        Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => Ok(None),
        Err(e) => Err(e),
    }
}

fn strip_prefix<'a>(msg: &'a [u8], prefix: &[u8]) -> Option<&'a [u8]> {
    if msg.starts_with(prefix) {
        Some(&msg[prefix.len()..])
    } else {
        None
    }
}

fn bytes_as_ascii(bytes: &[u8]) -> String {
    let ascii_chars = bytes
        .iter()
        .flat_map(|c| std::ascii::escape_default(*c))
        .collect::<Vec<_>>();
    String::from_utf8(ascii_chars).unwrap()
}

fn split_at(haystack: &[u8], needle: u8) -> Result<(&[u8], &[u8]), failure::Error> {
    let split_pos = haystack
        .iter()
        .position(|&c| c == needle)
        .ok_or_else(|| failure::err_msg(format!("missing '{}'", needle as char)))?;
    Ok((&haystack[..split_pos], &haystack[split_pos + 1..]))
}

fn checksum(bytes: &[u8]) -> u8 {
    bytes
        .iter()
        .map(|&c| c as u64)
        .sum::<u64>() as u8
}