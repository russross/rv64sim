pub mod ui;

use crossterm::tty::IsTty;
use self::ui::*;
use std::cmp::min;
use std::collections::HashMap;
use std::fmt::{self, Write as FmtWrite};
use std::fs::File;
use std::io::{self, Read, Write};
use std::os::unix::io::{AsRawFd};
use std::rc::Rc;

const STACK_SIZE: i64 = 4096;

struct Segment {
    start: i64,
    end: i64,
    mem: Vec<u8>,
    init: Vec<u8>,
    writeable: bool,
    executable: bool,
}

impl Segment {
    fn new(start: i64, end: i64, writeable: bool, executable: bool, init: Vec<u8>) -> Self {
        assert!(start > 0 && end > start && writeable != executable);
        assert!(init.len() <= (end - start) as usize);
        Self {
            start,
            end,
            mem: Vec::new(),
            init,
            writeable,
            executable,
        }
    }

    fn in_range(&self, addr: i64, size: i64) -> bool {
        addr >= self.start && addr+size <= self.end
    }

    fn reset(&mut self) {
        self.mem = self.init.clone();
        self.mem.resize((self.end - self.start) as usize, 0);
    }

    fn load(&self, addr: i64, size: i64, effects: &mut Option<Effects>) -> &[u8] {
        assert!(self.in_range(addr, size));
        let raw = &self.mem[(addr - self.start) as usize .. (addr+size - self.start) as usize];
        if let Some(effects) = effects {
            assert!(effects.mem_read.is_none());
            effects.mem_read = Some(MemoryValue { address: addr, value: raw.to_vec() });
        }
        raw
    }

    fn store(&mut self, addr: i64, raw: &[u8], effects: &mut Option<Effects>) {
        assert!(self.in_range(addr, raw.len() as i64));
        let offset = (addr - self.start) as usize;
        if let Some(effects) = effects {
            assert!(effects.mem_write.is_none());
            let old_val = self.mem[offset..offset + raw.len()].to_vec();
            effects.mem_write = Some((
                MemoryValue { address: addr, value: old_val },
                MemoryValue { address: addr, value: raw.to_vec() }
            ));
        }
        self.mem[offset..offset + raw.len()].copy_from_slice(raw);
    }
}

pub struct Machine {
    segments: Vec<Segment>,
    pc_start: i64,
    global_pointer: i64,
    address_symbols: HashMap<i64, String>,
    other_symbols: HashMap<String, i64>,
    stack_start: i64,
    stack_end: i64,
    data_start: i64,
    data_end: i64,
    text_start: i64,
    text_end: i64,
    x: [i64; 32],
    pc: i64,
    stdout: Vec<u8>,
    stdin: Vec<u8>,
    stack_frames: Vec<i64>,
    effects: Option<Effects>,
    most_recent_memory: i64,
    most_recent_data: (i64, usize),  // (address, size)
    most_recent_stack: (i64, usize), // (address, size)
}

impl Machine {
   fn new(
       mut segments: Vec<Segment>,
       pc_start: i64,
       global_pointer: i64,
       address_symbols: HashMap<i64, String>,
       other_symbols: HashMap<String, i64>,
   ) -> Self {
       let mut stack_start = 0x100000 - STACK_SIZE;
       for segment in &segments {
           if segment.end + STACK_SIZE >= stack_start {
               stack_start = (segment.end + STACK_SIZE*2 - 1) & (STACK_SIZE-1);
           }
       }
       
       let stack_end = stack_start + STACK_SIZE;
       let mut data_start = stack_end - 8;
       let mut data_end = 0;
       let mut text_start = stack_end;
       let mut text_end = 0;
       
       for segment in &segments {
           if segment.executable {
               text_start = text_start.min(segment.start);
               text_end = text_end.max(segment.end);
           } else {
               data_start = data_start.min(segment.start);
               data_end = data_end.max(segment.end);
           }
       }
       
       if data_end == 0 {
           data_end = data_start;
       }
       
       segments.push(Segment::new(stack_start, stack_end, true, false, Vec::new()));
       
       let mut machine = Self {
           segments,
           pc_start,
           global_pointer,
           address_symbols,
           other_symbols,
           stack_start,
           stack_end,
           data_start,
           data_end,
           text_start,
           text_end,
           x: [0; 32],
           pc: pc_start,
           stdout: Vec::new(),
           stdin: Vec::new(),
           stack_frames: Vec::new(),
           effects: None,
           most_recent_memory: 0,
           most_recent_data: (0, 0),
           most_recent_stack: (0, 0),
       };
       
       machine.reset();
       machine
   }

   fn reset(&mut self) {
       for segment in &mut self.segments {
           segment.reset();
       }
       
       self.x = [0; 32];
       self.x[2] = self.stack_end;
       self.pc = self.pc_start;
       
       self.stdout.clear();
       self.stdin.clear();
       self.stack_frames.clear();
       self.effects = None;
   }

    fn set_most_recent_memory(&mut self, sequence: &[Effects], seq_i: usize) {
        self.most_recent_memory = if self.data_start > 0 {
            self.data_start
        } else {
            self.stack_end - 8
        };
        self.most_recent_data = (self.data_start, 0);
        self.most_recent_stack = (self.stack_end - 8, 0);

        let mut stack = false;
        let mut data = false;

        for effect in sequence[..=seq_i].iter().rev() {
            let (address, value_len) = if let Some(read) = &effect.mem_read {
                (read.address, read.value.len())
            } else if let Some((_, write)) = &effect.mem_write {
                (write.address, write.value.len())
            } else {
                continue;
            };

            if !stack && address >= self.stack_start {
                self.most_recent_stack = (address, value_len);
                if !data {
                    self.most_recent_memory = address;
                }
                stack = true;
            }

            if !data && address < self.data_end {
                self.most_recent_data = (address, value_len);
                if !stack {
                    self.most_recent_memory = address;
                }
                data = true;
            }

            if stack && data {
                break;
            }
        }
    }

   fn load(&mut self, addr: i64, size: i64) -> Result<Vec<u8>, String> {
        for segment in &self.segments {
            if segment.in_range(addr, size) {
                let raw = segment.load(addr, size, &mut self.effects);
                return Ok(raw.to_vec());
            }
        }
        Err(format!("segfault: load addr={:#x} size={}", addr, size))
    }

    fn load_i8(&mut self, addr: i64) -> Result<i64, String> {
        let bytes = self.load(addr, 1)?;
        Ok(i8::from_le_bytes(bytes[..1].try_into().unwrap()) as i64)
    }

    fn load_u8(&mut self, addr: i64) -> Result<i64, String> {
        let bytes = self.load(addr, 1)?;
        Ok(u8::from_le_bytes(bytes[..1].try_into().unwrap()) as i64)
    }

    fn load_i16(&mut self, addr: i64) -> Result<i64, String> {
        let bytes = self.load(addr, 2)?;
        Ok(i16::from_le_bytes(bytes[..2].try_into().unwrap()) as i64)
    }

    fn load_u16(&mut self, addr: i64) -> Result<i64, String> {
        let bytes = self.load(addr, 2)?;
        Ok(u16::from_le_bytes(bytes[..2].try_into().unwrap()) as i64)
    }

    fn load_i32(&mut self, addr: i64) -> Result<i64, String> {
        let bytes = self.load(addr, 4)?;
        Ok(i32::from_le_bytes(bytes[..4].try_into().unwrap()) as i64)
    }

    fn load_u32(&mut self, addr: i64) -> Result<i64, String> {
        let bytes = self.load(addr, 4)?;
        Ok(u32::from_le_bytes(bytes[..4].try_into().unwrap()) as i64)
    }

    fn load_i64(&mut self, addr: i64) -> Result<i64, String> {
        let bytes = self.load(addr, 8)?;
        Ok(i64::from_le_bytes(bytes.try_into().unwrap()))
    }

    fn load_instruction(&self, addr: i64) -> Result<i32, String> {
        let size = 4;
        for segment in &self.segments {
            if segment.in_range(addr, size) && segment.executable {
                let raw = segment.load(addr, size, &mut None);
                return Ok(i32::from_le_bytes(raw.try_into().unwrap()));
            }
        }
        Err(format!("segfault: instruction fetch addr={:#x} size={}", addr, size))
    }

    fn store(&mut self, addr: i64, raw: &[u8]) -> Result<(), String> {
        let size = raw.len() as i64;
        for segment in &mut self.segments {
            if segment.in_range(addr, size) && segment.writeable {
                segment.store(addr, raw, &mut self.effects);
                return Ok(());
            }
        }
        Err(format!("segfault: store addr={:#x} size={}", addr, size))
    }

   fn get(&mut self, reg: usize) -> i64 {
       if reg != 0 && self.effects.is_some() {
           let effects = self.effects.as_mut().unwrap();
           if !effects.reg_reads.iter().any(|r| r.register == reg) {
               effects.reg_reads.push(RegisterValue { 
                   register: reg, 
                   value: self.x[reg] 
               });
           }
       }
       self.x[reg]
   }

   fn set(&mut self, reg: usize, value: i64) {
       // zero register never changes
       if reg != 0 {
           if let Some(effects) = &mut self.effects {
               assert!(effects.reg_write.is_none());
               effects.reg_write = Some((
                   RegisterValue { register: reg, value: self.x[reg] },
                   RegisterValue { register: reg, value }
               ));
           }
           self.x[reg] = value;
       }
   }

   fn set32(&mut self, reg: usize, value: i64) {
       let value = (value as i32) as i64;
       // zero register never changes
       if reg != 0 {
           if let Some(effects) = &mut self.effects {
               assert!(effects.reg_write.is_none());
               effects.reg_write = Some((
                   RegisterValue { register: reg, value: self.x[reg] },
                   RegisterValue { register: reg, value }
               ));
           }
           self.x[reg] = value;
       }
   }

   fn set_pc(&mut self, value: i64) -> Result<(), String> {
       let old_pc = self.pc;
       self.pc = value;
       if self.pc & 1 != 0 {
           return Err(format!("bus error: pc addr={}", self.pc));
       }
       if let Some(effects) = &mut self.effects {
           effects.pc = (old_pc, self.pc);
       }
       Ok(())
   }

    fn address_label(&self, addr: i64) -> Option<String> {
        self.address_symbols
            .get(&addr)
            .cloned()
    }

    fn execute_and_collect_effects(&mut self, instruction: &Rc<Instruction>) -> Effects {
        // trace the effects
        self.effects = Some(Effects::new(instruction));

        // execute the instruction
        if let Err(msg)  = instruction.execute.as_ref()(self) {
            let mut effects = self.effects.take().unwrap();
            effects.error(msg);
            return effects;
        }

        // handle default PC update
        if self.effects.as_ref().unwrap().pc == (0, 0) {
            if let Err(msg) = self.set_pc(self.pc + instruction.length) {
                let mut effects = self.effects.take().unwrap();
                effects.error(msg);
                return effects;
            }
        };

        self.effects.take().unwrap()
    }

    fn apply(&mut self, effect: &Effects, is_forward: bool) {
        let (old_pc, new_pc) = effect.pc;
        self.set_pc(if is_forward { new_pc } else { old_pc })
            .expect("PC should be valid during replay");

        if let Some((old, new)) = &effect.reg_write {
            let write = if is_forward { new } else { old };
            self.set(write.register, write.value);
        }

        if let Some((old, new)) = &effect.mem_write {
            let store = if is_forward { new } else { old };
            self.store(store.address, &store.value)
                .expect("Memory should be valid during replay");
        }

        if let Some(output) = &effect.stdout {
            if is_forward {
                self.stdout.extend(output);
            } else {
                let new_len = self.stdout.len() - output.len();
                self.stdout.truncate(new_len);
            }
        }

        if let Some(input) = &effect.stdin {
            // echo input
            if is_forward {
                self.stdout.extend(input);
            } else {
                let new_len = self.stdout.len() - input.len();
                self.stdout.truncate(new_len);
            }
        }

        if let Some(frame) = effect.function_start {
            if is_forward {
                self.stack_frames.push(frame);
            } else {
                self.stack_frames.pop();
            }
        }

        if let Some(frame) = effect.function_end {
            if is_forward {
                self.stack_frames.pop();
            } else {
                self.stack_frames.push(frame);
            }
        }
    }
}

type ExecuteFn = Box<dyn Fn(&mut Machine) -> Result<(),String>>;

#[allow(unused)]
#[derive(Clone)]
enum Field {
    Opcode (&'static str),
    Reg (usize),
    Imm (i64),
    Indirect (i64, usize),
    Addr (Option<String>, i64),
    LocalBranchTarget (usize, bool, i64),
    Fence,
}

impl Field {
    fn to_string(&self, hex: bool) -> String {
        match self {
            Field::Opcode(inst) => inst.to_string(),
            Field::Reg(reg) => R[*reg].to_string(),
            &Field::Imm(n) => if hex && !(0..=9).contains(&n) { format!("{:#x}", n) } else { format!("{}", n) },
            Field::Indirect(0, reg) => format!("({})", R[*reg]),
            Field::Indirect(imm, reg) => if hex { format!("{:#x}({})", imm, R[*reg]) } else { format!("{}({})", imm, R[*reg]) },
            Field::Addr(Some(label), _) => label.to_string(),
            Field::Addr(None, addr) => if hex { format!("{:#x}", addr) } else { format!("{}", addr) },
            Field::LocalBranchTarget(n, true, _) => format!("{}f", n),
            Field::LocalBranchTarget(n, false, _) => format!("{}b", n),
            Field::Fence => "*".to_string(),
        }
    }
}


#[allow(unused)]
pub struct Instruction {
    address: i64,
    instruction: i32,
    fields: Vec<Field>,
    execute: ExecuteFn,
    static_target: Option<i64>,
    return_target: Option<i64>,
    label: Option<String>,
    length: i64,
    is_target: bool,
}

impl Instruction {
    fn new(address: i64, instruction: i32, fields: Vec<Field>, execute: ExecuteFn) -> Self {
        Self {
            address,
            instruction,
            fields,
            execute,
            static_target: None,
            return_target: None,
            label: None,
            length: 4,
            is_target: false,
        }
    }

    fn branches(&self) -> bool {
        matches!((self.fields.last(), self.static_target, self.return_target),
            (Some(Field::LocalBranchTarget (_,_,_)), Some(_), None))
    }

    fn to_string(&self, hex: bool) -> String {
        let operands = self.fields[1..].iter().map(|elt| elt.to_string(hex)).collect::<Vec<_>>().join(", ");
        let disasm = format!("{:<8}{}", self.fields[0].to_string(hex), operands);
        let addr_part = if let Some(label) = &self.label {
            format!("{}:", label)
        } else {
            String::new()
        };
        format!("{addr_part:<16}{disasm:<48}")
    }
}

struct MemoryValue {
    address: i64,
    value: Vec<u8>,
}

struct RegisterValue {
    register: usize,
    value: i64,
}

pub struct Effects {
    instruction: Rc<Instruction>,

    // pairs are (old_value, new_value)
    pc: (i64, i64),
    reg_reads: Vec<RegisterValue>,
    reg_write: Option<(RegisterValue, RegisterValue)>,
    mem_read: Option<MemoryValue>,
    mem_write: Option<(MemoryValue, MemoryValue)>,
    stdin: Option<Vec<u8>>,
    stdout: Option<Vec<u8>>,
    other_message: Option<String>,
    terminate: bool,
    function_start: Option<i64>,
    function_end: Option<i64>,
}

impl Effects {
    fn new(instruction: &Rc<Instruction>) -> Self {
        Effects {
            instruction: instruction.clone(),
            pc: (0, 0),
            reg_reads: Vec::new(),
            reg_write: None,
            mem_read: None,
            mem_write: None,
            stdin: None,
            stdout: None,
            other_message: None,
            terminate: false,
            function_start: None,
            function_end: None,
        }
    }

    fn error(&mut self, msg: String) {
        self.other_message = Some(msg);
        self.terminate = true;
    }

    fn report(&self, hex_mode: bool) -> Vec<String> {
        let mut parts = Vec::new();
        if let Some((_, RegisterValue { register: rd, value: val })) = self.reg_write {
            if hex_mode {
                parts.push(format!("{} <- {:#x}", R[rd], val));
            } else {
                parts.push(format!("{} <- {}", R[rd], val));
            }
        }
        if self.pc.1 != self.pc.0 + self.instruction.length {
            if hex_mode {
                parts.push(format!("pc <- {:#x}", self.pc.1));
            } else {
                parts.push(format!("pc <- {}", self.pc.1));
            }
        }

        let mut lines = vec![parts.join(", ")];

        if let Some(msg) = &self.other_message {
            lines.push(msg.clone());
        }
        if let Some(stdin) = &self.stdin {
            let msg = String::from_utf8_lossy(stdin).to_string();
            lines.push(format!("{:?}", msg));
        }
        if let Some(stdout) = &self.stdout {
            let msg = String::from_utf8_lossy(stdout).to_string();
            lines.push(format!("{:?}", msg));
        }

        lines
    }
}

fn load_elf(filename: &str) -> Result<Machine, String> {
    let raw = std::fs::read(filename).map_err(|e| format!("{}", e))?;

    // unpack the elf header
    if raw.len() < 0x40 {
        return Err(format!("{filename} is too short"));
    }
if raw[0..4] != *b"\x7fELF" {
        return Err(format!("{filename} does not have ELF magic number"));
    }
    if raw[4] != 2 || raw[5] != 1 || raw[6] != 1 || raw[7] != 0 {
        return Err(format!("{filename} is not a 64-bit, little-endian, version 1, System V ABI ELF file"));
    }

    if u16::from_le_bytes(raw[0x10..0x12].try_into().unwrap()) != 2 ||
        u16::from_le_bytes(raw[0x12..0x14].try_into().unwrap()) != 0xf3 ||
        u32::from_le_bytes(raw[0x14..0x18].try_into().unwrap()) != 1 {
        return Err(format!("{filename} is not an executable, RISC-V, ELF version 1 file"));
    }

    let e_entry = i64::from_le_bytes(raw[0x18..0x20].try_into().unwrap());
    let e_phoff = u64::from_le_bytes(raw[0x20..0x28].try_into().unwrap()) as usize;
    let e_shoff = u64::from_le_bytes(raw[0x28..0x30].try_into().unwrap()) as usize;

    //let e_flags = u32::from_le_bytes(raw[0x30..0x34].try_into().unwrap());
    let e_ehsize = u16::from_le_bytes(raw[0x34..0x36].try_into().unwrap());
    let e_phentsize = u16::from_le_bytes(raw[0x36..0x38].try_into().unwrap()) as usize;
    let e_phnum = u16::from_le_bytes(raw[0x38..0x3a].try_into().unwrap()) as usize;
    let e_shentsize = u16::from_le_bytes(raw[0x3a..0x3c].try_into().unwrap()) as usize;
    let e_shnum = u16::from_le_bytes(raw[0x3c..0x3e].try_into().unwrap()) as usize;
    let e_shstrndx = u16::from_le_bytes(raw[0x3e..0x40].try_into().unwrap()) as usize;

    if e_phoff != 0x40 || e_ehsize != 0x40 || e_phentsize != 0x38 || e_phnum < 1 {
        return Err(format!("{filename} has unexpected header sizes"));
    }

    // get the loadable segments
    let mut chunks = Vec::new();
    for i in 0..e_phnum {
        // unpack the program header
        let start = e_phoff + e_phentsize*i;
        if start + e_phentsize > raw.len() {
            return Err(format!("{filename} program header entry {i} out of range"));
        }
        let header = &raw[start..start+e_phentsize];
        let p_type = u32::from_le_bytes(header[0x00..0x04].try_into().unwrap());
        //let p_flags = u32::from_le_bytes(header[0x04..0x08].try_into().unwrap());
        let p_offset = i64::from_le_bytes(header[0x08..0x10].try_into().unwrap());
        let p_vaddr = i64::from_le_bytes(header[0x10..0x18].try_into().unwrap());
        //let p_paddr = i64::from_le_bytes(header[0x18..0x20].try_into().unwrap());
        let p_filesz = i64::from_le_bytes(header[0x20..0x28].try_into().unwrap());
        //let p_memsz = i64::from_le_bytes(header[0x28..0x30].try_into().unwrap());
        //let p_align = i64::from_le_bytes(header[0x30..0x38].try_into().unwrap());

        if p_type != 1 {
            continue;
        }
        if p_vaddr < 0 {
            return Err(format!("{filename} program segment {i} has negative vaddr"));
        }
        if (p_offset + p_filesz) as usize > raw.len() {
            return Err(format!("{filename} program segment {i} out of range"));
        }
        let chunk = (p_vaddr, raw[p_offset as usize..(p_offset+p_filesz) as usize].to_vec());
        chunks.push(chunk);
    }

    // get the section header strings
    let start = e_shoff + e_shentsize*e_shstrndx;
    if start + e_shentsize > raw.len() {
        return Err(format!("{filename} section header string table entry out of range"));
    }
    let header = &raw[start..start+e_shentsize];
    //let sh_name = u32::from_le_bytes(header[0x00..0x04].try_into().unwrap());
    //let sh_type = u32::from_le_bytes(header[0x04..0x08].try_into().unwrap());
    //let sh_flags = u64::from_le_bytes(header[0x08..0x10].try_into().unwrap());
    //let sh_addr = i64::from_le_bytes(header[0x10..0x18].try_into().unwrap());
    let sh_offset = u64::from_le_bytes(header[0x18..0x20].try_into().unwrap()) as usize;
    let sh_size = u64::from_le_bytes(header[0x20..0x28].try_into().unwrap()) as usize;
    //let sh_link = u32::from_le_bytes(header[0x28..0x2C].try_into().unwrap());
    //let sh_info = u32::from_le_bytes(header[0x2C..0x30].try_into().unwrap());
    //let sh_addralign = i64::from_le_bytes(header[0x30..0x38].try_into().unwrap());
    //let sh_entsize = i64::from_le_bytes(header[0x38..0x40].try_into().unwrap());

    if sh_offset + sh_size > raw.len() {
        return Err(format!("{filename} section header string table out of range"));
    }

    // unpack the strings, keyed by offset
    let mut sh_strs = HashMap::new();
    let sh_str_raw = &raw[sh_offset..sh_offset + sh_size];
    let mut start = 0;
    for (i, &b) in sh_str_raw.iter().enumerate() {
        if b == 0 {
            sh_strs.insert(
                start,
                String::from_utf8_lossy(&sh_str_raw[start..i]).to_string(),
            );
            start = i + 1;
        }
    }

    // read the section headers
    let (mut strs_raw, mut syms_raw) = (Vec::new(), Vec::new());
    let mut segments = Vec::new();

    for i in 0..e_shnum {
       let start = e_shoff + e_shentsize*i;
       if start + e_shentsize > raw.len() {
           return Err(format!("{filename} section header {i} out of range"));
       }

       // unpack the section header
       let header = &raw[start..start+e_shentsize];
       let sh_name = u32::from_le_bytes(header[0x00..0x04].try_into().unwrap()) as usize;
       let sh_type = u32::from_le_bytes(header[0x04..0x08].try_into().unwrap());
       let sh_flags = u64::from_le_bytes(header[0x08..0x10].try_into().unwrap());
       let sh_addr = i64::from_le_bytes(header[0x10..0x18].try_into().unwrap());
       let sh_offset = u64::from_le_bytes(header[0x18..0x20].try_into().unwrap()) as usize;
       let sh_size = u64::from_le_bytes(header[0x20..0x28].try_into().unwrap()) as usize;
       //let sh_link = u32::from_le_bytes(header[0x28..0x2C].try_into().unwrap());
       //let sh_info = u32::from_le_bytes(header[0x2C..0x30].try_into().unwrap());
       //let sh_addralign = u64::from_le_bytes(header[0x30..0x38].try_into().unwrap());
       //let sh_entsize = u64::from_le_bytes(header[0x38..0x40].try_into().unwrap());

       // check for unsupported features
       if sh_type == 0x4 || sh_type == 0x5 || sh_type == 0x6 || sh_type == 0x9 || 
          sh_type == 0xb || sh_type == 0xe || sh_type == 0xf || sh_type == 0x10 || sh_type == 0x11 {
           return Err(format!("{filename} contains unsupported section type {:#x}", sh_type));
       }

       if (sh_type == 1 || sh_type == 8) && (sh_flags & 0x2) != 0 {
           // in-memory section; see if we have loadable data
           let mut init = Vec::new();
           for &(p_vaddr, ref seg_raw) in &chunks {
               if p_vaddr <= sh_addr && sh_addr < p_vaddr + seg_raw.len() as i64 {
                   let start_idx = (sh_addr - p_vaddr) as usize;
                   let end_idx = start_idx + sh_size;
                   init = seg_raw[start_idx..end_idx].to_vec();
               }
           }
           segments.push(Segment::new(
               sh_addr,
               sh_addr + sh_size as i64,
               (sh_flags & 0x1) != 0,
               (sh_flags & 0x4) != 0,
               init,
           ));
       } else if sh_strs.get(&sh_name) == Some(&String::from(".strtab")) && sh_type == 3 {
           if sh_offset + sh_size > raw.len() {
               return Err(format!("{filename} string table out of range"));
           }
           strs_raw = raw[sh_offset..sh_offset + sh_size].to_vec();
       } else if sh_strs.get(&sh_name) == Some(&String::from(".symtab")) && sh_type == 2 {
           if sh_offset + sh_size > raw.len() {
               return Err(format!("{filename} symbol table out of range"));
           }
           syms_raw = raw[sh_offset..sh_offset + sh_size].to_vec();
       }
    }
 
    // make sure we found the string and symbol sections
    if strs_raw.is_empty() {
        return Err(format!("{filename}: no string table found"));
    }
    if syms_raw.is_empty() {
        return Err(format!("{filename}: no symbol table found"));
    }

    // parse the symbol table
    let mut address_symbols = HashMap::new();
    let mut other_symbols = HashMap::new();
    let mut global_pointer = 0;
    const SYMBOL_SIZE: usize = 24;

    for start in (0..syms_raw.len()).step_by(SYMBOL_SIZE) {
       if start + SYMBOL_SIZE > syms_raw.len() {
           return Err(format!("{filename} symbol table entry out of range"));
       }
       let symbol = &syms_raw[start..start+SYMBOL_SIZE];
       let st_name = u32::from_le_bytes(symbol[0x00..0x04].try_into().unwrap()) as usize;
       let st_info = symbol[0x04];
       //let st_other = symbol[0x05];
       let st_shndx = u16::from_le_bytes(symbol[0x06..0x08].try_into().unwrap());
       let st_value = i64::from_le_bytes(symbol[0x08..0x10].try_into().unwrap());
       //let st_size = u64::from_le_bytes(symbol[0x10..0x18].try_into().unwrap());

       // find the name
       let mut end = st_name;
       while end < strs_raw.len() && strs_raw[end] != 0 {
           end += 1;
       }
       if end >= strs_raw.len() {
           return Err(format!("{filename} symbol name out of range"));
       }
       let name = String::from_utf8_lossy(&strs_raw[st_name..end]).to_string();
       
       if name.is_empty() || st_info == 4 {
           // skip section entries and object file names
           continue;
       } else if name == "__global_pointer$" {
           // keep global pointer
           global_pointer = st_value;
           address_symbols.insert(st_value, name);
           continue;
       } else if name.starts_with('$') || name.starts_with("__") {
           // skip internal names
           continue;
       }
       
       // sort into text, data/bss, and other symbols
       if st_shndx > 0 {
           address_symbols.insert(st_value, name);
       } else {
           other_symbols.insert(name, st_value);
       }
    }

    // allocate address space
    Ok(Machine::new(
        segments,
        e_entry,
        global_pointer,
        address_symbols,
        other_symbols))
}

fn get_funct3(inst: i32) -> i32 {
   (inst >> 12) & 0x07
}

fn get_rd(inst: i32) -> usize {
   ((inst >> 7) & 0x1f) as usize
}

fn get_rs1(inst: i32) -> usize {
   ((inst >> 15) & 0x1f) as usize
}

fn get_rs2(inst: i32) -> usize {
   ((inst >> 20) & 0x1f) as usize
}

fn get_imm_i(inst: i32) -> i64 {
   (inst >> 20) as i64
}

fn get_imm_s(inst: i32) -> i64 {
   let mut imm = (inst >> 20) & !0x0000001f;
   imm |= (inst >> 7) & 0x0000001f;
   imm as i64
}

fn get_imm_b(inst: i32) -> i64 {
   let mut imm = (inst >> 20) & !0x00000fff;
   imm |= (inst << 4) & 0x00000800;
   imm |= (inst >> 20) & 0x000007e0;
   imm |= (inst >> 7) & 0x0000001e;
   imm as i64
}

fn get_imm_u(inst: i32) -> i64 {
   (inst & !0x00000fff) as i64
}

fn get_imm_j(inst: i32) -> i64 {
   let mut imm = (inst >> 11) & !0x000fffff;
   imm |= inst & 0x000ff000;
   imm |= (inst >> 9) & 0x00000800;
   imm |= (inst >> 20) & 0x000007e0;
   imm |= (inst >> 20) & 0x0000001e;
   imm as i64
}

fn get_funct7(inst: i32) -> i32 {
   inst >> 25
}

fn i64_to_u64(n: i64) -> u64 {
    u64::from_ne_bytes(n.to_ne_bytes())
}

const R: [&str; 32] = [
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
    "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
    "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
    "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6",
];

const RA: usize = 1;
const SP: usize = 2;

const A_REGS: [usize; 8] = [10, 11, 12, 13, 14, 15, 16, 17];
const T_REGS: [usize; 7] = [5, 6, 7, 28, 29, 30, 31];
const S_REGS: [usize; 12] = [8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27];

fn decode(labels: &Machine, address: i64, inst: i32) -> Result<Instruction, String> {
    // divide the instructions up according the main instruction opcode
    // then call a specialized function for each opcode to do the rest
    let opcode = inst & 0x7f;
    match opcode {
        // lui
        0x37 => Ok(decode_lui(address, inst, get_rd(inst), get_imm_u(inst))),

        // auipc
        0x17 => Ok(decode_auipc(address, inst, get_rd(inst), get_imm_u(inst))),

        // jal
        0x6f => Ok(decode_jal(labels, address, inst, get_rd(inst), get_imm_j(inst))),

        // jalr
        0x67 => decode_jalr(address, inst, get_funct3(inst), get_rd(inst), get_rs1(inst), get_imm_i(inst)),

        // beq, bne, blt, bge, bltu, bgeu
        0x63 => decode_branches(labels, address, inst, get_funct3(inst), get_rs1(inst), get_rs2(inst), get_imm_b(inst)),

        // lb, lh, lw, ld, lbu, lhu, lwu
        0x03 => decode_load(address, inst, get_funct3(inst), get_rd(inst), get_rs1(inst), get_imm_i(inst)),

        // sb, sh, sw, sd
        0x23 => decode_store(address, inst, get_funct3(inst), get_rs1(inst), get_rs2(inst), get_imm_s(inst)),

        // addi, slti, sltiu, xori, ori, andi, slli, srli, srai
        0x13 => decode_alu_imm(labels, address, inst, get_funct3(inst), get_rd(inst), get_rs1(inst), get_imm_i(inst)),

        // addiw, slliw, srliw, sraiw
        0x1b => decode_alu_imm_w(address, inst, get_funct3(inst), get_rd(inst), get_rs1(inst), get_imm_i(inst)),

        // add, sub, sll, slt, sltu, xor, srl, sra, or, and
        // mul, mulh, mulhsu, mulhu, div, divu, rem, remu
        0x33 => decode_alu(address, inst, get_funct3(inst), get_rd(inst), get_rs1(inst), get_rs2(inst), get_funct7(inst)),

        // addw, subw, sllw, srlw, sraw
        // mulw, divw, remw, remuw
        0x3b => decode_alu_w(address, inst, get_funct3(inst), get_rd(inst), get_rs1(inst), get_rs2(inst), get_funct7(inst)),

        // fence
        0x0f => {
            let execute: ExecuteFn = Box::new(move |_m: &mut Machine| {
                Ok(())
            });

            Ok(Instruction::new(address, inst, vec![Field::Opcode("fence"), Field::Fence], execute))
        }

        // ecall, ebreak
        0x73 => {
            if inst == 0x00000073 {
                Ok(decode_ecall(address, inst))
            } else if inst == 0x00100073 {
                // handle ebreak here--exit
                let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                    let effects = m.effects.as_mut().unwrap();
                    effects.error(String::from("ebreak"));
                    Ok(())
                });

                Ok(Instruction::new(address, inst, vec![Field::Opcode("ebreak")], execute))
            } else {
                Err(format!("disassembler found unknown instruction {:#x}", inst))
            }
        }
        _ => Err(format!("disassembler found unknown opcode {:#x}", opcode)),
    }
}

fn decode_lui(address: i64, inst: i32, rd: usize, imm: i64) -> Instruction {
    let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
        m.set(rd, imm);
        Ok(())
    });
    Instruction::new(address, inst, vec![Field::Opcode("lui"), Field::Reg(rd), Field::Imm(imm)], execute)
}

fn decode_auipc(address: i64, inst: i32, rd: usize, imm: i64) -> Instruction {
    let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
        m.set(rd, m.pc + imm);
        Ok(())
    });
    Instruction::new(address, inst, vec![Field::Opcode("auipc"), Field::Reg(rd), Field::Imm(imm)], execute)
}

fn decode_jal(labels: &Machine, address: i64, inst: i32, rd: usize, imm: i64) -> Instruction {
    let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
        m.set(rd, m.pc + 4);
        m.set_pc(m.pc + imm)
    });
    
    let dest = Field::Addr(labels.address_label(address + imm), address+imm);
    let fields = if R[rd] == "zero" {
        vec![Field::Opcode("j"), dest]
    } else if R[rd] == "ra" {
        vec![Field::Opcode("jal"), dest]
    } else {
        vec![Field::Opcode("jal"), Field::Reg(rd), dest]
    };
    
    let mut inst = Instruction::new(address, inst, fields, execute);
    inst.static_target = Some(address + imm);
    inst.return_target = if rd == 0 { None } else { Some(address + 4) };
    inst
}

fn decode_jalr(address: i64, inst: i32, funct3: i32, rd: usize, rs1: usize, imm: i64) -> Result<Instruction, String> {
    if funct3 != 0 {
        return Err(format!("jalr with unknown funct3 value of {}", funct3));
    }

    let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
        // rd and rs1 could be the same, so capture rs1 before setting rd
        let rs1_val = m.get(rs1);
        m.set(rd, m.pc + 4);
        m.set_pc((rs1_val + imm) & !1)
    });

    let fields = match (R[rd], imm, R[rs1]) {
        ("zero", 0, "ra") => vec![Field::Opcode("ret")],
        ("zero", 0, _) => vec![Field::Opcode("jr"), Field::Reg(rs1)],
        ("ra", 0, _) => vec![Field::Opcode("jalr"), Field::Reg(rs1)],
        _ => vec![Field::Opcode("jalr"), Field::Reg(rd), Field::Indirect(imm, rs1)],
    };

    let mut inst = Instruction::new(address, inst, fields, execute);
    if rd != 0 {
        inst.return_target = Some(address + 4);
    }
    Ok(inst)
}

fn decode_branches(labels: &Machine, address: i64, inst: i32, funct3: i32, rs1: usize, rs2: usize, imm: i64)
    -> Result<Instruction, String> {

    let dest = Field::Addr(labels.address_label(address + imm), address+imm);

    let (execute, fields) = match funct3 {
        0 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                if m.get(rs1) == m.get(rs2) {
                    m.set_pc(m.pc + imm)
                } else {
                    Ok(())
                }
            });

            let fields = if R[rs1] == "zero" {
                vec![Field::Opcode("beqz"), Field::Reg(rs2), dest]
            } else if R[rs2] == "zero" {
                vec![Field::Opcode("beqz"), Field::Reg(rs1), dest.clone()]
            } else {
                vec![Field::Opcode("beq"), Field::Reg(rs1), Field::Reg(rs2), dest.clone()]
            };
            (execute, fields)
        },

        1 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                if m.get(rs1) != m.get(rs2) {
                    m.set_pc(m.pc + imm)
                } else {
                    Ok(())
                }
            });

            let fields = if R[rs1] == "zero" {
                vec![Field::Opcode("bnez"), Field::Reg(rs2), dest.clone()]
            } else if R[rs2] == "zero" {
                vec![Field::Opcode("bnez"), Field::Reg(rs1), dest.clone()]
            } else {
                vec![Field::Opcode("bne"), Field::Reg(rs1), Field::Reg(rs2), dest.clone()]
            };
            (execute, fields)
        },

        4 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                if m.get(rs1) < m.get(rs2) {
                    m.set_pc(m.pc + imm)
                } else {
                    Ok(())
                }
            });

            let fields = if R[rs1] == "zero" {
                vec![Field::Opcode("bgtz"), Field::Reg(rs2), dest.clone()]
            } else if R[rs2] == "zero" {
                vec![Field::Opcode("bltz"), Field::Reg(rs1), dest.clone()]
            } else {
                vec![Field::Opcode("blt"), Field::Reg(rs1), Field::Reg(rs2), dest.clone()]
            };
            (execute, fields)
        },

        5 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                if m.get(rs1) >= m.get(rs2) {
                    m.set_pc(m.pc + imm)
                } else {
                    Ok(())
                }
            });

            let fields = if R[rs1] == "zero" {
                vec![Field::Opcode("blez"), Field::Reg(rs2), dest.clone()]
            } else if R[rs2] == "zero" {
                vec![Field::Opcode("bgez"), Field::Reg(rs1), dest.clone()]
            } else {
                vec![Field::Opcode("bge"), Field::Reg(rs1), Field::Reg(rs2), dest.clone()]
            };
            (execute, fields)
        },

        6 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                if i64_to_u64(m.get(rs1)) < i64_to_u64(m.get(rs2)) {
                    m.set_pc(m.pc + imm)
                } else{
                    Ok(())
                }
            });

            let fields = vec![Field::Opcode("bltu"), Field::Reg(rs1), Field::Reg(rs2), dest.clone()];
            (execute, fields)
        },

        7 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                if i64_to_u64(m.get(rs1)) >= i64_to_u64(m.get(rs2)) {
                    m.set_pc(m.pc + imm)
                } else{
                    Ok(())
                }
            });

            let fields = vec![Field::Opcode("bgeu"), Field::Reg(rs1), Field::Reg(rs2), dest.clone()];
            (execute, fields)
        },

        _ => return Err(format!("branch of unknown type {}", funct3))
    };

    let mut inst = Instruction::new(address, inst, fields, execute);
    inst.static_target = Some(address + imm);
    Ok(inst)
}

fn decode_load(address: i64, inst: i32, funct3: i32, rd: usize, rs1: usize, imm: i64) -> Result<Instruction, String> {
    let (execute, fields) = match funct3 {
        0 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let val = m.load_i8(effective_address)?;
                m.set(rd, val);
                Ok(())
            });
            
            let fields = vec![Field::Opcode("lb"), Field::Reg(rd), Field::Indirect(imm, rs1)];
            (execute, fields)
        },
        
        1 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let val = m.load_i16(effective_address)?;
                m.set(rd, val);
                Ok(())
            });
            
            let fields = vec![Field::Opcode("lh"), Field::Reg(rd), Field::Indirect(imm, rs1)];
            (execute, fields)
        },
        
        2 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let val = m.load_i32(effective_address)?;
                m.set(rd, val);
                Ok(())
            });
            
            let fields = vec![Field::Opcode("lw"), Field::Reg(rd), Field::Indirect(imm, rs1)];
            (execute, fields)
        },
        
        3 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let val = m.load_i64(effective_address)?;
                m.set(rd, val);
                Ok(())
            });
            
            let fields = vec![Field::Opcode("ld"), Field::Reg(rd), Field::Indirect(imm, rs1)];
            (execute, fields)
        },
        
        4 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let val = m.load_u8(effective_address)?;
                m.set(rd, val);
                Ok(())
            });
            
            let fields = vec![Field::Opcode("lbu"), Field::Reg(rd), Field::Indirect(imm, rs1)];
            (execute, fields)
        },
        
        5 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let val = m.load_u16(effective_address)?;
                m.set(rd, val);
                Ok(())
            });
            
            let fields = vec![Field::Opcode("lhu"), Field::Reg(rd), Field::Indirect(imm, rs1)];
            (execute, fields)
        },
        
        6 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let val = m.load_u32(effective_address)?;
                m.set(rd, val);
                Ok(())
            });
            
            let fields = vec![Field::Opcode("lwu"), Field::Reg(rd), Field::Indirect(imm, rs1) ];
            (execute, fields)
        },
        
        _ => return Err(format!("load instruction of unknown type {}", funct3))
    };
    
    Ok(Instruction::new(address, inst, fields, execute))
}

fn decode_store(address: i64, inst: i32, funct3: i32, rs1: usize, rs2: usize, imm: i64) -> Result<Instruction, String> {
    let (execute, fields) = match funct3 {
        0 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let raw = ((m.get(rs2) & 0xff) as u8).to_le_bytes();
                m.store(effective_address, &raw)
            });
            
            let fields = vec![Field::Opcode("sb"), Field::Reg(rs2), Field::Indirect(imm, rs1)];
            (execute, fields)
        },

        1 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let raw = ((m.get(rs2) & 0xffff) as u16).to_le_bytes();
                m.store(effective_address, &raw)
            });
            
            let fields = vec![Field::Opcode("sh"), Field::Reg(rs2), Field::Indirect(imm, rs1)];
            (execute, fields)
        },

        2 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let raw = ((m.get(rs2) & 0xffffffff) as u32).to_le_bytes();
                m.store(effective_address, &raw)
            });
            
            let fields = vec![Field::Opcode("sw"), Field::Reg(rs2), Field::Indirect(imm, rs1)];
            (execute, fields)
        },

        3 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let effective_address = m.get(rs1) + imm;
                let raw = m.get(rs2).to_le_bytes();
                m.store(effective_address, &raw)
            });
            
            let fields = vec![Field::Opcode("sd"), Field::Reg(rs2), Field::Indirect(imm, rs1)];
            (execute, fields)
        },

        _ => return Err(format!("store instruction of unknown type {}", funct3))
    };

    Ok(Instruction::new(address, inst, fields, execute))
}

fn decode_alu_imm(labels: &Machine, address: i64, inst: i32, funct3: i32, rd: usize, rs1: usize, imm: i64) -> Result<Instruction, String> {
    let shamt = imm & 0x3f;
    let imm_high = imm >> 6;
    
    match funct3 {
        0 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1).wrapping_add(imm);
                m.set(rd, val);
                Ok(())
            });
            
            let fields = match (R[rd], R[rs1], imm) {
                ("zero", "zero", 0) => vec![Field::Opcode("nop")],
                (_, "zero", _) => vec![Field::Opcode("li"), Field::Reg(rd), Field::Imm(imm)],
                (_, _, 0) => vec![Field::Opcode("mv"), Field::Reg(rd), Field::Reg(rs1)],
                (rd_str, "gp", _) if rd_str != "gp" => vec![Field::Opcode("la"), Field::Reg(rd), Field::Addr(labels.address_label(labels.global_pointer + imm), labels.global_pointer + imm)],
                _ => vec![Field::Opcode("addi"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)]
            };
            Ok(Instruction::new(address, inst, fields, execute))
        },
        
        2 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = if m.get(rs1) < imm { 1 } else { 0 };
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("slti"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)], execute))
        },
        
        3 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = if i64_to_u64(m.get(rs1)) < i64_to_u64(imm) { 1 } else { 0 };
                m.set(rd, val);
                Ok(())
            });
            
            let fields = if imm == 1 {
                vec![Field::Opcode("seqz"), Field::Reg(rd), Field::Reg(rs1)]
            } else {
                vec![Field::Opcode("sltiu"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)]
            };
            Ok(Instruction::new(address, inst, fields, execute))
        },
        
        4 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1) ^ imm;
                m.set(rd, val);
                Ok(())
            });
            
            let fields = if imm == -1 {
                vec![Field::Opcode("not"), Field::Reg(rd), Field::Reg(rs1)]
            } else {
                vec![Field::Opcode("xori"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)]
            };
            Ok(Instruction::new(address, inst, fields, execute))
        },
        
        6 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1) | imm;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("ori"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)], execute))
        },
        
        7 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1) & imm;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("andi"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)], execute))
        },
        
        1 => {
            if imm_high != 0x00 {
                return Err(format!("immediate mode alu instruction of type {} with unknown subtype {}", funct3, imm_high));
            }
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1) << shamt;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("slli"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)], execute))
        },
        
        5 => {
            match imm_high {
                0x00 => {
                    let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                        let val = (i64_to_u64(m.get(rs1)) >> shamt) as i64;
                        m.set(rd, val);
                        Ok(())
                    });
                    Ok(Instruction::new(address, inst, vec![Field::Opcode("srli"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)], execute))
                },
                0x10 => {
                    let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                        let val = m.get(rs1) >> shamt;
                        m.set(rd, val);
                        Ok(())
                    });
                    Ok(Instruction::new(address, inst, vec![Field::Opcode("srai"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)], execute))
                },
                _ => Err(format!("immediate mode alu instruction of type {} with unknown subtype {}", funct3, imm_high))
            }
        },
        
        _ => Err(format!("immediate mode alu instruction of unknown type {}", funct3))
    }
}

fn decode_alu_imm_w(address: i64, inst: i32, funct3: i32, rd: usize, rs1: usize, imm: i64) -> Result<Instruction, String> {
    let shamt = imm & 0x1f;
    let imm_high = imm >> 5;
    
    match funct3 {
        0 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1).wrapping_add(imm);
                m.set32(rd, val);
                Ok(())
            });
            
            let fields = match (R[rd], R[rs1], imm) {
                ("zero", "zero", 0) => vec![Field::Opcode("nop")],
                (_, "zero", _) => vec![Field::Opcode("li"), Field::Reg(rd), Field::Imm(imm)],
                _ => vec![Field::Opcode("addiw"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(imm)]
            };
            Ok(Instruction::new(address, inst, fields, execute))
        },
        
        1 => {
            if imm_high != 0x00 {
                return Err(format!("immediate mode alu w instruction of type {} with unknown subtype {}", funct3, imm_high));
            }
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1) << shamt;
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("slliw"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)], execute))
        },
        
        5 => {
            match imm_high {
                0x00 => {
                    let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                        let val = (m.get(rs1) & 0xffffffff) >> shamt;
                        m.set32(rd, val);
                        Ok(())
                    });
                    Ok(Instruction::new(address, inst, vec![Field::Opcode("srliw"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)], execute))
                },
                0x20 => {
                    let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                        let val = (m.get(rs1) as i32) as i64 >> shamt;
                        m.set32(rd, val);
                        Ok(())
                    });
                    Ok(Instruction::new(address, inst, vec![Field::Opcode("sraiw"), Field::Reg(rd), Field::Reg(rs1), Field::Imm(shamt)], execute))
                },
                _ => Err(format!("immediate mode alu w instruction of type {} with unknown subtype {}", funct3, imm_high))
            }
        },
        
        _ => Err(format!("immediate mode alu w instruction of unknown type {}", funct3))
    }
}

fn decode_alu(address: i64, inst: i32, funct3: i32, rd: usize, rs1: usize, rs2: usize, funct7: i32) -> Result<Instruction, String> {
    let combined = (funct7 << 8) | funct3;
    
    match combined {
        0x0000 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1).wrapping_add(m.get(rs2));
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("add"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x2000 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1).wrapping_sub(m.get(rs2));
                m.set(rd, val);
                Ok(())
            });
            let fields = if R[rs1] == "zero" {
                vec![Field::Opcode("neg"), Field::Reg(rd), Field::Reg(rs2)]
            } else {
                vec![Field::Opcode("sub"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)]
            };
            Ok(Instruction::new(address, inst, fields, execute))
        },
        
        0x0001 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2) & 0x3f;
                let val = m.get(rs1) << rs2_val;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("sll"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0002 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = if m.get(rs1) < m.get(rs2) { 1 } else { 0 };
                m.set(rd, val);
                Ok(())
            });
            let fields = if R[rs2] == "zero" {
                vec![Field::Opcode("sltz"), Field::Reg(rd), Field::Reg(rs1)]
            } else if R[rs1] == "zero" {
                vec![Field::Opcode("sgtz"), Field::Reg(rd), Field::Reg(rs2)]
            } else {
                vec![Field::Opcode("slt"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)]
            };
            Ok(Instruction::new(address, inst, fields, execute))
        },
        
        0x0003 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = if i64_to_u64(m.get(rs1)) < i64_to_u64(m.get(rs2)) { 1 } else { 0 };
                m.set(rd, val);
                Ok(())
            });
            let fields = if R[rs1] == "zero" {
                vec![Field::Opcode("snez"), Field::Reg(rd), Field::Reg(rs2)]
            } else {
                vec![Field::Opcode("sltu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)]
            };
            Ok(Instruction::new(address, inst, fields, execute))
        },
        
        0x0004 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1) ^ m.get(rs2);
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("xor"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0005 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2) & 0x3f;
                let val = (i64_to_u64(m.get(rs1)) >> rs2_val) as i64;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("srl"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x2005 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2) & 0x3f;
                let val = m.get(rs1) >> rs2_val;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("sra"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0006 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1) | m.get(rs2);
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("or"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0007 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1) & m.get(rs2);
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("and"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0100 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1).wrapping_mul(m.get(rs2));
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("mul"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0101 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = ((m.get(rs1) as i128 * m.get(rs2) as i128) >> 64) as i64;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("mulh"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0102 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = ((m.get(rs1) as i128 * i64_to_u64(m.get(rs2)) as i128) >> 64) as i64;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("mulhsu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0103 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = ((i64_to_u64(m.get(rs1)) as u128 * i64_to_u64(m.get(rs2)) as u128) >> 64) as i64;
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("mulhu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0104 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2);
                let val = if rs2_val == 0 {
                    -1
                } else {
                    (m.get(rs1) as i128 / rs2_val as i128) as i64
                };
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("div"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0105 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = i64_to_u64(m.get(rs2));
                let val = if rs2_val == 0 {
                    -1
                } else {
                    (i64_to_u64(m.get(rs1)) / rs2_val) as i64
                };
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("divu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0106 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2);
                let val = if rs2_val == 0 {
                    m.get(rs1)
                } else {
                    (m.get(rs1) as i128 % rs2_val as i128) as i64
                };
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("rem"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0107 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = i64_to_u64(m.get(rs2));
                let val = if rs2_val == 0 {
                    m.get(rs1)
                } else {
                    (i64_to_u64(m.get(rs1)) % rs2_val) as i64
                };
                m.set(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("remu"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        _ => Err(format!("alu instruction of unknown type {} subtype {}", funct3, funct7))
    }
}

fn decode_alu_w(address: i64, inst: i32, funct3: i32, rd: usize, rs1: usize, rs2: usize, funct7: i32) -> Result<Instruction, String> {
    let combined = (funct7 << 8) | funct3;
    
    match combined {
        0x0000 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1).wrapping_add(m.get(rs2));
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("addw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x2000 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1).wrapping_sub(m.get(rs2));
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("subw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0001 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2) & 0x1f;
                let val = m.get(rs1) << rs2_val;
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("sllw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0005 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2) & 0x1f;
                let val = (m.get(rs1) & 0xffffffff) >> rs2_val;
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("srlw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x2005 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2) & 0x1f;
                let val = (m.get(rs1) as i32) as i64 >> rs2_val;
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("sraw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0100 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let val = m.get(rs1).wrapping_mul(m.get(rs2));
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("mulw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0104 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2);
                let val = if rs2_val == 0 {
                    -1
                } else {
                    m.get(rs1) / rs2_val
                };
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("divw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0105 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = i64_to_u64(m.get(rs2));
                let val = if rs2_val == 0 {
                    -1
                } else {
                    (i64_to_u64(m.get(rs1)) / rs2_val) as i64
                };
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("divuw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0106 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = m.get(rs2);
                let val = if rs2_val == 0 {
                    m.get(rs1)
                } else {
                    m.get(rs1) % rs2_val
                };
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("remw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        0x0107 => {
            let execute: ExecuteFn = Box::new(move |m: &mut Machine| {
                let rs2_val = i64_to_u64(m.get(rs2));
                let val = if rs2_val == 0 {
                    m.get(rs1)
                } else {
                    (i64_to_u64(m.get(rs1)) % rs2_val) as i64
                };
                m.set32(rd, val);
                Ok(())
            });
            Ok(Instruction::new(address, inst, vec![Field::Opcode("remuw"), Field::Reg(rd), Field::Reg(rs1), Field::Reg(rs2)], execute))
        },
        
        _ => Err(format!("alu w instruction of unknown type {} subtype {}", funct3, funct7))
    }
}

fn decode_ecall(address: i64, inst: i32) -> Instruction {
    let execute_ecall: ExecuteFn = Box::new(|m: &mut Machine| {
        match m.get(17) {
            63 => {
                // read system call
                m.effects.as_mut().unwrap().other_message =
                    Some(format!("read({}, {:#x}, {})", m.get(10), m.get(11), m.get(12)));
                let fd = m.get(10);
                let buf_addr = m.get(11);
                let count = m.get(12);
                
                if fd != 0 {
                    return Err(format!("read syscall: only stdin (fd 0) supported, not {fd}"));
                }
                if count < 0 {
                    return Err(format!("read syscall: invalid buffer size: {count}"));
                }

                // make a buffer and read from stdin
                let mut read_buffer = vec![0; count as usize];
                let mut handle = io::stdin().lock();
                match handle.read(&mut read_buffer) {
                    Ok(n) => read_buffer.truncate(n),
                    Err(e) => return Err(format!("read syscall error: {}", e)),
                }

                m.store(buf_addr, &read_buffer)?;
                m.set(10, read_buffer.len() as i64);
                m.stdin.extend_from_slice(&read_buffer);
                m.effects.as_mut().unwrap().stdin = Some(read_buffer);
                Ok(())
            },

            64 => {
                // write system call
                m.effects.as_mut().unwrap().other_message =
                    Some(format!("write({}, {:#x}, {})", m.get(10), m.get(11), m.get(12)));
                let fd = m.get(10);
                let buf_addr = m.get(11);
                let count = m.get(12);

                if fd != 1 {
                    return Err(format!("write syscall: only stdout (fd 1) supported, not {fd}"));
                }
                if count < 0 {
                    return Err(format!("write syscall: invalid buffer size: {count}"));
                }

                let write_buffer = m.load(buf_addr, count)?;
                m.set(10, write_buffer.len() as i64);
                m.stdout.extend_from_slice(&write_buffer);
                m.effects.as_mut().unwrap().stdout = Some(write_buffer);
                Ok(())
            },

            93 => {
                // exit system call
                let status = m.get(10) & 0xff;
                let effects = m.effects.as_mut().unwrap();
                effects.error(format!("exit({})", status));
                Ok(())
            },

            syscall => Err(format!("unsupported syscall {syscall}"))
        }
    });

    Instruction::new(address, inst, vec![Field::Opcode("ecall")], execute_ecall)
}

fn merge_instruction_pairs(m: &Machine, instructions: &mut Vec<Instruction>) -> Result<(), String> {
    let mut i = 0;
    while i + 1 < instructions.len() {
        let instruction1 = &instructions[i];
        let instruction2 = &instructions[i + 1];
        if instruction2.is_target {
            continue;
        }

        let fields1 = &instruction1.fields;
        let fields2 = &instruction2.fields;

        if fields1.len() == 3 && fields2.len() == 4 {
            match (fields1.as_slice(), fields2.as_slice()) {
                ([Field::Opcode("auipc"), Field::Reg(rd1), Field::Imm(imm1)], [Field::Opcode("addi"), Field::Reg(rd2), Field::Reg(rs2), Field::Imm(imm2)]) if rd1 == rd2 && rd2 == rs2 => {
                    let addr = instruction1.address + imm1 + imm2;
                    let sym = Field::Addr(m.address_label(addr), addr);
                    let rd = *rd1;

                    let execute: ExecuteFn = {
                        let length = instruction1.length + instruction2.length;
                        Box::new(move |m: &mut Machine| {
                            m.set(rd, addr);
                            m.set_pc(m.pc + length)
                        })
                    };

                    let length = instruction1.length + instruction2.length;
                    let mut new_instruction = Instruction::new(instruction1.address, -1, vec![Field::Opcode("la"), Field::Reg(rd), sym], execute);
                    new_instruction.length = length;

                    instructions.remove(i);
                    instructions.remove(i);
                    instructions.insert(i, new_instruction);
                }

                ([Field::Opcode("auipc"), Field::Reg(rd1), Field::Imm(imm1)], [Field::Opcode("jalr"), Field::Reg(rd2), Field::Indirect(imm2, rs2)]) if rd1 == rd2 && rd1 == rs2 && *rd1 == RA => {
                    let addr = instruction1.address + imm1 + imm2;
                    let sym = Field::Addr(m.address_label(addr), addr);
                    let rd = *rd1;

                    let execute: ExecuteFn = {
                        let length = instruction1.length + instruction2.length;
                        Box::new(move |m: &mut Machine| {
                            m.set(rd, m.pc + length);
                            m.set_pc(addr)
                        })
                    };

                    let length = instruction1.length + instruction2.length;
                    let mut new_instruction = Instruction::new(instruction1.address, -1, vec![Field::Opcode("call"), sym], execute);
                    new_instruction.length = length;

                    instructions.remove(i);
                    instructions.remove(i);
                    instructions.insert(i, new_instruction);
                }

                _ => {}
            }
        }
        i += 1;
    }
    Ok(())
}

fn add_labels(m: &Machine, instructions: &mut [Instruction], addresses: &HashMap<i64,usize>) -> Result<(), String> {
   // add labels from symbol table and note branch targets
   for i in 0..instructions.len() {
       let addr = instructions[i].address;
       if m.address_symbols.contains_key(&addr) {
           instructions[i].label = Some(m.address_symbols[&addr].clone());
       }
       if let Some(target) = instructions[i].static_target {
           let Some(&target_i) = addresses.get(&target) else {
               return Err(format!("branch from {:#x} to unknown target address {:#x}", addr, target));
           };
           instructions[target_i].is_target = true;
       }
   }

   // add numbered local labels
   let mut next_label = 1;
   let mut next_fn = HashMap::new();
   let mut prev = None;

   for instruction in instructions.iter_mut() {
       if instruction.label.is_none() && instruction.is_target {
           instruction.label = Some(next_label.to_string());
           next_label += 1;
       } else if instruction.label.is_some() {
           next_label = 1;
           if let Some(prev) = prev {
               next_fn.insert(prev, instruction.address);
           }
           prev = Some(instruction.address);
       }
   }
   if let Some(prev) = prev {
       next_fn.insert(prev, m.text_end);
   }

   // update branches to reference local labels
   let mut fn_start = 0;
   let mut fn_end = 0;

   for i in 0..instructions.len() {
       if let Some(ref label) = instructions[i].label {
           if !is_digit(label) {
               fn_start = instructions[i].address;
               fn_end = next_fn[&fn_start];
           }
       }

       let Some(&addr) = (match instructions[i].fields.as_slice() {
           [Field::Opcode("j"), Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("beqz"), _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("beq"), _, _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("bnez"), _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("bne"), _, _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("bgtz"), _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("bltz"), _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("blt"), _, _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("blez"), _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("bgez"), _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("bge"), _, _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("bltu"), _, _, Field::Addr(None, dest)] => Some(dest),
           [Field::Opcode("bgeu"), _, _, Field::Addr(None, dest)] => Some(dest),
           _ => None,
       }) else {
           continue;
       };

       // ignore branches outside the current function
       if addr < fn_start || addr >= fn_end {
           continue;
       }

       let Some(&target_i) = addresses.get(&addr) else {
           return Err(format!("branch from {:#x} found to unknown address {:#x}", instructions[i].address, addr));
       };
       if let Some(ref target_label) = instructions[target_i].label {
           if is_digit(target_label) {
               let label: usize = target_label.parse().unwrap();
               let is_forward = addr > instructions[i].address;
               let last = instructions[i].fields.len() - 1;
               instructions[i].fields[last] = Field::LocalBranchTarget (label, is_forward, addr);
           }
       }
   }
   Ok(())
}

fn trace(m: &mut Machine, instructions: &[Rc<Instruction>], addresses: &HashMap<i64,usize>, lint: bool, max_steps: usize, mode: &str) -> Vec<Effects> {
    let mut linter = Linter::new(m.x[2]);
    let mut sequence: Vec<Effects> = Vec::new();
    let mut i = 0;
    let echo_in = ["run", "debug"].contains(&mode) && !io::stdin().is_tty();

    for steps in 1..=max_steps {
        if i >= instructions.len() || instructions[i].address != m.pc {
            let Some(&new_i) = addresses.get(&m.pc) else {
                if let Some(effects) = sequence.last_mut() {
                    effects.error("next instruction not found".to_string());
                }
                break;
            };
            i = new_i;
        }

        // execute the instruction
        let instruction = &instructions[i];
        let mut effects = m.execute_and_collect_effects(instruction);
        i += 1;

        // echo the output?
        if !effects.terminate && ["run", "debug"].contains(&mode) {
            if let Some(output) = &effects.stdout {
                let mut handle = io::stdout().lock();
                if let Err(e) = handle.write(output) {
                    effects.error(format!("error echoing stdout: {}", e));
                }
            }
        }

        // echo the input?
        if !effects.terminate && echo_in {
            if let Some(input) = &effects.stdin {
                let mut handle = io::stdout().lock();
                if let Err(e) = handle.write(input) {
                    effects.error(format!("error echoing stdin: {}", e));
                }
            }
        }

        if !effects.terminate && lint {
            if let Err(msg) = linter.check_instruction(m, instruction, &mut effects) {
                effects.error(msg);
            }
        }

        if mode == "trace" {
            print!("{}", effects.instruction.to_string(false));
            for line in effects.report(false) {
                println!("{:16}{}", "", line);
            }
        }

        let terminate = effects.terminate;
        sequence.push(effects);
        if terminate {
            break;
        }

        if steps == max_steps {
            if let Some(last) = sequence.last_mut() {
                if last.other_message.is_none() {
                    last.error(format!("stopped after {} steps", max_steps));
                }
            }
        } else if mode != "debug" {
            sequence.clear();
        }
    }

    sequence
}

struct FunctionRegisters {
    at_entry: [Option<usize>; 32],
    valid: [bool; 32],
    at_entry_sp: i64,
}

struct ValueInMemory {
    n: usize,
    size: usize,
}

struct Linter {
    memory: HashMap<i64, ValueInMemory>,

    stack: Vec<FunctionRegisters>,
    at_entry: [Option<usize>; 32],
    at_entry_sp: i64,

    registers: [Option<usize>; 32],
    valid: [bool; 32],
    next_n: usize,
}

impl Linter {
    fn new(at_entry_sp: i64) -> Self {
        let mut at_entry = [None; 32];
        for (n, elt) in at_entry.iter_mut().enumerate() {
            *elt = Some(n);
        };
        let mut valid = [false; 32];
        valid[0] = true;
        valid[2] = true;
        let registers = at_entry;

        Self {
            memory: HashMap::new(),
            stack: Vec::new(),
            at_entry,
            at_entry_sp,
            registers,
            valid,
            next_n: 32,
        }
    }

    fn new_n(&mut self) -> usize {
        self.next_n += 1;
        self.next_n - 1
    }

    fn check_instruction(&mut self, m: &Machine, instruction: &Rc<Instruction>, effects: &mut Effects) -> Result<(), String> {
        // start with checks applicable to all instructions
        // this allows us to make basic assumptions later

        // first check that all input registers are valid values
        for read in &effects.reg_reads {
            let x = read.register;
            if !self.valid[x] || self.registers[x].is_none() {
                return Err(format!("{} is uninitialized", R[x]));
            }
        }

        // next record register write
        if let Some((_, write)) = &effects.reg_write {
            let x = write.register;
            self.valid[x] = true;

            // mv clones a value
            if let Field::Opcode("mv") = instruction.fields[0] {
                assert!(effects.reg_reads.len() == 1);
                self.registers[x] = self.registers[effects.reg_reads[0].register];
            } else {
                self.registers[x] = Some(self.new_n());
            }

            // sp must be aligned on 16-byte address
            if x == 2 && m.x[x] & 0xf != 0 {
                return Err("sp must be a multiple of 16".to_string());
            }
        }

        // special per-instruction cases
        match instruction.fields[0] {
            Field::Opcode("call" | "jal" | "jalr") => {
                // must use ra for return address
                let Some((_, RegisterValue { register: 1, .. })) = effects.reg_write else {
                    return Err(format!("{} did not use ra for return address", instruction.fields[0].to_string(false)));
                };

                // must call named function
                let (_, target_pc) = effects.pc;
                if !m.address_symbols.contains_key(&target_pc) {
                    return Err(format!("{} to unlabeled address", instruction.fields[0].to_string(false)));
                }
                let name = &m.address_symbols[&target_pc];

                // push caller register context
                self.stack.push(FunctionRegisters {
                    at_entry: self.at_entry,
                    valid: self.valid,
                    at_entry_sp: self.at_entry_sp,
                });

                // update context for callee
                self.at_entry_sp = m.x[SP];

                // capture the stack start in the Effect for the tui
                effects.function_start = Some(m.x[SP]);

                // invalidate t registers
                for &x in &T_REGS {
                    self.registers[x] = None;
                    self.valid[x] = false;
                }

                let mut arg_count = 8;
                let args_sym = format!("{}_args", name);
                if let Some(&count) = m.other_symbols.get(&args_sym) {
                    // we have an argument count
                    assert!((0..8).contains(&count));
                    arg_count = count as usize;

                    // make sure func args are all valid values
                    for &x in A_REGS.iter().take(arg_count) {
                        if !self.valid[x] {
                            return Err(format!("argument in {} is uninitialized", R[x]));
                        }
                    }
                    for &x in A_REGS.iter().skip(arg_count) {
                        self.registers[x] = None;
                        self.valid[x] = false;
                    }
                } else {
                    // no argument count, so assume all a registers are args
                    for &x in A_REGS.iter().take(arg_count) {
                        self.valid[x] = self.registers[x].is_some();
                    }
                }

                // make sure all s registers have a number
                for &x in &S_REGS {
                    if self.registers[x].is_none() {
                        self.registers[x] = Some(self.new_n());
                    }
                    self.valid[x] = true;
                }

                // record the registers at function entry time
                self.at_entry = self.registers;
            },

            Field::Opcode("ret") => {
                // ra, gp, and tp must match what they were at call time
                for x in [1, 3, 4] {
                    if self.registers[x] != self.at_entry[x] {
                        return Err(format!("{} is not same value as when function called", R[x]));
                    }
                }

                // s registers must be same as at call time
                for &x in &S_REGS {
                    if self.registers[x] != self.at_entry[x] {
                        return Err(format!("{} is not same value as when function called", R[x]));
                    }
                }

                // sp must have the same address, but not necessarily the same value number
                if m.x[2] != self.at_entry_sp {
                    return Err("sp is not same value as when function called".to_string());
                }

                // record sp at function exit in Effects for the tui
                effects.function_end = Some(m.x[2]);

                // pop previous function context
                if let Some(FunctionRegisters { at_entry, valid, at_entry_sp }) = self.stack.pop() {
                    self.at_entry = at_entry;
                    self.valid = valid;
                    self.at_entry_sp = at_entry_sp;
                } else {
                    return Err("ret with no stack frame to return to".to_string());
                }

                // invalidate t and a1+ registers
                for &x in &T_REGS {
                    self.registers[x] = None;
                    self.valid[x] = false;
                }
                for &x in A_REGS.iter().skip(1) {
                    self.registers[x] = None;
                    self.valid[x] = false;
                }
            },

            Field::Opcode("sb" | "sh" | "sw" | "sd") => {
                let Some((_, write)) = &effects.mem_write else {
                    return Err("store instruction with no memory write".to_string());
                };

                let addr = write.address;
                let size = write.value.len();

                // insist on aligned writes
                // partial register writes count as new values
                // since re-reading them does not restore a full register value
                let Field::Reg(rs2) = instruction.fields[1] else {
                    unreachable!()
                };
                let (alignment, n) = match instruction.fields[0] {
                    Field::Opcode("sb") => (0, self.new_n()),
                    Field::Opcode("sh") => (1, self.new_n()),
                    Field::Opcode("sw") => (3, self.new_n()),
                    Field::Opcode("sd") => (7, self.registers[rs2].unwrap()),
                    _ => unreachable!()
                };

                if addr & alignment != 0 {
                    return Err("unaligned write to memory".to_string());
                }

                // record the memory write
                for address in addr..addr+size as i64 {
                    self.memory.insert(address, ValueInMemory {
                        n,
                        size,
                    });
                }
            },

            Field::Opcode("lb" | "lh" | "lw" | "ld" | "lbu" | "lhu" | "lwu") => {
                let Some(read) = &effects.mem_read else {
                    return Err("load instruction with no memory read".to_string());
                };

                let addr = read.address;
                let size = read.value.len();

                // insist on aligned reads
                // partial register reads count as new values
                // since they do not restore a full register value
                let Field::Reg(rd) = instruction.fields[1] else {
                    unreachable!()
                };
                let alignment = match instruction.fields[0] {
                    Field::Opcode("lb" | "lbu") => 0,
                    Field::Opcode("lh" | "lhu") => 1,
                    Field::Opcode("lw" | "lwu") => 3,
                    Field::Opcode("ld") => 7,
                    _ => unreachable!()
                };
                if addr & alignment != 0 {
                    return Err("unaligned read from memory".to_string());
                }

                // we accept two kinds of reads:
                // 1. a value that has not been written (recorded as a new number)
                // 2. a single value that is the same size as when written
                let n = if let Some(mem_val) = self.memory.get(&addr) {
                    let n = mem_val.n;

                    for address in addr..addr+size as i64 {
                        match self.memory.get(&address) {
                            None => return Err("reading data that was only partially written".to_string()),
                            Some(ValueInMemory { n: mem_n, size: mem_size }) => {
                                if *mem_n != n {
                                    return Err("reading data from multiple writes".to_string());
                                }
                                if *mem_size != size {
                                    return Err("reading data with different size than when written".to_string());
                                }
                            }
                        }
                    }
                    n
                } else {
                    // record this value in memory as we verify that no bytes
                    // already have a value number
                    let n = self.new_n();
                    for address in addr..addr+size as i64 {
                        if self.memory.contains_key(&address) {
                            return Err("reading data that is only partially from a previous write".to_string());
                        }
                        self.memory.insert(address, ValueInMemory { n, size });
                    }
                    n

                };

                self.registers[rd] = Some(n);
            },

            Field::Opcode("ecall") => {
                // write syscall
                if let Some(read) = &effects.mem_read {
                    let addr = read.address;
                    let size = read.value.len();

                    // only allow byte values from memory
                    for address in addr..addr+size as i64 {
                        if let Some(val) = self.memory.get(&address) {
                            if val.size != 1 {
                                return Err("write syscall on non-byte data".to_string());
                            }
                        }
                    }
                }

                // read syscall
                if let Some((_, write)) = &effects.mem_write {
                    let addr = write.address;
                    let size = write.value.len();

                    for address in addr..addr+size as i64 {
                        // do not allow overwrite of non-byte data
                        if let Some(val) = self.memory.get(&address) {
                            if val.size != 1 {
                                return Err("read syscall overwriting non-byte data".to_string());
                            }
                        }

                        // record data as individual bytes
                        let n = self.new_n();
                        self.memory.insert(address, ValueInMemory { n, size: 1 });
                    }
                }
            },

            _ => {}
        }

        Ok(())
    }
}

fn is_digit(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_digit())
}

fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();

    let mut mode = String::from("debug");
    let mut executable = String::from("a.out");
    let mut lint = String::from("true");

    let mut usage = false;
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-m" | "--mode" => {
                i += 1;
                if i < args.len() {
                    mode = args[i].clone();
                    if !["run", "trace", "dasm", "debug"].contains(&mode.as_str()) {
                         eprintln!("invalid mode");
                         usage = true;
                    }
                } else {
                    eprintln!("missing argument for mode");
                    usage = true;
                }
            }
            "-e" | "--executable" => {
                i += 1;
                if i < args.len() {
                    executable = args[i].clone();
                } else {
                    eprintln!("missing argument for executable");
                    usage = true;
                }
            }
            "-l" | "--lint" => {
                i += 1;
                if i < args.len() {
                    lint = args[i].clone();
                    if !["true", "false"].contains(&lint.as_str()) {
                        eprintln!("invalid value for lint");
                        usage = true;
                    }
                } else {
                    eprintln!("missing argument for lint");
                    usage = true;
                }
            }
            "-h" | "--help" => usage = true,
            _ => usage = true,
        }
        i += 1;
    }
    if usage {
        eprintln!("Usage: rv64sim [options]");
        eprintln!();
        eprintln!("Options:");
        eprintln!("  -e, --executable <path>            Path of executable to run (default a.out)");
        eprintln!("  -l, --list <true|false>            Apply strict ABI and other checks (default true)");
        eprintln!("  -m, --mode <run|trace|dasm|debug>  Simulator Mode (default run)");
        eprintln!("  -h, --help                         Show this help");
        std::process::exit(1);
    }

    // load the program from disk and form the
    // simulated address space and cpu
    let mut m = load_elf(&executable)?;

    // disassemble the entire text segment
    let mut instructions = Vec::new();
    for pc in (m.text_start..m.text_end).step_by(4) {
        let inst = m.load_instruction(pc)?;
        let instruction = decode(&m, pc, inst)?;
        instructions.push(instruction);
    }
    merge_instruction_pairs(&m, &mut instructions)?;
    let mut addresses = HashMap::new();
    for (index, instruction) in instructions.iter().enumerate() {
        addresses.insert(instruction.address, index);
    }
    add_labels(&m, &mut instructions, &addresses)?;

    if mode == "dasm" {
        for instruction in &instructions {
            println!("{}", instruction.to_string(false));
        }
        return Ok(());
    }

    // convert to Rc<Instruction> so Effects can reference entries
    let instructions: Vec<Rc<Instruction>> = instructions.into_iter()
        .map(Rc::new)
        .collect();

    // trace the entire execution
    // for run mode, have pre_trace echo output as it goes
    // so inputs and outputs are correctly interleved
    let sequence = trace(&mut m, &instructions, &addresses, lint == "true", 100000000, &mode);

    // debug
    if mode == "debug" {
        m.reset();
        m.set_most_recent_memory(&sequence, 0);
        let mut tui = Tui::new(m, instructions, addresses, sequence)?;
        tui.main_loop()?;
        return Ok(());
    }

    // should have ended with exit(0)
    if let Some(effects) = sequence.last() {
        if let (Field::Opcode("ecall"), Some(msg)) = (&effects.instruction.fields[0], &effects.other_message) {
            if msg.starts_with("exit(") && msg.ends_with(")") {
                let n: i32 = msg[5..msg.len()-1].parse().unwrap();
                std::process::exit(n);
            }
        }

        if let Some(msg) = &effects.other_message {
            eprintln!("{}", msg);
            std::process::exit(1);
        }
    }
    eprintln!("program ended unexpectedly");
    std::process::exit(1);
}
