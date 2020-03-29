use std::fs;
use std::collections::HashMap;

use crate::bus::Bus;
use crate::disassembler::Disassembler;

enum Mode {
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Accumulator,
    Immediate,
    Implied,
    IndexedIndirect,
    Indirect,
    IndirectIndexed,
    Relative,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
}

enum Flag {
    CarryFlag,
    ZeroFlag,
    InterruptDisable,
    DecimalMode,
    BreakCommand,
    Unused,
    OverflowFlag,
    NegativeFlag,
}

pub struct DebugState {
    pub pc: u16,
    pub sp: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: u8,
    pub clock: u32,
    pub instruction: Vec<u8>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Instruction {
    pub label: String,
    pub mode: String,
}

impl Instruction {
    pub fn new(label: &str, mode: &str) -> Instruction {
        Instruction {
            label: label.to_string(),
            mode: mode.to_string(),
        }
    }
}

pub struct InstructionLine(pub u16, pub String);

pub struct Cpu {
    pub bus: Bus,
    pub pc: u16,
    sp: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: u8,
    clock: u32,
    debug: bool,
    disassembler: Option<Disassembler>,
    debug_state: Option<DebugState>,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            bus: Bus::new(),
            pc: 0,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            p: 0,
            clock: 0,
            debug: false,
            disassembler: None,
            debug_state: None,
        }
    }

    pub fn load(&mut self, path: &str) {
        let buffer = fs::read(path).expect("Could not read ROM.");

        self.bus.load(&buffer[0x0010..0x4000]);
    }

    pub fn pc(&mut self, pc: u16) {
        self.pc = pc;
    }

    pub fn sp(&mut self, sp: u8) {
        self.sp = sp;
    }

    pub fn p(&mut self, p: u8) {
        self.p = p;
    }

    pub fn clock(&mut self, clock: u32) {
        self.clock = clock;
    }

    pub fn reset(&mut self) {
        let lo = self.bus.read(0xfffc, false) as u16;
        let hi = self.bus.read(0xfffd, false) as u16;
        self.pc = (hi << 8) | lo;
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xfd;
        self.p = 0x20;
        self.clock = 7;
    }

    pub fn debug(&mut self) {
        self.debug = true;
        self.disassembler = Some(Disassembler::new());
    }

    pub fn debug_state(&self) -> Option<&DebugState> {
        self.debug_state.as_ref()
    }
}

impl Cpu {
    pub fn step(&mut self) -> u32 {
        let opcode = self.read_opcode();

        let debug_state = (self.pc - 1, self.sp, self.a, self.x, self.y, self.p, self.clock);

        match opcode {
            0x18 => { self.clc(Mode::Implied); self.tick(2); },
            0xd8 => { self.cld(Mode::Implied); self.tick(2); },
            0x58 => { self.cli(Mode::Implied); self.tick(2); },
            0xb8 => { self.clv(Mode::Implied); self.tick(2); },

            0x9a => { self.txs(Mode::Implied); self.tick(2); },

            0xf0 => { self.beq(Mode::Relative); self.tick(2); },
            0x90 => { self.bcc(Mode::Relative); self.tick(2); },
            0xb0 => { self.bcs(Mode::Relative); self.tick(2); },
            0xd0 => { self.bne(Mode::Relative); self.tick(2); },
            0x30 => { self.bmi(Mode::Relative); self.tick(2); },
            0x10 => { self.bpl(Mode::Relative); self.tick(2); },
            0x50 => { self.bvc(Mode::Relative); self.tick(2); },
            0x70 => { self.bvs(Mode::Relative); self.tick(2); },

            0xa8 => { self.tay(Mode::Implied); self.tick(2); },
            0x98 => { self.tya(Mode::Implied); self.tick(2); },
            0xaa => { self.tax(Mode::Implied); self.tick(2); },
            0x8a => { self.txa(Mode::Implied); self.tick(2); },
            0xba => { self.tsx(Mode::Implied); self.tick(2); },

            0x48 => { self.pha(Mode::Implied); self.tick(3); },
            0x08 => { self.php(Mode::Implied); self.tick(3); },
            0x68 => { self.pla(Mode::Implied); self.tick(4); },
            0x28 => { self.plp(Mode::Implied); self.tick(4); },

            0xca => { self.dex(Mode::Implied); self.tick(2); },
            0x88 => { self.dey(Mode::Implied); self.tick(2); },
            0xe8 => { self.inx(Mode::Implied); self.tick(2); },
            0xc8 => { self.iny(Mode::Implied); self.tick(2); },

            0x00 => { self.brk(Mode::Implied); self.tick(7); },
            0x40 => { self.rti(Mode::Implied); self.tick(6); },

            0x38 => { self.sec(Mode::Implied); self.tick(2); },
            0x78 => { self.sei(Mode::Implied); self.tick(2); },
            0xf8 => { self.sed(Mode::Implied); self.tick(2); },

            // NOP
            0x80 => { self.nop(Mode::Immediate); self.tick(2); },
            0x0c => { self.nop(Mode::Absolute); self.tick(4) },
            0x1c | 0x3c | 0x5c | 0x7c | 0xdc | 0xfc => { self.nop(Mode::AbsoluteX); self.tick(4); },
            0x04 | 0x44 | 0x64 => { self.nop(Mode::ZeroPage); self.tick(3); },
            0x14 | 0x34 | 0x54 | 0x74 | 0xd4 | 0xf4 => { self.nop(Mode::ZeroPageX); self.tick(4); },
            0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa | 0xea => { self.nop(Mode::Implied); self.tick(2); },

            // LAX
            0xa3 => { self.lax(Mode::IndexedIndirect); self.tick(6); },
            0xa7 => { self.lax(Mode::ZeroPage); self.tick(3); },
            0xaf => { self.lax(Mode::Absolute); self.tick(4); },
            0xb3 => { self.lax(Mode::IndirectIndexed); self.tick(5); },
            0xb7 => { self.lax(Mode::ZeroPageY); self.tick(4); },
            0xbf => { self.lax(Mode::AbsoluteY); self.tick(4); },

            // SAX
            0x83 => { self.sax(Mode::IndexedIndirect); self.tick(6); },
            0x87 => { self.sax(Mode::ZeroPage); self.tick(3); },
            0x8f => { self.sax(Mode::Absolute); self.tick(4); },
            0x97 => { self.sax(Mode::ZeroPageY); self.tick(4); },

            // DCP
            0xc3 => { self.dcp(Mode::IndexedIndirect); self.tick(8); },
            0xc7 => { self.dcp(Mode::ZeroPage); self.tick(5); },
            0xcf => { self.dcp(Mode::Absolute); self.tick(6); },
            0xd3 => { self.dcp(Mode::IndirectIndexed); self.tick(8); },
            0xd7 => { self.dcp(Mode::ZeroPageX); self.tick(6); },
            0xdb => { self.dcp(Mode::AbsoluteY); self.tick(7); },
            0xdf => { self.dcp(Mode::AbsoluteX); self.tick(7); },

            // ISC
            0xe3 => { self.isc(Mode::IndexedIndirect); self.tick(8); },
            0xe7 => { self.isc(Mode::ZeroPage); self.tick(5); },
            0xef => { self.isc(Mode::Absolute); self.tick(6); },
            0xf3 => { self.isc(Mode::IndirectIndexed); self.tick(8); },
            0xf7 => { self.isc(Mode::ZeroPageX); self.tick(6); },
            0xfb => { self.isc(Mode::AbsoluteY); self.tick(7); },
            0xff => { self.isc(Mode::AbsoluteX); self.tick(7); },

            // SLO
            0x03 => { self.slo(Mode::IndexedIndirect); self.tick(8); },
            0x07 => { self.slo(Mode::ZeroPage); self.tick(5); },
            0x0f => { self.slo(Mode::Absolute); self.tick(6); },
            0x13 => { self.slo(Mode::IndirectIndexed); self.tick(8); },
            0x17 => { self.slo(Mode::ZeroPageX); self.tick(6); },
            0x1b => { self.slo(Mode::AbsoluteY); self.tick(7); },
            0x1f => { self.slo(Mode::AbsoluteX); self.tick(7); },

            // RLA
            0x23 => { self.rla(Mode::IndexedIndirect); self.tick(8); },
            0x27 => { self.rla(Mode::ZeroPage); self.tick(5); },
            0x2f => { self.rla(Mode::Absolute); self.tick(6); },
            0x33 => { self.rla(Mode::IndirectIndexed); self.tick(8); },
            0x37 => { self.rla(Mode::ZeroPageX); self.tick(6); },
            0x3b => { self.rla(Mode::AbsoluteY); self.tick(7); },
            0x3f => { self.rla(Mode::AbsoluteX); self.tick(7); },

            // SRE
            0x43 => { self.sre(Mode::IndexedIndirect); self.tick(8); },
            0x47 => { self.sre(Mode::ZeroPage); self.tick(5); },
            0x4f => { self.sre(Mode::Absolute); self.tick(6); },
            0x53 => { self.sre(Mode::IndirectIndexed); self.tick(8); },
            0x57 => { self.sre(Mode::ZeroPageX); self.tick(6); },
            0x5b => { self.sre(Mode::AbsoluteY); self.tick(7); },
            0x5f => { self.sre(Mode::AbsoluteX); self.tick(7); },

            // RRA
            0x63 => { self.rra(Mode::IndexedIndirect); self.tick(8); },
            0x67 => { self.rra(Mode::ZeroPage); self.tick(5); },
            0x6f => { self.rra(Mode::Absolute); self.tick(6); },
            0x73 => { self.rra(Mode::IndirectIndexed); self.tick(8); },
            0x77 => { self.rra(Mode::ZeroPageX); self.tick(6); },
            0x7b => { self.rra(Mode::AbsoluteY); self.tick(7); },
            0x7f => { self.rra(Mode::AbsoluteX); self.tick(7); },

            // INC
            0xe6 => { self.inc(Mode::ZeroPage); self.tick(5); },
            0xf6 => { self.inc(Mode::ZeroPageX); self.tick(6); },
            0xee => { self.inc(Mode::Absolute); self.tick(6); },
            0xfe => { self.inc(Mode::AbsoluteX); self.tick(7); },

            // DEC
            0xc6 => { self.dec(Mode::ZeroPage); self.tick(5); },
            0xd6 => { self.dec(Mode::ZeroPageX); self.tick(6); },
            0xce => { self.dec(Mode::Absolute); self.tick(6); },
            0xde => { self.dec(Mode::AbsoluteX); self.tick(7); },

            // ASL
            0x0a => { self.asl(Mode::Accumulator); self.tick(2); },
            0x06 => { self.asl(Mode::ZeroPage); self.tick(5); },
            0x16 => { self.asl(Mode::ZeroPageX); self.tick(6); },
            0x0e => { self.asl(Mode::Absolute); self.tick(6); },
            0x1e => { self.asl(Mode::AbsoluteX); self.tick(7); },

            // LSR
            0x4a => { self.lsr(Mode::Accumulator); self.tick(2); },
            0x46 => { self.lsr(Mode::ZeroPage); self.tick(5); },
            0x56 => { self.lsr(Mode::ZeroPageX); self.tick(6); },
            0x4e => { self.lsr(Mode::Absolute); self.tick(6); },
            0x5e => { self.lsr(Mode::AbsoluteX); self.tick(7); },

            // ROL
            0x2a => { self.rol(Mode::Accumulator); self.tick(2); },
            0x26 => { self.rol(Mode::ZeroPage); self.tick(5); },
            0x36 => { self.rol(Mode::ZeroPageX); self.tick(6); },
            0x2e => { self.rol(Mode::Absolute); self.tick(6); },
            0x3e => { self.rol(Mode::AbsoluteX); self.tick(7); },

            // ROR
            0x6a => { self.ror(Mode::Accumulator); self.tick(2); },
            0x66 => { self.ror(Mode::ZeroPage); self.tick(5); },
            0x76 => { self.ror(Mode::ZeroPageX); self.tick(6); },
            0x6e => { self.ror(Mode::Absolute); self.tick(6); },
            0x7e => { self.ror(Mode::AbsoluteX); self.tick(7); },

            // BIT
            0x24 => { self.bit(Mode::ZeroPage); self.tick(3); },
            0x2c => { self.bit(Mode::Absolute); self.tick(4); },

            // ADC
            0x69 => { self.adc(Mode::Immediate); self.tick(2); },
            0x65 => { self.adc(Mode::ZeroPage); self.tick(3); },
            0x75 => { self.adc(Mode::ZeroPageX); self.tick(4); },
            0x6d => { self.adc(Mode::Absolute); self.tick(4); },
            0x7d => { self.adc(Mode::AbsoluteX); self.tick(4); },
            0x79 => { self.adc(Mode::AbsoluteY); self.tick(4); },
            0x61 => { self.adc(Mode::IndexedIndirect); self.tick(6); },
            0x71 => { self.adc(Mode::IndirectIndexed); self.tick(5); },

            // SBC
            0xe9 | 0xeb => { self.sbc(Mode::Immediate); self.tick(2); },
            0xe5 => { self.sbc(Mode::ZeroPage); self.tick(3); },
            0xf5 => { self.sbc(Mode::ZeroPageX); self.tick(4); },
            0xed => { self.sbc(Mode::Absolute); self.tick(4); },
            0xfd => { self.sbc(Mode::AbsoluteX); self.tick(4); },
            0xf9 => { self.sbc(Mode::AbsoluteY); self.tick(4); },
            0xe1 => { self.sbc(Mode::IndexedIndirect); self.tick(6); },
            0xf1 => { self.sbc(Mode::IndirectIndexed); self.tick(5); },

            // AND
            0x29 => { self.and(Mode::Immediate); self.tick(2); },
            0x25 => { self.and(Mode::ZeroPage); self.tick(3); },
            0x35 => { self.and(Mode::ZeroPageX); self.tick(4); },
            0x2d => { self.and(Mode::Absolute); self.tick(4); },
            0x3d => { self.and(Mode::AbsoluteX); self.tick(4); },
            0x39 => { self.and(Mode::AbsoluteY); self.tick(4); },
            0x21 => { self.and(Mode::IndexedIndirect); self.tick(6); },
            0x31 => { self.and(Mode::IndirectIndexed); self.tick(5); },

            // EOR
            0x49 => { self.eor(Mode::Immediate); self.tick(2); },
            0x45 => { self.eor(Mode::ZeroPage); self.tick(3); },
            0x55 => { self.eor(Mode::ZeroPageX); self.tick(4); },
            0x4d => { self.eor(Mode::Absolute); self.tick(4); },
            0x5d => { self.eor(Mode::AbsoluteX); self.tick(4); },
            0x59 => { self.eor(Mode::AbsoluteY); self.tick(4); },
            0x41 => { self.eor(Mode::IndexedIndirect); self.tick(6); },
            0x51 => { self.eor(Mode::IndirectIndexed); self.tick(5); },

            // ORA
            0x09 => { self.ora(Mode::Immediate); self.tick(2); },
            0x05 => { self.ora(Mode::ZeroPage); self.tick(3); },
            0x15 => { self.ora(Mode::ZeroPageX); self.tick(4); },
            0x0d => { self.ora(Mode::Absolute); self.tick(4); },
            0x1d => { self.ora(Mode::AbsoluteX); self.tick(4); },
            0x19 => { self.ora(Mode::AbsoluteY); self.tick(4); },
            0x01 => { self.ora(Mode::IndexedIndirect); self.tick(6); },
            0x11 => { self.ora(Mode::IndirectIndexed); self.tick(5); },

            // LDA
            0xa9 => { self.lda(Mode::Immediate); self.tick(2); },
            0xa5 => { self.lda(Mode::ZeroPage); self.tick(3); },
            0xb5 => { self.lda(Mode::ZeroPageX); self.tick(4); },
            0xad => { self.lda(Mode::Absolute); self.tick(4); },
            0xbd => { self.lda(Mode::AbsoluteX); self.tick(4); },
            0xb9 => { self.lda(Mode::AbsoluteY); self.tick(4); },
            0xa1 => { self.lda(Mode::IndexedIndirect); self.tick(6); },
            0xb1 => { self.lda(Mode::IndirectIndexed); self.tick(5); },

            // LDX
            0xa2 => { self.ldx(Mode::Immediate); self.tick(2); },
            0xa6 => { self.ldx(Mode::ZeroPage); self.tick(3); },
            0xb6 => { self.ldx(Mode::ZeroPageY); self.tick(4); },
            0xae => { self.ldx(Mode::Absolute); self.tick(4); },
            0xbe => { self.ldx(Mode::AbsoluteY); self.tick(4); },

            // LDY
            0xa0 => { self.ldy(Mode::Immediate); self.tick(2); },
            0xa4 => { self.ldy(Mode::ZeroPage); self.tick(3); },
            0xb4 => { self.ldy(Mode::ZeroPageX); self.tick(4); },
            0xac => { self.ldy(Mode::Absolute); self.tick(4); },
            0xbc => { self.ldy(Mode::AbsoluteX); self.tick(4); },

            // CMP
            0xc9 => { self.cmp(Mode::Immediate); self.tick(2); },
            0xc5 => { self.cmp(Mode::ZeroPage); self.tick(3); },
            0xd5 => { self.cmp(Mode::ZeroPageX); self.tick(4); },
            0xcd => { self.cmp(Mode::Absolute); self.tick(4); },
            0xdd => { self.cmp(Mode::AbsoluteX); self.tick(4); },
            0xd9 => { self.cmp(Mode::AbsoluteY); self.tick(4); },
            0xc1 => { self.cmp(Mode::IndexedIndirect); self.tick(6); },
            0xd1 => { self.cmp(Mode::IndirectIndexed); self.tick(5); }

            // CPX
            0xe0 => { self.cpx(Mode::Immediate); self.tick(2); },
            0xe4 => { self.cpx(Mode::ZeroPage); self.tick(3); },
            0xec => { self.cpx(Mode::Absolute); self.tick(4); },

            // CPY
            0xc0 => { self.cpy(Mode::Immediate); self.tick(2); },
            0xc4 => { self.cpy(Mode::ZeroPage); self.tick(3); },
            0xcc => { self.cpy(Mode::Absolute); self.tick(4); },

            // JMP
            0x4c => { self.jmp(Mode::Absolute); self.tick(3); },
            0x6c => { self.jmp(Mode::Indirect); self.tick(5); },

            // JSR
            0x20 => { self.jsr(Mode::Absolute); self.tick(6); },

            // RTS
            0x60 => { self.rts(Mode::Implied); self.tick(6); },

            // STA
            0x85 => { self.sta(Mode::ZeroPage); self.tick(3); },
            0x95 => { self.sta(Mode::ZeroPageX); self.tick(4); },
            0x8d => { self.sta(Mode::Absolute); self.tick(4); },
            0x9d => { self.sta(Mode::AbsoluteX); self.tick(5); },
            0x99 => { self.sta(Mode::AbsoluteY); self.tick(5); },
            0x81 => { self.sta(Mode::IndexedIndirect); self.tick(6); },
            0x91 => { self.sta(Mode::IndirectIndexed); self.tick(6); },

            // STX
            0x86 => { self.stx(Mode::ZeroPage); self.tick(3); },
            0x96 => { self.stx(Mode::ZeroPageY); self.tick(4); },
            0x8e => { self.stx(Mode::Absolute); self.tick(4); },

            // STY
            0x84 => { self.sty(Mode::ZeroPage); self.tick(3); },
            0x94 => { self.sty(Mode::ZeroPageX); self.tick(4); },
            0x8c => { self.sty(Mode::Absolute); self.tick(4); },

            _ => panic!("opcode {:x} not implemented at {:x}.", opcode, self.pc),
        }

        if self.debug {
            self.debug_state = Some(DebugState {
                pc: debug_state.0,
                sp: debug_state.1,
                a: debug_state.2,
                x: debug_state.3,
                y: debug_state.4,
                p: debug_state.5,
                clock: debug_state.6,
                instruction: match self.disassembler.as_ref().unwrap().last() {
                    Some(instruction) => instruction.to_vec(),
                    None => vec!(),
                },
            });
        }

        // For blargg test
        // let mut done = false;
        // let mut s = String::new();
        // let mut address = 0x6000;

        // while !done {
        //     let c = self.bus.read(address, true);

        //     if c == 0x00 {
        //         done =true;
        //     } else {
        //         s.push(c as char);
        //         address += 1;
        //     }
        // }

        // if !s.is_empty() {
        //     println!("{}", s);
        // }

        self.clock
    }

    fn tick(&mut self, cycles: u8) {
        self.clock += cycles as u32;
    }

    fn set_flag(&mut self, flag: Flag, value: u8) {
        match flag {
            Flag::CarryFlag =>          self.p = (self.p & !(1 << 0)) | (value << 0),
            Flag::ZeroFlag =>           self.p = (self.p & !(1 << 1)) | (value << 1),
            Flag::InterruptDisable =>   self.p = (self.p & !(1 << 2)) | (value << 2),
            Flag::DecimalMode =>        self.p = (self.p & !(1 << 3)) | (value << 3),
            Flag::BreakCommand =>       self.p = (self.p & !(1 << 4)) | (value << 4),
            Flag::Unused =>             self.p = (self.p & !(1 << 5)) | (value << 5),
            Flag::OverflowFlag =>       self.p = (self.p & !(1 << 6)) | (value << 6),
            Flag::NegativeFlag =>       self.p = (self.p & !(1 << 7)) | (value << 7),
        }
    }

    fn get_flag(&mut self, flag: Flag) -> u8 {
        match flag {
            Flag::CarryFlag =>          if self.p & (1 << 0) > 0 { 1 } else { 0 },
            Flag::ZeroFlag =>           if self.p & (1 << 1) > 0 { 1 } else { 0 },
            Flag::InterruptDisable =>   if self.p & (1 << 2) > 0 { 1 } else { 0 },
            Flag::DecimalMode =>        if self.p & (1 << 3) > 0 { 1 } else { 0 },
            Flag::BreakCommand =>       if self.p & (1 << 4) > 0 { 1 } else { 0 },
            Flag::Unused =>             if self.p & (1 << 5) > 0 { 1 } else { 0 },
            Flag::OverflowFlag =>       if self.p & (1 << 6) > 0 { 1 } else { 0 },
            Flag::NegativeFlag =>       if self.p & (1 << 7) > 0 { 1 } else { 0 },
        }
    }

    fn set_flag_if_zero(&mut self, byte: u8) {
        if byte == 0 {
            self.set_flag(Flag::ZeroFlag, 1);
        } else {
            self.set_flag(Flag::ZeroFlag, 0);
        }
    }

    fn set_flag_if_negative(&mut self, byte: u8) {
        if byte & (1 << 7) > 0 {
            self.set_flag(Flag::NegativeFlag, 1);
        } else {
            self.set_flag(Flag::NegativeFlag, 0);
        }
    }

    fn read_opcode(&mut self) -> u8 {
        let opcode = self.bus.read(self.pc, false);
        self.pc += 1;
        opcode
    }

    fn read_address(&mut self, mode: Mode, tick: bool) -> u16 {
        let start_pc = self.pc;

        let address = match mode {
            Mode::Absolute => {
                let lo = self.bus.read(self.pc, false) as u16;
                let hi = self.bus.read(self.pc + 1, false) as u16;
                let address = (hi << 8) | lo;

                self.pc += 2;

                address
            },
            Mode::AbsoluteX => {
                let lo = self.bus.read(self.pc, false) as u16;
                let hi = self.bus.read(self.pc + 1, false) as u16;
                let address = ((hi << 8) | lo).wrapping_add(self.x as u16);

                if tick && address & 0xff00 != hi << 8 {
                    self.tick(1);
                }

                self.pc += 2;

                address
            },
            Mode::AbsoluteY => {
                let lo = self.bus.read(self.pc, false) as u16;
                let hi = self.bus.read(self.pc + 1, false) as u16;
                let address = ((hi << 8) | lo).wrapping_add(self.y as u16);

                if tick && address & 0xff00 != hi << 8 {
                    self.tick(1);
                }

                self.pc += 2;

                address
            },
            Mode::Accumulator => {
                0x0000
            },
            Mode::Immediate => {
                let address = self.pc;

                self.pc += 1;

                address
            },
            Mode::Implied => {
                0x0000
            },
            Mode::IndexedIndirect => {
                let operand = self.bus.read(self.pc, false);
                let address = ((self.bus.read(operand.wrapping_add(self.x + 1) as u16, false) as u16) << 8) | self.bus.read(operand.wrapping_add(self.x) as u16, false) as u16;

                self.pc += 1;

                address
            },
            Mode::Indirect => {
                let lo = self.bus.read(self.pc, false) as u16;
                let hi = self.bus.read(self.pc + 1, false) as u16;
                let mut address = (hi << 8) | lo;

                address = if lo == 0xff {
                    ((self.bus.read(address & 0xff00, false) as u16) << 8) | self.bus.read(address, false) as u16
                } else {
                    ((self.bus.read(address + 1, false) as u16) << 8) | self.bus.read(address, false) as u16
                };

                self.pc += 2;

                address
            },
            Mode::IndirectIndexed => {
                let operand = self.bus.read(self.pc, false) as u16;
                let lo = self.bus.read(operand, false) as u16;
                let hi = self.bus.read((operand + 1) % 256, false) as u16;
                let address = ((hi << 8) | lo).wrapping_add(self.y as u16);

                if tick && address & 0xff00 != hi << 8 {
                    self.tick(1);
                }

                self.pc += 1;

                address
            },
            Mode::Relative => {
                let address = self.pc;

                self.pc += 1;

                address
            },
            Mode::ZeroPage => {
                let address = self.bus.read(self.pc, false) as u16;

                self.pc += 1;

                address
            },
            Mode::ZeroPageX => {
                let address = self.bus.read(self.pc, false).wrapping_add(self.x) as u16;

                self.pc += 1;

                address
            }
            Mode::ZeroPageY => {
                let address = self.bus.read(self.pc, false).wrapping_add(self.y) as u16;

                self.pc += 1;

                address
            }
        };

        let stop_pc = self.pc;

        if let Some(disassembler) = &mut self.disassembler {
            let mut bytes = Vec::new();

            for debug_address in start_pc - 1..stop_pc {
                bytes.push(self.bus.read(debug_address, true));
            }

            disassembler.load(bytes.as_slice());
        }

        address
    }

    fn read_operand(&mut self, mode: Mode) -> u8 {
        let address = self.read_address(mode, true);

        self.bus.read(address, false)
    }

    fn compare(&mut self, a: u8, b: u8) {
        let result = a.wrapping_sub(b);

        self.set_flag_if_negative(result);

        if a == b {
            self.set_flag(Flag::ZeroFlag, 1);
        } else {
            self.set_flag(Flag::ZeroFlag, 0);
        }

        if a >= b {
            self.set_flag(Flag::CarryFlag, 1);
        } else {
            self.set_flag(Flag::CarryFlag, 0);
        }
    }

    fn branch(&mut self, offset: u16, condition: bool) {
        if condition {
            let pc = self.pc.wrapping_add(offset);
            if pc & 0xff00 != self.pc & 0xff00 {
                self.tick(1);
            }
            self.pc = pc;
            self.tick(1);
        }
    }

    fn push(&mut self, byte: u8) {
        let address = 0x100 + self.sp as u16;
        self.bus.write(address, byte);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pull(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let address = 0x100 + self.sp as u16;
        self.bus.read(address, false)
    }

    // Single byte
    fn clc(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.set_flag(Flag::CarryFlag, 0);
    }

    fn cld(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.set_flag(Flag::DecimalMode, 0);
    }

    fn cli(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.set_flag(Flag::InterruptDisable, 0);
    }

    fn clv(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.set_flag(Flag::OverflowFlag, 0);
    }

    fn dex(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.x = self.x.wrapping_sub(1);
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
    }

    fn txs(&mut self, mode: Mode) {
        self.read_address(mode, true);

        self.sp = self.x;
    }

    fn dey(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.y = self.y.wrapping_sub(1);
        self.set_flag_if_negative(self.y);
        self.set_flag_if_zero(self.y);
    }

    fn tay(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.y = self.a;
        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
    }

    fn tya(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.a = self.y;
        self.set_flag_if_negative(self.y);
        self.set_flag_if_zero(self.y);
    }

    fn tax(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.x = self.a;
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
    }

    fn txa(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.a = self.x;
        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
    }

    fn tsx(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.x = self.sp;
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
    }

    fn nop(&mut self, mode: Mode) {
        self.read_address(mode, true);
    }

    fn pha(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.push(self.a);
    }

    fn php(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.push(self.p | 0x10);
    }

    fn pla(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.a = self.pull();
        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
    }

    fn plp(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.p = self.pull() & 0xEF | 0x20;
    }

    fn inc(&mut self, mode: Mode) {
        let address = self.read_address(mode, true);
        let operand = self.bus.read(address, false).wrapping_add(1);

        self.set_flag_if_negative(operand);
        self.set_flag_if_zero(operand);

        self.bus.write(address, operand);
    }

    fn dec(&mut self, mode: Mode) {
        let address = self.read_address(mode, true);
        let operand = self.bus.read(address, false).wrapping_sub(1);
// println!("{:x}", operand);
        self.set_flag_if_negative(operand);
        self.set_flag_if_zero(operand);

        self.bus.write(address, operand);
    }

    fn inx(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.x = self.x.wrapping_add(1);
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
    }

    fn iny(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.y = self.y.wrapping_add(1);
        self.set_flag_if_negative(self.y);
        self.set_flag_if_zero(self.y);
    }

    fn sec(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.set_flag(Flag::CarryFlag, 1);
    }

    fn sei(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.set_flag(Flag::InterruptDisable, 1);
    }

    fn sed(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.set_flag(Flag::DecimalMode, 1);
    }

    fn brk(&mut self, mode: Mode) {
        self.read_address(mode, true);

        self.set_flag(Flag::BreakCommand, 1);

        let address = self.bus.read(0xfffe, false) as u16 | (self.bus.read(0xffff, false) as u16) << 8;

        self.push((self.pc >> 8) as u8);
        self.push(self.pc as u8);
        self.push(self.p);

        self.set_flag(Flag::InterruptDisable, 1);

        self.pc = address;
    }

    fn rti(&mut self, mode: Mode) {
        self.read_address(mode, true);
        self.p = self.pull();
        self.pc = (self.pull() as u16) | (self.pull() as u16) << 8;

        self.set_flag(Flag::Unused, 1);
    }

    fn beq(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as i8 as u16;
        let condition = self.get_flag(Flag::ZeroFlag) == 1;

        self.branch(operand, condition);
    }

    fn bne(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as i8 as u16;
        let condition = self.get_flag(Flag::ZeroFlag) == 0;

        self.branch(operand, condition);
    }

    fn bcc(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as i8 as u16;
        let condition = self.get_flag(Flag::CarryFlag) == 0;

        self.branch(operand, condition);
    }

    fn bcs(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as i8 as u16;
        let condition = self.get_flag(Flag::CarryFlag) == 1;

        self.branch(operand, condition);
    }

    fn bmi(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as i8 as u16;
        let condition = self.get_flag(Flag::NegativeFlag) == 1;

        self.branch(operand, condition);
    }

    fn bpl(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as i8 as u16;
        let condition = self.get_flag(Flag::NegativeFlag) == 0;

        self.branch(operand, condition);
    }

    fn bvs(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as i8 as u16;
        let condition = self.get_flag(Flag::OverflowFlag) == 1;

        self.branch(operand, condition);
    }

    fn bvc(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as i8 as u16;
        let condition = self.get_flag(Flag::OverflowFlag) == 0;

        self.branch(operand, condition);
    }

    fn asl(&mut self, mode: Mode) {
        if let Mode::Accumulator = mode {
            self.read_address(mode, true);
            let mut operand = self.a as u16;

            operand <<= 1;

            self.set_flag(Flag::CarryFlag, if operand > 0xff { 1 } else { 0 });
            self.set_flag(Flag::ZeroFlag, if operand & 0x00ff == 0x00 { 1 } else { 0 });
            self.set_flag(Flag::NegativeFlag, if operand & 0x80 > 0 { 1 } else { 0 });

            self.a = operand as u8;
        } else {
            let address = self.read_address(mode, true);
            let mut operand = self.bus.read(address, false) as u16;

            operand <<= 1;

            self.set_flag(Flag::CarryFlag, if operand > 0xff { 1 } else { 0 });
            self.set_flag(Flag::ZeroFlag, if operand & 0x00ff == 0x00 { 1 } else { 0 });
            self.set_flag(Flag::NegativeFlag, if operand & 0x80 > 0 { 1 } else { 0 });

            self.bus.write(address, operand as u8);
        }
    }

    fn lsr(&mut self, mode: Mode) {
        if let Mode::Accumulator = mode {
            self.read_address(mode, true);
            let mut operand = self.a as u16;

            self.set_flag(Flag::CarryFlag, if operand & 1 > 0 { 1 } else { 0 });

            operand >>= 1;

            self.set_flag(Flag::ZeroFlag, if operand & 0x00ff == 0x00 { 1 } else { 0 });
            self.set_flag(Flag::NegativeFlag, if operand & 0x80 > 0 { 1 } else { 0 });

            self.a = operand as u8;
        } else {
            let address = self.read_address(mode, true);
            let mut operand = self.bus.read(address, false) as u16;

            self.set_flag(Flag::CarryFlag, if operand & 1 > 0 { 1 } else { 0 });

            operand >>= 1;

            self.set_flag(Flag::ZeroFlag, if operand & 0x00ff == 0x00 { 1 } else { 0 });
            self.set_flag(Flag::NegativeFlag, if operand & 0x80 > 0 { 1 } else { 0 });

            self.bus.write(address, operand as u8);
        }
    }

    fn rol(&mut self, mode: Mode) {
        if let Mode::Accumulator = mode {
            self.read_address(mode, true);
            let operand = self.a as u16;

            let result = (operand << 1) | self.get_flag(Flag::CarryFlag) as u16;

            self.set_flag(Flag::CarryFlag, if result > 0xff { 1 } else { 0 });
            self.set_flag_if_zero(result as u8);
            self.set_flag_if_negative(result as u8);

            self.a = result as u8;
        } else {
            let address = self.read_address(mode, true);
            let operand = self.bus.read(address, false) as u16;

            let result = (operand << 1) | self.get_flag(Flag::CarryFlag) as u16;

            self.set_flag(Flag::CarryFlag, if result > 0xff { 1 } else { 0 });
            self.set_flag_if_zero(result as u8);
            self.set_flag_if_negative(result as u8);

            self.bus.write(address, result as u8);
        }
    }

    fn ror(&mut self, mode: Mode) {
        if let Mode::Accumulator = mode {
            self.read_address(mode, true);
            let operand = self.a;

            let result = ((operand as u16) >> 1) | ((self.get_flag(Flag::CarryFlag) as u16) << 7);

            self.set_flag(Flag::CarryFlag, if operand & 0x01 != 0 { 1 } else { 0 });
            self.set_flag_if_zero(result as u8);
            self.set_flag_if_negative(result as u8);

            self.a = result as u8;
        } else {
            let address = self.read_address(mode, true);
            let operand = self.bus.read(address, false);

            let result = ((operand as u16) >> 1) | ((self.get_flag(Flag::CarryFlag) as u16) << 7);

            self.set_flag(Flag::CarryFlag, if operand & 0x01 != 0 { 1 } else { 0 });
            self.set_flag_if_zero(result as u8);
            self.set_flag_if_negative(result as u8);

            self.bus.write(address, result as u8);
        }
    }

    fn bit(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);
        let result = self.a & operand;

        self.set_flag_if_zero(result);
        self.set_flag(Flag::OverflowFlag, if operand & (1 << 6) > 0 { 1 } else { 0 });
        self.set_flag(Flag::NegativeFlag, if operand & (1 << 7) > 0 { 1 } else { 0 });
    }

    fn adc(&mut self, mode: Mode) {
        let operand = self.read_operand(mode) as u16;

        let result = (self.a as u16) + operand + (self.get_flag(Flag::CarryFlag) as u16);
        let overflow =  if !((((self.a as u16) ^ operand) & 0x80) != 0) && ((((self.a as u16) ^ result) & 0x80) != 0) { 1 } else { 0 };
        let carry = if result > 0xff { 1 } else { 0 };
        let zero = if result & 0x00ff == 0x00 { 1 } else { 0 };
        let negative = if result & 0x80 > 0 { 1 } else { 0 };

        self.set_flag(Flag::OverflowFlag, overflow);
        self.set_flag(Flag::CarryFlag, carry);
        self.set_flag(Flag::ZeroFlag, zero);
        self.set_flag(Flag::NegativeFlag, negative);

        self.a = (result & 0xff) as u8;
    }

    fn sbc(&mut self, mode: Mode) {
        let operand = (self.read_operand(mode) as u16) ^ 0x00ff;

        let result = (self.a as u16) + operand + (self.get_flag(Flag::CarryFlag) as u16);
        let overflow = if ((self.a as u16) ^ result) & (operand ^ result) & 0x80 == 0x80 { 1 } else { 0 };
        let carry = if result > 0xff { 1 } else { 0 };
        let zero = if result & 0x00ff == 0x00 { 1 } else { 0 };
        let negative = if result & 0x80 > 0 { 1 } else { 0 };

        self.set_flag(Flag::OverflowFlag, overflow);
        self.set_flag(Flag::CarryFlag, carry);
        self.set_flag(Flag::ZeroFlag, zero);
        self.set_flag(Flag::NegativeFlag, negative);

        self.a = (result & 0xff) as u8;
    }

    fn and(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.a = self.a & operand;

        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
    }

    fn eor(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.a = self.a ^ operand;

        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
    }

    fn ora(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.a = self.a | operand;

        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
    }

    fn lda(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.a = operand;

        self.set_flag_if_zero(operand);
        self.set_flag_if_negative(operand);
    }

    fn ldx(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.x = operand;

        self.set_flag_if_zero(operand);
        self.set_flag_if_negative(operand);
    }

    fn ldy(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.y = operand;

        self.set_flag_if_zero(operand);
        self.set_flag_if_negative(operand);
    }

    fn cmp(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.compare(self.a, operand);
    }

    fn cpx(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.compare(self.x, operand);
    }

    fn cpy(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);

        self.compare(self.y, operand);
    }

    fn jmp(&mut self, mode: Mode) {
        let address = self.read_address(mode, true);

        self.pc = address;
    }

    fn jsr(&mut self, mode: Mode) {
        let target_address = self.read_address(mode, true);
        let return_address = self.pc - 1;

        self.push((return_address >> 8) as u8);
        self.push(return_address as u8);
        self.pc = target_address;
    }

    fn rts(&mut self, mode: Mode) {
        self.read_address(mode, true);
        let return_address = (self.pull() as u16) | (self.pull() as u16) << 8;
        self.pc = return_address + 1;
    }

    fn sta(&mut self, mode: Mode) {
        let address = self.read_address(mode, false);

        self.bus.write(address, self.a);
    }

    fn stx(&mut self, mode: Mode) {
        let address = self.read_address(mode, true);

        self.bus.write(address, self.x);
    }

    fn sty(&mut self, mode: Mode) {
        let address = self.read_address(mode, true);

        self.bus.write(address, self.y);
    }

    fn lax(&mut self, mode: Mode) {
        let operand = self.read_operand(mode);
        self.a = operand;
        self.x = self.a;
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
    }

    fn sax(&mut self, mode: Mode) {
        let address = self.read_address(mode, true);

        self.bus.write(address, self.a & self.x);
    }

    fn dcp(&mut self, mode: Mode) {
        let address = self.read_address(mode, false);
        let operand = self.bus.read(address, false).wrapping_sub(1);
        self.bus.write(address, operand);

        self.compare(self.a, operand);
    }

    fn isc(&mut self, mode: Mode) {
        let address = self.read_address(mode, false);
        let mut operand = self.bus.read(address, false).wrapping_add(1) as u16;
        self.bus.write(address, operand as u8);

        operand ^= 0x00ff;

        let result = (self.a as u16) + operand + (self.get_flag(Flag::CarryFlag) as u16);
        let overflow = if ((self.a as u16) ^ result) & (operand ^ result) & 0x80 == 0x80 { 1 } else { 0 };
        let carry = if result > 0xff { 1 } else { 0 };
        let zero = if result & 0x00ff == 0x00 { 1 } else { 0 };
        let negative = if result & 0x80 > 0 { 1 } else { 0 };

        self.set_flag(Flag::OverflowFlag, overflow);
        self.set_flag(Flag::CarryFlag, carry);
        self.set_flag(Flag::ZeroFlag, zero);
        self.set_flag(Flag::NegativeFlag, negative);

        self.a = (result & 0xff) as u8;
    }

    fn slo(&mut self, mode: Mode) {
        let address = self.read_address(mode, false);
        let mut operand = self.bus.read(address, false) as u16;

        operand <<= 1;
        self.bus.write(address, operand as u8);

        operand = self.a as u16 | operand;
        self.a = operand as u8;

        self.set_flag(Flag::CarryFlag, if operand > 0xff { 1 } else { 0 });
        self.set_flag(Flag::ZeroFlag, if operand & 0x00ff == 0x00 { 1 } else { 0 });
        self.set_flag(Flag::NegativeFlag, if operand & 0x80 > 0 { 1 } else { 0 });
    }

    fn rla(&mut self, mode: Mode) {
        let address = self.read_address(mode, false);
        let mut operand = self.bus.read(address, false) as u16;

        operand = (operand << 1) | self.get_flag(Flag::CarryFlag) as u16;
        self.bus.write(address, operand as u8);

        self.set_flag(Flag::CarryFlag, if operand > 0xff { 1 } else { 0 });

        operand = self.a as u16 & operand;
        self.a = operand as u8;

        self.set_flag_if_zero(operand as u8);
        self.set_flag_if_negative(operand as u8);
    }

    fn sre(&mut self, mode: Mode) {
        let address = self.read_address(mode, false);
        let mut operand = self.bus.read(address, false) as u16;

        self.set_flag(Flag::CarryFlag, if operand & 1 > 0 { 1 } else { 0 });

        operand >>= 1;
        self.bus.write(address, operand as u8);

        operand = self.a as u16 ^ operand;
        self.a = operand as u8;

        self.set_flag(Flag::ZeroFlag, if operand & 0x00ff == 0x00 { 1 } else { 0 });
        self.set_flag(Flag::NegativeFlag, if operand & 0x80 > 0 { 1 } else { 0 });
    }

    fn rra(&mut self, mode: Mode) {
        let address = self.read_address(mode, false);
        let mut operand = self.bus.read(address, false) as u16;

        operand = (operand >> 1) | ((self.get_flag(Flag::CarryFlag) as u16) << 7);

        let carry = if self.bus.read(address, false) & 0x01 != 0 { 1 } else { 0 };

        self.set_flag(Flag::CarryFlag, carry);

        self.bus.write(address, operand as u8);

        let result = (self.a as u16) + operand + (self.get_flag(Flag::CarryFlag) as u16);
        let overflow =  if !((((self.a as u16) ^ operand) & 0x80) != 0) && ((((self.a as u16) ^ result) & 0x80) != 0) { 1 } else { 0 };
        let carry = if result > 0xff { 1 } else { 0 };
        let zero = if result & 0x00ff == 0x00 { 1 } else { 0 };
        let negative = if result & 0x80 > 0 { 1 } else { 0 };

        self.set_flag(Flag::OverflowFlag, overflow);
        self.set_flag(Flag::CarryFlag, carry);
        self.set_flag(Flag::ZeroFlag, zero);
        self.set_flag(Flag::NegativeFlag, negative);

        self.a = (result & 0xff) as u8;
    }

    pub fn nmi(&mut self) {
        // self.read_address(mode, true);

        self.set_flag(Flag::BreakCommand, 1);

        let address = self.bus.read(0xfffa, false) as u16 | (self.bus.read(0xfffb, false) as u16) << 8;

        self.push((self.pc >> 8) as u8);
        self.push(self.pc as u8);
        self.push(self.p);

        self.set_flag(Flag::InterruptDisable, 1);

        self.pc = address;

        self.tick(8);
    }

    pub fn disassemble(&mut self, start: u16, stop: u16) -> Vec<InstructionLine> {
        let mut lines: Vec<InstructionLine> = Vec::new();
        let mut instructions: HashMap<u8, Instruction> = HashMap::new();

        instructions.insert(0x00, Instruction::new("BRK", "IMP"));
        instructions.insert(0x01, Instruction::new("ORA", "IDX"));
        instructions.insert(0x03, Instruction::new("SLO", "IDX"));
        instructions.insert(0x04, Instruction::new("NOP", "ZPG"));
        instructions.insert(0x05, Instruction::new("ORA", "ZPG"));
        instructions.insert(0x06, Instruction::new("ASL", "ZPG"));
        instructions.insert(0x07, Instruction::new("SLO", "ZPG"));
        instructions.insert(0x08, Instruction::new("PHP", "IMP"));
        instructions.insert(0x09, Instruction::new("ORA", "IMM"));
        instructions.insert(0x0a, Instruction::new("ASL", "IMP"));
        instructions.insert(0x0c, Instruction::new("NOP", "ABS"));
        instructions.insert(0x0d, Instruction::new("ORA", "ABS"));
        instructions.insert(0x0e, Instruction::new("ASL", "ABS"));
        instructions.insert(0x0f, Instruction::new("SLO", "ABS"));
        instructions.insert(0x10, Instruction::new("BPL", "REL"));
        instructions.insert(0x11, Instruction::new("ORA", "IDY"));
        instructions.insert(0x13, Instruction::new("SLO", "IDY"));
        instructions.insert(0x14, Instruction::new("NOP", "ZPX"));
        instructions.insert(0x15, Instruction::new("ORA", "ZPX"));
        instructions.insert(0x16, Instruction::new("ASL", "ZPX"));
        instructions.insert(0x17, Instruction::new("SLO", "ZPX"));
        instructions.insert(0x18, Instruction::new("CLC", "IMP"));
        instructions.insert(0x19, Instruction::new("ORA", "ABY"));
        instructions.insert(0x1a, Instruction::new("NOP", "IMP"));
        instructions.insert(0x1b, Instruction::new("SLO", "ABY"));
        instructions.insert(0x1c, Instruction::new("NOP", "ABX"));
        instructions.insert(0x1d, Instruction::new("ORA", "ABX"));
        instructions.insert(0x1e, Instruction::new("ASL", "ABX"));
        instructions.insert(0x1f, Instruction::new("SLO", "ABX"));
        instructions.insert(0x20, Instruction::new("JSR", "ABS"));
        instructions.insert(0x21, Instruction::new("AND", "IDX"));
        instructions.insert(0x23, Instruction::new("RLA", "IDX"));
        instructions.insert(0x24, Instruction::new("BIT", "ZPG"));
        instructions.insert(0x25, Instruction::new("AND", "ZPG"));
        instructions.insert(0x26, Instruction::new("ROL", "ZPG"));
        instructions.insert(0x27, Instruction::new("RLA", "ZPG"));
        instructions.insert(0x28, Instruction::new("PLP", "IMP"));
        instructions.insert(0x29, Instruction::new("AND", "IMM"));
        instructions.insert(0x2a, Instruction::new("ROL", "IMP"));
        instructions.insert(0x2c, Instruction::new("BIT", "ABS"));
        instructions.insert(0x2d, Instruction::new("AND", "ABS"));
        instructions.insert(0x2e, Instruction::new("ROL", "ABS"));
        instructions.insert(0x2f, Instruction::new("RLA", "ABS"));
        instructions.insert(0x30, Instruction::new("BMI", "REL"));
        instructions.insert(0x31, Instruction::new("AND", "IDY"));
        instructions.insert(0x33, Instruction::new("RLA", "IDY"));
        instructions.insert(0x34, Instruction::new("NOP", "ZPX"));
        instructions.insert(0x35, Instruction::new("AND", "ZPX"));
        instructions.insert(0x36, Instruction::new("ROL", "ZPX"));
        instructions.insert(0x37, Instruction::new("RLA", "ZPX"));
        instructions.insert(0x38, Instruction::new("SEC", "IMP"));
        instructions.insert(0x39, Instruction::new("AND", "ABY"));
        instructions.insert(0x3a, Instruction::new("NOP", "IMP"));
        instructions.insert(0x3b, Instruction::new("RLA", "ABY"));
        instructions.insert(0x3c, Instruction::new("NOP", "ABX"));
        instructions.insert(0x3d, Instruction::new("AND", "ABX"));
        instructions.insert(0x3e, Instruction::new("ROL", "ABX"));
        instructions.insert(0x3f, Instruction::new("RLA", "ABX"));
        instructions.insert(0x40, Instruction::new("RTI", "IMP"));
        instructions.insert(0x41, Instruction::new("EOR", "IDX"));
        instructions.insert(0x43, Instruction::new("SRE", "IDX"));
        instructions.insert(0x44, Instruction::new("NOP", "ZPG"));
        instructions.insert(0x45, Instruction::new("EOR", "ZPG"));
        instructions.insert(0x46, Instruction::new("LSR", "ZPG"));
        instructions.insert(0x47, Instruction::new("SRE", "ZPG"));
        instructions.insert(0x48, Instruction::new("PHA", "IMP"));
        instructions.insert(0x49, Instruction::new("EOR", "IMM"));
        instructions.insert(0x4a, Instruction::new("LSR", "IMP"));
        instructions.insert(0x4c, Instruction::new("JMP", "ABS"));
        instructions.insert(0x4d, Instruction::new("EOR", "ABS"));
        instructions.insert(0x4e, Instruction::new("LSR", "ABS"));
        instructions.insert(0x4f, Instruction::new("SRE", "ABS"));
        instructions.insert(0x50, Instruction::new("BVC", "REL"));
        instructions.insert(0x51, Instruction::new("EOR", "IDY"));
        instructions.insert(0x53, Instruction::new("SRE", "IDY"));
        instructions.insert(0x54, Instruction::new("NOP", "ZPX"));
        instructions.insert(0x55, Instruction::new("EOR", "ZPX"));
        instructions.insert(0x56, Instruction::new("LSR", "ZPX"));
        instructions.insert(0x57, Instruction::new("SRE", "ZPX"));
        instructions.insert(0x58, Instruction::new("CLI", "IMP"));
        instructions.insert(0x59, Instruction::new("EOR", "ABY"));
        instructions.insert(0x5a, Instruction::new("NOP", "IMP"));
        instructions.insert(0x5b, Instruction::new("SRE", "ABY"));
        instructions.insert(0x5c, Instruction::new("NOP", "ABX"));
        instructions.insert(0x5d, Instruction::new("EOR", "ABX"));
        instructions.insert(0x5e, Instruction::new("LSR", "ABX"));
        instructions.insert(0x5f, Instruction::new("SRE", "ABX"));
        instructions.insert(0x60, Instruction::new("RTS", "IMP"));
        instructions.insert(0x61, Instruction::new("ADC", "IDX"));
        instructions.insert(0x63, Instruction::new("RRA", "IDX"));
        instructions.insert(0x64, Instruction::new("NOP", "ZPG"));
        instructions.insert(0x65, Instruction::new("ADC", "ZPG"));
        instructions.insert(0x66, Instruction::new("ROR", "ZPG"));
        instructions.insert(0x67, Instruction::new("RRA", "ZPG"));
        instructions.insert(0x68, Instruction::new("PLA", "IMP"));
        instructions.insert(0x69, Instruction::new("ADC", "IMM"));
        instructions.insert(0x6a, Instruction::new("ROR", "IMP"));
        instructions.insert(0x6c, Instruction::new("JMP", "IND"));
        instructions.insert(0x6d, Instruction::new("ADC", "ABS"));
        instructions.insert(0x6e, Instruction::new("ROR", "ABS"));
        instructions.insert(0x6f, Instruction::new("RRA", "ABS"));
        instructions.insert(0x70, Instruction::new("BVS", "REL"));
        instructions.insert(0x71, Instruction::new("ADC", "IDY"));
        instructions.insert(0x73, Instruction::new("RRA", "IDY"));
        instructions.insert(0x74, Instruction::new("NOP", "ZPX"));
        instructions.insert(0x75, Instruction::new("ADC", "ZPX"));
        instructions.insert(0x76, Instruction::new("ROR", "ZPX"));
        instructions.insert(0x77, Instruction::new("RRA", "ZPX"));
        instructions.insert(0x78, Instruction::new("SEI", "IMP"));
        instructions.insert(0x79, Instruction::new("ADC", "ABY"));
        instructions.insert(0x7a, Instruction::new("NOP", "IMP"));
        instructions.insert(0x7b, Instruction::new("RRA", "ABY"));
        instructions.insert(0x7c, Instruction::new("NOP", "ABX"));
        instructions.insert(0x7d, Instruction::new("ADC", "ABX"));
        instructions.insert(0x7e, Instruction::new("ROR", "ABX"));
        instructions.insert(0x7f, Instruction::new("RRA", "ABX"));
        instructions.insert(0x80, Instruction::new("NOP", "IMM"));
        instructions.insert(0x81, Instruction::new("STA", "IDX"));
        instructions.insert(0x83, Instruction::new("SAX", "IDX"));
        instructions.insert(0x84, Instruction::new("STY", "ZPG"));
        instructions.insert(0x85, Instruction::new("STA", "ZPG"));
        instructions.insert(0x86, Instruction::new("STX", "ZPG"));
        instructions.insert(0x87, Instruction::new("SAX", "ZPG"));
        instructions.insert(0x88, Instruction::new("DEY", "IMP"));
        instructions.insert(0x8a, Instruction::new("TXA", "IMP"));
        instructions.insert(0x8c, Instruction::new("STY", "ABS"));
        instructions.insert(0x8d, Instruction::new("STA", "ABS"));
        instructions.insert(0x8e, Instruction::new("STX", "ABS"));
        instructions.insert(0x8f, Instruction::new("SAX", "ABS"));
        instructions.insert(0x90, Instruction::new("BCC", "REL"));
        instructions.insert(0x91, Instruction::new("STA", "IDY"));
        instructions.insert(0x94, Instruction::new("STY", "ZPX"));
        instructions.insert(0x95, Instruction::new("STA", "ZPX"));
        instructions.insert(0x96, Instruction::new("STX", "ZPY"));
        instructions.insert(0x97, Instruction::new("SAX", "ZPY"));
        instructions.insert(0x98, Instruction::new("TYA", "IMP"));
        instructions.insert(0x99, Instruction::new("STA", "ABY"));
        instructions.insert(0x9a, Instruction::new("TXS", "IMP"));
        instructions.insert(0x9d, Instruction::new("STA", "ABX"));
        instructions.insert(0xa0, Instruction::new("LDY", "IMM"));
        instructions.insert(0xa1, Instruction::new("LDA", "IDX"));
        instructions.insert(0xa2, Instruction::new("LDX", "IMM"));
        instructions.insert(0xa3, Instruction::new("LAX", "IDX"));
        instructions.insert(0xa4, Instruction::new("LDY", "ZPG"));
        instructions.insert(0xa5, Instruction::new("LDA", "ZPG"));
        instructions.insert(0xa6, Instruction::new("LDX", "ZPG"));
        instructions.insert(0xa7, Instruction::new("LAX", "ZPG"));
        instructions.insert(0xa8, Instruction::new("TAY", "IMP"));
        instructions.insert(0xa9, Instruction::new("LDA", "IMM"));
        instructions.insert(0xaa, Instruction::new("TAX", "IMP"));
        instructions.insert(0xac, Instruction::new("LDY", "ABS"));
        instructions.insert(0xad, Instruction::new("LDA", "ABS"));
        instructions.insert(0xae, Instruction::new("LDX", "ABS"));
        instructions.insert(0xaf, Instruction::new("LAX", "ABS"));
        instructions.insert(0xb0, Instruction::new("BCS", "REL"));
        instructions.insert(0xb1, Instruction::new("LDA", "IDY"));
        instructions.insert(0xb3, Instruction::new("LAX", "IDY"));
        instructions.insert(0xb4, Instruction::new("LDY", "ZPX"));
        instructions.insert(0xb5, Instruction::new("LDA", "ZPX"));
        instructions.insert(0xb6, Instruction::new("LDX", "ZPY"));
        instructions.insert(0xb7, Instruction::new("LAX", "ZPY"));
        instructions.insert(0xb8, Instruction::new("CLV", "IMP"));
        instructions.insert(0xb9, Instruction::new("LDA", "ABY"));
        instructions.insert(0xba, Instruction::new("TSX", "IMP"));
        instructions.insert(0xbc, Instruction::new("LDY", "ABX"));
        instructions.insert(0xbd, Instruction::new("LDA", "ABX"));
        instructions.insert(0xbe, Instruction::new("LDX", "ABY"));
        instructions.insert(0xbf, Instruction::new("LAX", "ABY"));
        instructions.insert(0xc0, Instruction::new("CPY", "IMM"));
        instructions.insert(0xc1, Instruction::new("CMP", "IDX"));
        instructions.insert(0xc3, Instruction::new("DCP", "IDX"));
        instructions.insert(0xc4, Instruction::new("CPY", "ZPG"));
        instructions.insert(0xc5, Instruction::new("CMP", "ZPG"));
        instructions.insert(0xc6, Instruction::new("DEC", "ZPG"));
        instructions.insert(0xc7, Instruction::new("DCP", "ZPG"));
        instructions.insert(0xc8, Instruction::new("INY", "IMP"));
        instructions.insert(0xc9, Instruction::new("CMP", "IMM"));
        instructions.insert(0xca, Instruction::new("DEX", "IMP"));
        instructions.insert(0xcc, Instruction::new("CPY", "ABS"));
        instructions.insert(0xcd, Instruction::new("CMP", "ABS"));
        instructions.insert(0xce, Instruction::new("DEC", "ABS"));
        instructions.insert(0xcf, Instruction::new("DCP", "ABS"));
        instructions.insert(0xd0, Instruction::new("BNE", "REL"));
        instructions.insert(0xd1, Instruction::new("CMP", "IDY"));
        instructions.insert(0xd3, Instruction::new("DCP", "IDY"));
        instructions.insert(0xd4, Instruction::new("NOP", "ZPX"));
        instructions.insert(0xd5, Instruction::new("CMP", "ZPX"));
        instructions.insert(0xd6, Instruction::new("DEC", "ZPX"));
        instructions.insert(0xd7, Instruction::new("DCP", "ZPX"));
        instructions.insert(0xd8, Instruction::new("CLD", "IMP"));
        instructions.insert(0xd9, Instruction::new("CMP", "ABY"));
        instructions.insert(0xda, Instruction::new("NOP", "IMP"));
        instructions.insert(0xdb, Instruction::new("DCP", "ABY"));
        instructions.insert(0xdc, Instruction::new("NOP", "ABX"));
        instructions.insert(0xdd, Instruction::new("CMP", "ABX"));
        instructions.insert(0xde, Instruction::new("DEC", "ABX"));
        instructions.insert(0xdf, Instruction::new("DCP", "ABX"));
        instructions.insert(0xe0, Instruction::new("CPX", "IMM"));
        instructions.insert(0xe1, Instruction::new("SBC", "IDX"));
        instructions.insert(0xe3, Instruction::new("ISC", "IDX"));
        instructions.insert(0xe4, Instruction::new("CPX", "ZPG"));
        instructions.insert(0xe5, Instruction::new("SBC", "ZPG"));
        instructions.insert(0xe6, Instruction::new("INC", "ZPG"));
        instructions.insert(0xe7, Instruction::new("ISC", "ZPG"));
        instructions.insert(0xe8, Instruction::new("INX", "IMP"));
        instructions.insert(0xe9, Instruction::new("SBC", "IMM"));
        instructions.insert(0xea, Instruction::new("NOP", "IMP"));
        instructions.insert(0xeb, Instruction::new("SBC", "IMM"));
        instructions.insert(0xec, Instruction::new("CPX", "ABS"));
        instructions.insert(0xed, Instruction::new("SBC", "ABS"));
        instructions.insert(0xee, Instruction::new("INC", "ABS"));
        instructions.insert(0xef, Instruction::new("ISC", "ABS"));
        instructions.insert(0xf0, Instruction::new("BEQ", "REL"));
        instructions.insert(0xf1, Instruction::new("SBC", "IDY"));
        instructions.insert(0xf3, Instruction::new("ISC", "IDY"));
        instructions.insert(0xf4, Instruction::new("NOP", "ZPX"));
        instructions.insert(0xf5, Instruction::new("SBC", "ZPX"));
        instructions.insert(0xf6, Instruction::new("INC", "ZPX"));
        instructions.insert(0xf7, Instruction::new("ISC", "ZPX"));
        instructions.insert(0xf8, Instruction::new("SED", "IMP"));
        instructions.insert(0xf9, Instruction::new("SBC", "ABY"));
        instructions.insert(0xfa, Instruction::new("NOP", "IMP"));
        instructions.insert(0xfb, Instruction::new("ISC", "ABY"));
        instructions.insert(0xfc, Instruction::new("NOP", "ABX"));
        instructions.insert(0xfd, Instruction::new("SBC", "ABX"));
        instructions.insert(0xfe, Instruction::new("INC", "ABX"));
        instructions.insert(0xff, Instruction::new("ISC", "ABX"));

        let mut address = start as u32;

        while address <= stop as u32 {
            let opcode_address = address as u16;
            let opcode = self.bus.read(address as u16, true);
            address += 1;

            let mut line = String::new();

            match instructions.get(&opcode) {
                Some(instruction) => {
                    line.push_str(&format!("{:04X}: ${}", opcode_address, instruction.label));

                    match &instruction.mode[..] {
                        "ABS" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;
                            let hi = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" ${:04X} - ABS", (hi << 8) | lo));

                        },
                        "ABX" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;
                            let hi = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" ${:04X}, X - ABX", (hi << 8) | lo));
                        },
                        "ABY" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;
                            let hi = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" ${:04X}, Y - ABY", (hi << 8) | lo));
                        },
                        "IMM" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" #${:02X} - IMM", lo));
                        },
                        "IMP" => {
                            line.push_str(" - IMP");
                        },
                        "IDX" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;
                            let hi = 0x00;

                            line.push_str(&format!(" (${:04X}, X) - IDX", (hi << 8) | lo));
                        },
                        "IND" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;
                            let hi = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" (${:04X}) - IND", (hi << 8) | lo));
                        },
                        "IDY" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;
                            let hi = 0x00;

                            line.push_str(&format!(" (${:04X}), Y - IDY", (hi << 8) | lo));
                        },
                        "REL" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" ${:02X} [${:04X}] - REL", lo, address.wrapping_add(lo as u32)));
                        },
                        "ZPG" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" ${:02X} - ZPG", lo));
                        },
                        "ZPX" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" ${:02X}, X - ZPX", lo));
                        },
                        "ZPY" => {
                            let lo = self.bus.read(address as u16, false) as u16;
                            address += 1;

                            line.push_str(&format!(" ${:02X}, Y - ZPY", lo));
                        },
                        _ => panic!("Invalid addressing mode"),
                    }
                },
                None => {
                    // line.push_str(&format!("{:04X}: $??? ({:02X}) ", opcode_address, opcode));
                },
            }


            lines.push(InstructionLine(opcode_address, line));
        }

        lines
    }
}

#[cfg(test)]
mod tests {
    // ...
}