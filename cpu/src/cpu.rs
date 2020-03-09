use std::fs;

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

pub struct Cpu {
    bus: Bus,
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
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
            sp: 0xff,
            a: 0,
            x: 0,
            y: 0,
            p: 0x30,
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

    pub fn debug(&mut self) {
        self.debug = true;
        self.disassembler = Some(Disassembler::new());
    }

    pub fn debug_state(&self) -> Option<&DebugState> {
        self.debug_state.as_ref()
    }
}

impl Cpu {
    pub fn run(&mut self) {
        self.step();
    }

    pub fn tick(&mut self, cycles: u8) {
        self.clock += cycles as u32;
    }

    pub fn step(&mut self) {
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
        let opcode = self.bus.read(self.pc);
        self.pc += 1;
        opcode
    }

    fn read_address(&mut self, mode: Mode, tick: bool) -> u16 {
        let start_pc = self.pc;

        let address = match mode {
            Mode::Absolute => {
                let lo = self.bus.read(self.pc) as u16;
                let hi = self.bus.read(self.pc + 1) as u16;
                let address = (hi << 8) | lo;
                self.pc += 2;
                address
            },
            Mode::AbsoluteX => {
                let lo = self.bus.read(self.pc) as u16;
                let hi = self.bus.read(self.pc + 1) as u16;
                let address = ((hi << 8) | lo).wrapping_add(self.x as u16);
                if tick && address & 0xff00 != hi << 8 {
                    self.tick(1);
                }
                self.pc += 2;
                address
            },
            Mode::AbsoluteY => {
                let lo = self.bus.read(self.pc) as u16;
                let hi = self.bus.read(self.pc + 1) as u16;
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
                let operand = self.bus.read(self.pc);
                let address = ((self.bus.read(operand.wrapping_add(self.x + 1) as u16) as u16) << 8) | self.bus.read(operand.wrapping_add(self.x) as u16) as u16;
                self.pc += 1;
                address
            },
            Mode::Indirect => {
                let lo = self.bus.read(self.pc) as u16;
                let hi = self.bus.read(self.pc + 1) as u16;

                let mut address = (hi << 8) | lo;

                address = if lo == 0xff {
                    ((self.bus.read(address & 0xff00) as u16) << 8) | self.bus.read(address) as u16
                } else {
                    ((self.bus.read(address + 1) as u16) << 8) | self.bus.read(address) as u16
                };

                self.pc += 2;
                address
            },
            Mode::IndirectIndexed => {
                let operand = self.bus.read(self.pc) as u16;
                let lo = self.bus.read(operand) as u16;
                let hi = self.bus.read((operand + 1) % 256) as u16;
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
                let address = self.bus.read(self.pc) as u16;
                self.pc += 1;
                address
            },
            Mode::ZeroPageX => {
                let address = self.bus.read(self.pc).wrapping_add(self.x) as u16;
                self.pc += 1;
                address
            }
            Mode::ZeroPageY => {
                let address = self.bus.read(self.pc).wrapping_add(self.y) as u16;
                self.pc += 1;
                address
            }
        };

        let stop_pc = self.pc;

        if let Some(disassembler) = &mut self.disassembler {
            let mut bytes = Vec::new();

            for debug_address in start_pc - 1..stop_pc {
                bytes.push(self.bus.read(debug_address));
            }

            disassembler.load(bytes.as_slice());
        }

        address
    }

    fn read_operand(&mut self, mode: Mode) -> u8 {
        let address = self.read_address(mode, true);

        self.bus.read(address)
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
        self.bus.read(address)
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
        let operand = self.bus.read(address).wrapping_add(1);

        self.set_flag_if_negative(operand);
        self.set_flag_if_zero(operand);

        self.bus.write(address, operand);
    }

    fn dec(&mut self, mode: Mode) {
        let address = self.read_address(mode, true);
        let operand = self.bus.read(address).wrapping_sub(1);

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

        let address = self.bus.read(0xfffe) as u16 | (self.bus.read(0xffff) as u16) << 8;

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
            let mut operand = self.bus.read(address) as u16;

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
            let mut operand = self.bus.read(address) as u16;

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
            let operand = self.bus.read(address) as u16;

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
            let operand = self.bus.read(address);

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
        let operand = self.bus.read(address).wrapping_sub(1);
        self.bus.write(address, operand);

        self.compare(self.a, operand);
    }

    fn isc(&mut self, mode: Mode) {
        let address = self.read_address(mode, false);
        let mut operand = self.bus.read(address).wrapping_add(1) as u16;
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
        let mut operand = self.bus.read(address) as u16;

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
        let mut operand = self.bus.read(address) as u16;

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
        let mut operand = self.bus.read(address) as u16;

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
        let mut operand = self.bus.read(address) as u16;

        operand = (operand >> 1) | ((self.get_flag(Flag::CarryFlag) as u16) << 7);

        self.set_flag(Flag::CarryFlag, if self.bus.read(address) & 0x01 != 0 { 1 } else { 0 });

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
}

#[cfg(test)]
mod tests {
    // ...
}