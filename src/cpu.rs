/**
 * TODO: set_flag_if_x should also branch and set to 0 if false...
 *
 */

use std::fs;
use std::io::{stdout, Write};

use crossterm::{
    cursor,
    terminal::{self, ClearType},
    execute,
    style::{Color, Print, ResetColor, SetBackgroundColor},
};

use crate::Bus;

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

pub struct Cpu {
    pub bus: Bus,
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    pub halt: bool,
    opcode: u8,
}

impl Cpu {
    pub fn new(bus: Bus) -> Cpu {
        Cpu {
            bus,
            pc: 0x400,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            p: 0x20,
            halt: false,
            opcode: 0,
        }
    }

    pub fn load(&mut self, path: &str) {
        let buffer = fs::read(path).expect("Could not read ROM.");

        self.bus.load(&buffer[..]);
    }

    pub fn clock(&mut self) {
        if self.halt {
            return;
        }

        self.step();
    }

    pub fn step(&mut self) {
        let opcode = self.bus.read(self.pc);
        self.opcode = opcode;

        let debug_pc = self.pc;

        match opcode {
            0x18 => self.clc(),
            0xd8 => self.cld(),
            0x58 => self.cli(),
            0xb8 => self.clv(),
            0xca => self.dex(),
            0x9a => self.txs(),
            0xf0 => self.beq(Mode::Relative),
            0x90 => self.bcc(Mode::Relative),
            0xb0 => self.bcs(Mode::Relative),
            0xd0 => self.bne(Mode::Relative),
            0x30 => self.bmi(Mode::Relative),
            0x10 => self.bpl(Mode::Relative),
            0x50 => self.bvc(Mode::Relative),
            0x70 => self.bvs(Mode::Relative),
            0x88 => self.dey(),
            0xa8 => self.tay(),
            0x98 => self.tya(),
            0xaa => self.tax(),
            0x8a => self.txa(),
            0xba => self.tsx(),
            0xea => self.nop(),
            0x48 => self.pha(),
            0x08 => self.php(),
            0x68 => self.pla(),
            0x28 => self.plp(),
            0xe8 => self.inx(),
            0xc8 => self.iny(),
            0x00 => self.brk(),
            0x40 => self.rti(),
            0x38 => self.sec(),
            0x78 => self.sei(),
            0xf8 => self.sed(),

            0xe6 => self.inc(Mode::ZeroPage),
            0xf6 => self.inc(Mode::ZeroPageX),
            0xee => self.inc(Mode::Absolute),
            0xfe => self.inc(Mode::AbsoluteX),

            // ASL
            0x0a => self.asl(Mode::Accumulator),
            0x06 => self.asl(Mode::ZeroPage),
            0x16 => self.asl(Mode::ZeroPageX),
            0x0e => self.asl(Mode::Absolute),
            0x1e => self.asl(Mode::AbsoluteX),

            // LSR
            0x4a => self.lsr(Mode::Accumulator),
            0x46 => self.lsr(Mode::ZeroPage),
            0x56 => self.lsr(Mode::ZeroPageX),
            0x4e => self.lsr(Mode::Absolute),
            0x5e => self.lsr(Mode::AbsoluteX),

            // ROL
            0x2a => self.rol(Mode::Accumulator),
            0x26 => self.rol(Mode::ZeroPage),
            0x36 => self.rol(Mode::ZeroPageX),
            0x2e => self.rol(Mode::Absolute),
            0x3e => self.rol(Mode::AbsoluteX),

            // ROR
            0x6a => self.ror(Mode::Accumulator),
            0x66 => self.ror(Mode::ZeroPage),
            0x76 => self.ror(Mode::ZeroPageX),
            0x6e => self.ror(Mode::Absolute),
            0x7e => self.ror(Mode::AbsoluteX),

            // BIT
            0x24 => self.bit(Mode::ZeroPage),
            0x2c => self.bit(Mode::Absolute),

            // ADC
            0x69 => self.adc(Mode::Immediate),
            0x65 => self.adc(Mode::ZeroPage),
            0x75 => self.adc(Mode::ZeroPageX),
            0x6d => self.adc(Mode::Absolute),
            0x7d => self.adc(Mode::AbsoluteX),
            0x79 => self.adc(Mode::AbsoluteY),
            0x61 => self.adc(Mode::IndexedIndirect),
            0x71 => self.adc(Mode::IndirectIndexed),

            // EOR
            0x49 => self.eor(Mode::Immediate),
            0x45 => self.eor(Mode::ZeroPage),
            0x55 => self.eor(Mode::ZeroPageX),
            0x4d => self.eor(Mode::Absolute),
            0x5d => self.eor(Mode::AbsoluteX),
            0x59 => self.eor(Mode::AbsoluteY),
            0x41 => self.eor(Mode::IndexedIndirect),
            0x51 => self.eor(Mode::IndirectIndexed),

            // ORA
            0x09 => self.ora(Mode::Immediate),
            0x05 => self.ora(Mode::ZeroPage),
            0x15 => self.ora(Mode::ZeroPageX),
            0x0d => self.ora(Mode::Absolute),
            0x1d => self.ora(Mode::AbsoluteX),
            0x19 => self.ora(Mode::AbsoluteY),
            0x01 => self.ora(Mode::IndexedIndirect),
            0x11 => self.ora(Mode::IndirectIndexed),

            // LDA
            0xa9 => self.lda(Mode::Immediate),
            0xa5 => self.lda(Mode::ZeroPage),
            0xb5 => self.lda(Mode::ZeroPageX),
            0xad => self.lda(Mode::Absolute),
            0xbd => self.lda(Mode::AbsoluteX),
            0xb9 => self.lda(Mode::AbsoluteY),
            0xa1 => self.lda(Mode::IndexedIndirect),
            0xb1 => self.lda(Mode::IndirectIndexed),

            // LDX
            0xa2 => self.ldx(Mode::Immediate),
            0xa6 => self.ldx(Mode::ZeroPage),
            0xb6 => self.ldx(Mode::ZeroPageY),
            0xae => self.ldx(Mode::Absolute),
            0xbe => self.ldx(Mode::AbsoluteY),

            // LDY
            0xa0 => self.ldy(Mode::Immediate),
            0xa4 => self.ldy(Mode::ZeroPage),
            0xb4 => self.ldy(Mode::ZeroPageX),
            0xac => self.ldy(Mode::Absolute),
            0xbc => self.ldy(Mode::AbsoluteX),

            // CMP
            0xc9 => self.cmp(Mode::Immediate),
            0xc5 => self.cmp(Mode::ZeroPage),
            0xd5 => self.cmp(Mode::ZeroPageX),
            0xcd => self.cmp(Mode::Absolute),
            0xdd => self.cmp(Mode::AbsoluteX),
            0xd9 => self.cmp(Mode::AbsoluteY),
            0xc1 => self.cmp(Mode::IndexedIndirect),
            0xd1 => self.cmp(Mode::IndirectIndexed),

            // CPX
            0xe0 => self.cpx(Mode::Immediate),
            0xe4 => self.cpx(Mode::ZeroPage),
            0xec => self.cpx(Mode::Absolute),

            // CPY
            0xc0 => self.cpy(Mode::Immediate),
            0xc4 => self.cpy(Mode::ZeroPage),
            0xcc => self.cpy(Mode::Absolute),

            // JMP
            0x4c => self.jmp(Mode::Absolute),
            0x6c => self.jmp(Mode::Indirect),

            // JSR
            0x20 => self.jsr(Mode::Absolute),

            // RTS
            0x60 => self.rts(),

            // STA
            0x85 => self.sta(Mode::ZeroPage),
            0x95 => self.sta(Mode::ZeroPageX),
            0x8d => self.sta(Mode::Absolute),
            0x9d => self.sta(Mode::AbsoluteX),
            0x99 => self.sta(Mode::AbsoluteY),
            0x81 => self.sta(Mode::IndexedIndirect),
            0x91 => self.sta(Mode::IndirectIndexed),

            // STX
            0x86 => self.stx(Mode::ZeroPage),
            0x96 => self.stx(Mode::ZeroPageY),
            0x8e => self.stx(Mode::Absolute),

            // STY
            0x84 => self.sty(Mode::ZeroPage),
            0x94 => self.sty(Mode::ZeroPageX),
            0x8c => self.sty(Mode::Absolute),

            _ => panic!("opcode {:x} not implemented at {:x}.", opcode, self.pc),
        }

        // Debug
        if self.halt {
            execute!(
                stdout(),
                terminal::Clear(ClearType::All),
                cursor::Hide,
                cursor::MoveTo(80, 0),
                Print("REGISTERS"),
                cursor::MoveTo(80, 1),
                Print(format!(" A: {:08b} {:02x}", self.a, self.a)),
                cursor::MoveTo(80, 2),
                Print(format!(" X: {:08b} {:02x}", self.x, self.x)),
                cursor::MoveTo(80, 3),
                Print(format!(" Y: {:08b} {:02x}", self.y, self.y)),
                cursor::MoveTo(80, 4),
                Print(format!("SP: {:08b} {:02x}", self.sp, self.sp)),

                cursor::MoveTo(80, 6),
                Print(format!("STATUS: {:08b}", self.p)),
                cursor::MoveTo(80, 7),
                Print("        NO BDIZC"),

                cursor::MoveTo(80, 9),
                Print(format!("OPCODE: {:x}", opcode)),

                cursor::MoveTo(80, 11),
                Print(format!("PC: {:x}", self.pc)),
            ).expect("Could not print.");

            let mut col = 0;
            let mut row = 0;
            let start = 0x2a50;
            let stop = 0x2b50;

            for i in start..stop {
                if i == debug_pc {
                    execute!(
                        stdout(),
                        SetBackgroundColor(Color::Red),
                    ).expect("Could not print.");
                }

                execute!(
                    stdout(),
                    cursor::MoveTo(col, row),
                    Print(format!("{:02x}", self.bus.read(i))),
                    ResetColor,
                ).expect("Could not print.");

                if (i - (start - 1)) % 16 == 0 {
                    row += 1;
                    col = 0;
                } else {
                    col += 3;
                }
            }

            execute!(
                stdout(),
                cursor::MoveTo(80, 19),
                Print("STACK"),
                ResetColor,
            ).expect("Could not print.");

            for i in 0x1f0..0x1ff {
                execute!(
                    stdout(),
                    cursor::MoveTo(80, 20 + (i - 0x1f0)),
                    Print(format!("{:02x}", self.bus.read(i))),
                ).expect("Could not print.");
            }
        }

        if self.pc == 0x2a51 {
            self.halt = true;
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

    fn read_address(&mut self, mode: Mode) -> u16 {
        match mode {
            Mode::Absolute => {
                let address = ((self.bus.read(self.pc + 2) as u16) << 8) | self.bus.read(self.pc + 1) as u16;
                self.pc += 3;
                address
            },
            Mode::AbsoluteX => {
                let address = (((self.bus.read(self.pc + 2) as u16) << 8) | self.bus.read(self.pc + 1) as u16) + self.x as u16;
                self.pc += 3;
                address
            },
            Mode::AbsoluteY => {
                let address = (((self.bus.read(self.pc + 2) as u16) << 8) | self.bus.read(self.pc + 1) as u16) + self.y as u16;
                self.pc += 3;
                address
            },
            Mode::Accumulator => {
                0x0000
            },
            Mode::Immediate => {
                let address = self.pc + 1;
                self.pc += 2;
                address
            },
            Mode::Implied => panic!("Implied not implemented."),
            Mode::IndexedIndirect => {
                let operand = self.bus.read(self.pc + 1);
                let address = ((self.bus.read(operand.wrapping_add(self.x + 1) as u16) as u16) << 8) | self.bus.read(operand.wrapping_add(self.x) as u16) as u16;
                self.pc += 2;
                address
            },
            Mode::Indirect => {
                let mut address = ((self.bus.read(self.pc + 2) as u16) << 8) | self.bus.read(self.pc + 1) as u16;
                address = ((self.bus.read(address + 1) as u16) << 8) | self.bus.read(address) as u16;
                self.pc += 3;
                address
            },
            Mode::IndirectIndexed => {
                let operand = self.bus.read(self.pc + 1) as u16;
                let address = (((self.bus.read(operand + 1) as u16) << 8) | self.bus.read(operand) as u16) + self.y as u16;
                self.pc += 2;
                address
            },
            Mode::Relative => {
                let address = self.pc + 1;
                self.pc += 2;
                address
            },
            Mode::ZeroPage => {
                let address = self.bus.read(self.pc + 1) as u16;
                self.pc += 2;
                address
            },
            Mode::ZeroPageX => {
                let address = self.bus.read(self.pc + 1).wrapping_add(self.x) as u16;
                self.pc += 2;
                address
            }
            Mode::ZeroPageY => {
                let address = self.bus.read(self.pc + 1).wrapping_add(self.y) as u16;
                self.pc += 2;
                address
            }
        }
    }

    fn read_operand(&mut self, mode: Mode) -> u8 {
        let address = self.read_address(mode);

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
            self.pc = pc;
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
    fn clc(&mut self) {
        self.set_flag(Flag::CarryFlag, 0);

        self.pc += 1;
    }

    fn cld(&mut self) {
        self.set_flag(Flag::DecimalMode, 0);

        self.pc += 1;
    }

    fn cli(&mut self) {
        self.set_flag(Flag::InterruptDisable, 0);

        self.pc += 1;
    }

    fn clv(&mut self) {
        self.set_flag(Flag::OverflowFlag, 0);

        self.pc += 1;
    }

    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
        self.pc += 1;
    }

    fn txs(&mut self) {
        self.pc += 1;

        self.sp = self.x;
    }

    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.set_flag_if_negative(self.y);
        self.set_flag_if_zero(self.y);
        self.pc += 1;
    }

    fn tay(&mut self) {
        self.y = self.a;
        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
        self.pc += 1;
    }

    fn tya(&mut self) {
        self.a = self.y;
        self.set_flag_if_negative(self.y);
        self.set_flag_if_zero(self.y);
        self.pc += 1;
    }

    fn tax(&mut self) {
        self.x = self.a;
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
        self.pc += 1;
    }

    fn txa(&mut self) {
        self.a = self.x;
        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
        self.pc += 1;
    }

    fn tsx(&mut self) {
        self.x = self.sp;
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
        self.pc += 1;
    }

    fn nop(&mut self) {
        self.pc += 1;
    }

    fn pha(&mut self) {
        self.push(self.a);
        self.pc += 1;
    }

    fn php(&mut self) {
        self.set_flag(Flag::BreakCommand, 1);
        self.push(self.p);
        self.pc += 1;
    }

    fn pla(&mut self) {
        self.a = self.pull();
        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);
        self.pc += 1;
    }

    fn plp(&mut self) {
        self.p = self.pull();
        self.set_flag(Flag::Unused, 1);
        self.pc += 1;
    }

    fn inc(&mut self, mode: Mode) {
        let address = self.read_address(mode);
        let operand = self.bus.read(address).wrapping_add(1);

        self.set_flag_if_negative(operand);
        self.set_flag_if_zero(operand);

        self.bus.write(address, operand);
    }

    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
        self.pc += 1;
    }

    fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.set_flag_if_negative(self.y);
        self.set_flag_if_zero(self.y);
        self.pc += 1;
    }

    fn sec(&mut self) {
        self.set_flag(Flag::CarryFlag, 1);

        self.pc += 1;
    }

    fn sei(&mut self) {
        self.set_flag(Flag::InterruptDisable, 1);

        self.pc += 1;
    }

    fn sed(&mut self) {
        self.set_flag(Flag::DecimalMode, 1);

        self.pc += 1;
    }

    fn brk(&mut self) {
        self.pc += 2;

        self.set_flag(Flag::BreakCommand, 1);

        let address = self.bus.read(0xfffe) as u16 | (self.bus.read(0xffff) as u16) << 8;

        self.push((self.pc >> 8) as u8);
        self.push(self.pc as u8);
        self.push(self.p);

        self.set_flag(Flag::InterruptDisable, 1);

        self.pc = address;
    }

    fn rti(&mut self) {
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
            self.pc += 1;

            let mut operand = self.a as u16;

            operand <<= 1;

            self.set_flag(Flag::CarryFlag, if operand > 0xff { 1 } else { 0 });
            self.set_flag(Flag::ZeroFlag, if operand & 0x00ff == 0x00 { 1 } else { 0 });
            self.set_flag(Flag::NegativeFlag, if operand & 0x80 > 0 { 1 } else { 0 });

            self.a = operand as u8;
        } else {
            let address = self.read_address(mode);
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
            self.pc += 1;

            let mut operand = self.a as u16;

            self.set_flag(Flag::CarryFlag, if operand & 1 > 0 { 1 } else { 0 });

            operand >>= 1;

            self.set_flag(Flag::ZeroFlag, if operand & 0x00ff == 0x00 { 1 } else { 0 });
            self.set_flag(Flag::NegativeFlag, if operand & 0x80 > 0 { 1 } else { 0 });

            self.a = operand as u8;
        } else {
            let address = self.read_address(mode);
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
            self.pc += 1;

            let operand = self.a as u16;

            let result = (operand << 1) | self.get_flag(Flag::CarryFlag) as u16;

            self.set_flag(Flag::CarryFlag, if result > 0xff { 1 } else { 0 });
            self.set_flag_if_zero(result as u8);
            self.set_flag_if_negative(result as u8);

            self.a = result as u8;
        } else {
            let address = self.read_address(mode);
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
            self.pc += 1;

            let operand = self.a;

            let result = ((operand as u16) >> 1) | ((self.get_flag(Flag::CarryFlag) as u16) << 7);

            self.set_flag(Flag::CarryFlag, if operand & 0x01 != 0 { 1 } else { 0 });
            self.set_flag_if_zero(result as u8);
            self.set_flag_if_negative(result as u8);

            self.a = result as u8;
        } else {
            let address = self.read_address(mode);
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

        let result = self.a as u16 + operand + self.get_flag(Flag::CarryFlag) as u16;
        let overflow = if (self.a as u16 ^ result) & (operand ^ result) & 0x80 == 0x80 { 1 } else { 0 };
        let carry = if result > 0xff { 1 } else { 0 };
        let zero = if result & 0x80 > 0  { 1 } else { 0 };

        self.set_flag(Flag::OverflowFlag, overflow);
        self.set_flag(Flag::CarryFlag, carry);
        self.set_flag(Flag::ZeroFlag, zero);

        self.a = (result & 0xff) as u8;
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
        let address = self.read_address(mode);

        if self.pc - 3 == address {
            panic!("JMP to self...");
        }

        self.pc = address;
    }

    fn jsr(&mut self, mode: Mode) {
        let target_address = self.read_address(mode);
        let return_address = self.pc - 1;

        self.push((return_address >> 8) as u8);
        self.push(return_address as u8);
        self.pc = target_address;
    }

    fn rts(&mut self) {
        let return_address = (self.pull() as u16) | (self.pull() as u16) << 8;
        self.pc = return_address + 1;
    }

    fn sta(&mut self, mode: Mode) {
        let address = self.read_address(mode);

        self.bus.write(address, self.a);
    }

    fn stx(&mut self, mode: Mode) {
        let address = self.read_address(mode);

        self.bus.write(address, self.x);
    }

    fn sty(&mut self, mode: Mode) {
        let address = self.read_address(mode);

        self.bus.write(address, self.y);
    }
}
