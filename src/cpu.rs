/**
 * TODO: set_flag_if_x should also branch and set to 0 if false...
 *
 * y = 1 OR 0bxxxxxxxx
 * get bit    = x & (y << n)
 * set bit    = x | (y << n)
 * unset bit  = x & !(y << n)
 * toggle bit = x ^ (y << n)
 *
 * unions för p?
 *
 * implement bus struct in module
 * - devices:
 *    - cpu
 *          write - use bus
 *          read - use bus
 *    - ram
 * - write addr: u16
 *      - if addr => 0 && addr <= 0xfff
 * - read addr: u16 -> u8
 */

use std::fs;
use std::num::Wrapping;
use std::io::{stdout, Write};

use crossterm::{
    cursor,
    terminal::{self, ClearType},
    execute,
    queue,
    style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor},
    ExecutableCommand, Result,
};

use crate::Bus;

enum Pc {
    Next(u16),
    Abort,
}

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
    OverflowFlag,
    NegativeFlag,
}

enum Register {
    A,
    X,
    Y,
}

pub struct Cpu {
    bus: Bus,
    memory: [u8; 0x10000],
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
}

impl Cpu {
    pub fn new(bus: Bus) -> Cpu {
        Cpu {
            bus,
            memory: [0; 0x10000],
            pc: 0x400,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            p: 0x00,
        }
    }

    pub fn set_bus(&mut self, bus: Bus) {

    }

    pub fn load(&mut self, path: &str) {
        let buffer = fs::read(path).expect("Could not read ROM.");

        self.memory[0..buffer.len()].clone_from_slice(&buffer[..]);

        // print!("{:?}", &self.memory[..]);
    }

    pub fn step(&mut self) {
        let opcode = self.memory[self.pc as usize];

        let prev_pc = self.pc;

        match opcode {
            0x18 => self.clc(),
            0xd8 => self.cld(),
            0x58 => self.cli(),
            0xb8 => self.clv(),
            0xca => self.dex(),
            0x9a => self.txs(),
            0xf0 => self.beq(Mode::Relative),
            0xd0 => self.bne(Mode::Relative),
            0x10 => self.bpl(Mode::Relative),
            0x88 => self.dey(),
            0x98 => self.tya(),
            0xaa => self.tax(),
            0xea => self.nop(),

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
            0xb4 => self.ldy(Mode::ZeroPageY),
            0xac => self.ldy(Mode::Absolute),
            0xbc => self.ldy(Mode::AbsoluteY),

            // CMP
            0xc9 => self.cmp(Mode::Immediate),
            0xc5 => self.cmp(Mode::ZeroPage),
            0xd5 => self.cmp(Mode::ZeroPageX),
            0xcd => self.cmp(Mode::Absolute),
            0xdd => self.cmp(Mode::AbsoluteX),
            0xd9 => self.cmp(Mode::AbsoluteY),
            0xc1 => self.cmp(Mode::IndexedIndirect),
            0xd1 => self.cmp(Mode::IndirectIndexed),

            // JMP
            0x4c => self.jmp(Mode::Absolute),
            0x6c => self.jmp(Mode::Indirect),

            // STA
            0x8d => self.sta(Mode::Absolute),

            _ => panic!("opcode {:x} not implemented.", opcode),
        }

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

        for i in 0x400..0x5F0 {
            if i == prev_pc {
                execute!(
                    stdout(),
                    SetBackgroundColor(Color::Red),
                ).expect("Could not print.");
            }

            execute!(
                stdout(),
                cursor::MoveTo(col, row),
                Print(format!("{:02x}", self.memory[i as usize])),
                ResetColor,
            ).expect("Could not print.");

            if (i - 0x3ff) % 16 == 0 {
                row += 1;
                col = 0;
            } else {
                col += 3;
            }
        }
    }

    fn set_flag(&mut self, flag: Flag, value: u8) {
        match flag {
            Flag::CarryFlag => self.p = (self.p & !(1 << 0)) | (value << 0),
            Flag::ZeroFlag => self.p = (self.p & !(1 << 1)) | (value << 1),
            Flag::InterruptDisable => self.p = (self.p & !(1 << 2)) | (value << 2),
            Flag::DecimalMode => self.p = (self.p & !(1 << 3)) | (value << 3),
            Flag::BreakCommand => self.p = (self.p & !(1 << 4)) | (value << 4),
            Flag::OverflowFlag => self.p = (self.p & !(1 << 6)) | (value << 6),
            Flag::NegativeFlag => self.p = (self.p & !(1 << 7)) | (value << 7),
        }
    }

    fn get_flag(&mut self, flag: Flag) -> u8 {
        match flag {
            Flag::CarryFlag => if self.p & (1 << 0) > 0 { 1 } else { 0 },
            Flag::ZeroFlag => if self.p & (1 << 1) > 0 { 1 } else { 0 },
            Flag::InterruptDisable => if self.p & (1 << 2) > 0 { 1 } else { 0 },
            Flag::DecimalMode => if self.p & (1 << 3) > 0 { 1 } else { 0 },
            Flag::BreakCommand => if self.p & (1 << 4) > 0 { 1 } else { 0 },
            Flag::OverflowFlag => if self.p & (1 << 6) > 0 { 1 } else { 0 },
            Flag::NegativeFlag => if self.p & (1 << 7) > 0 { 1 } else { 0 },
        }
    }

    fn get_address(&mut self, mode: Mode) -> u16 {
        match mode {
            Mode::Absolute => {
                let address = ((self.memory[self.pc as usize + 2] as u16) << 8) | self.memory[self.pc as usize + 1] as u16;
                self.pc += 3;
                address
            },
            Mode::AbsoluteX => panic!("AbsoluteX not implemented."),
            Mode::AbsoluteY => panic!("AbsoluteY not implemented."),
            Mode::Accumulator => panic!("Accumulator not implemented."),
            Mode::Immediate => {
                let address = self.pc + 1;
                self.pc += 2;
                address
            }
            Mode::Implied => panic!("Implied not implemented."),
            Mode::IndexedIndirect => panic!("IndexedIndirect not implemented."),
            Mode::Indirect => panic!("Indirect not implemented."),
            Mode::IndirectIndexed => panic!("IndirectIndexed not implemented."),
            Mode::Relative => {
                let offset = self.memory[self.pc as usize + 1] as u16;
                // println!("--- {:x}", self.memory[self.pc as usize + 1]);
                offset
            },
            Mode::ZeroPage => panic!("ZeroPage not implemented."),
            Mode::ZeroPageX => panic!("ZeroPageX not implemented."),
            Mode::ZeroPageY => panic!("ZeroPageY not implemented."),
        }
    }

    // fn read_byte(&mut self, register: Register) -> u8 {
    //     match register {
    //         Register::A => self.a,
    //         Register::X => self.x,
    //         Register::Y => self.y,
    //     }
    // }

    fn copy_byte(&mut self, register: Register, address: u16) {
        let byte = self.memory[address as usize];

        match register {
            Register::A => self.a = byte,
            Register::X => self.x = byte,
            Register::Y => self.y = byte,
        }

        self.set_flag_if_zero(byte);
        self.set_flag_if_negative(byte);
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

    fn push(&mut self) {

    }

    fn pop(&mut self) {

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
        self.x = (Wrapping(self.x) - Wrapping(1)).0;
        self.set_flag_if_negative(self.x);
        self.set_flag_if_zero(self.x);
        self.pc += 1;
    }

    fn txs(&mut self) {
        self.pc += 1;

        self.sp = self.x;
    }

    fn dey(&mut self) {
        self.y = (Wrapping(self.y) - Wrapping(1)).0;
        self.set_flag_if_negative(self.y);
        self.set_flag_if_zero(self.y);
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

    fn nop(&mut self) {
        self.pc += 1;
    }

    fn beq(&mut self, mode: Mode) {
        let offset = self.get_address(mode) as u8;

        if self.get_flag(Flag::ZeroFlag) == 1 {
            if offset & 0x80 > 0 {
                self.pc -= (!(offset as u8) + 1) as u16 - 2;
            } else {
                self.pc += offset as u16 + 2;
            }
        } else {
            self.pc += 2;
        }
    }

    fn bne(&mut self, mode: Mode) {
        let offset = self.get_address(mode) as u8;

        if self.get_flag(Flag::ZeroFlag) == 0 {
            // println!("bne");
            if offset & 0x80 > 0 {
                self.pc -= (!(offset as u8) + 1) as u16 - 2;
            } else {
                self.pc += offset as u16 + 2;
            }
        } else {
            self.pc += 2;
        }
    }

    fn bpl(&mut self, mode: Mode) {
        let offset = self.get_address(mode) as u8;

        if self.get_flag(Flag::NegativeFlag) == 0 {
            // println!("bpl");
            if offset & 0x80 > 0 {
                self.pc -= (!(offset as u8) + 1) as u16 - 2;
            } else {
                self.pc += offset as u16 + 2;
            }
        } else {
            self.pc += 2;
        }
    }

    fn adc(&mut self, mode: Mode) {
        let address = self.get_address(mode);
        let byte = self.memory[address as usize] as u16;

        let result = self.a as u16 + byte + self.get_flag(Flag::CarryFlag) as u16;
        let overflow = if (self.a as u16 ^ result) & (byte ^ result) & 0x80 == 0x80 { 1 } else { 0 };
        let carry = if result > 0xff { 1 } else { 0 };
        let zero = if result & 0x80 > 0  { 1 } else { 0 };

        self.set_flag(Flag::OverflowFlag, overflow);
        self.set_flag(Flag::CarryFlag, carry);
        self.set_flag(Flag::ZeroFlag, zero);

        self.a = (result & 0xff) as u8;
    }

    fn eor(&mut self, mode: Mode) {
        let address = self.get_address(mode);
        let byte = self.memory[address as usize];

        self.a = self.a ^ byte;

        self.set_flag_if_negative(self.a);
        self.set_flag_if_zero(self.a);

    }

    fn lda(&mut self, mode: Mode) {
        let address = self.get_address(mode);

        self.copy_byte(Register::A, address);
    }

    fn ldx(&mut self, mode: Mode) {
        let address = self.get_address(mode);

        self.copy_byte(Register::X, address);
    }

    fn ldy(&mut self, mode: Mode) {
        let address = self.get_address(mode);

        self.copy_byte(Register::Y, address);
    }

    fn cmp(&mut self, mode: Mode) {
        let address = self.get_address(mode);
        let byte = self.memory[address as usize];

        // println!("{:x}", byte);

        let result = (Wrapping(self.a) - Wrapping(byte)).0;

        self.set_flag_if_negative(result);

        if self.a == byte {
            self.set_flag(Flag::ZeroFlag, 1);
        } else {
            self.set_flag(Flag::ZeroFlag, 0);
        }

        if self.a >= byte {
            self.set_flag(Flag::CarryFlag, 1);
        } else {
            self.set_flag(Flag::CarryFlag, 0);
        }
    }

    fn jmp(&mut self, mode: Mode) {
        let address = self.get_address(mode);

        if self.pc - 3 == address {
            panic!("JMP to self...");
        }

        self.pc = address;
    }

    fn sta(&mut self, mode: Mode) {
        let address = self.get_address(mode);

        // println!("{:x} {:x} {:x}", self.a, self.pc, address);

        self.memory[address as usize] = self.a;
    }
}