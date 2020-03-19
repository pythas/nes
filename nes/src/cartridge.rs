use std::fs;

use crate::mapper::Mapper;
use crate::mapper::iNES_000;

#[allow(non_camel_case_types)]
enum Format {
    iNES,
    NES20,
    Unknown,
}

enum Mirroring {
    Horizontal,
    Vertical,
}

pub struct Cartridge {
    mapper: Option<Box<dyn Mapper>>,
    prg_banks: u8,
    chr_banks: u8,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}

impl Cartridge {
    pub fn new() -> Cartridge {
        Cartridge {
            mapper: None,
            prg_banks: 0,
            chr_banks: 0,
            prg_rom: Vec::new(),
            chr_rom: Vec::new(),
        }
    }

    pub fn load(&mut self, path: &str) {
        let rom = fs::read(path).expect("Could not read ROM.");
        let format = self.format(&rom);

        match format {
            Format::iNES => {
                self.load_ines(&rom);
            },
            Format::NES20 => {
            },
            Format::Unknown => panic!("Unknown file format."),
        }
    }

    fn load_ines(&mut self, rom: &Vec<u8>) {
        self.prg_banks = rom[4];
        self.chr_banks = rom[5];

        let prg_size = self.prg_banks as usize * 16384;
        let chr_size = self.chr_banks as usize * 8192;
        let flags_6 = rom[6];
        let _flags_7 = rom[7];
        let _flags_8 = rom[8];
        let _flags_9 = rom[9];
        let _flags_10 = rom[10];
        let mapper = flags_6 & 0xf0;
        let trainer = false;
        let _mirroring = Mirroring::Horizontal;

        if trainer {
            unimplemented!();
        } else {
            self.prg_rom = rom[0x10..prg_size + 0x10].to_vec();
            self.chr_rom = rom[0x10 + prg_size..0x010 + prg_size + chr_size].to_vec();
        }

        if mapper == 0b0000 {
            self.mapper = Some(Box::new(iNES_000 { prg_banks: self.prg_banks, chr_banks: self.chr_banks }));
        }
    }

    fn format(&self, rom: &Vec<u8>) -> Format {
        if rom[0] == 'N' as u8 && rom[1] == 'E' as u8 && rom[2] == 'S' as u8 && rom[3] == 0x1a {
            if rom[7] & 0xc0 == 0x08 {
                Format::NES20
            } else {
                Format::iNES
            }
        } else {
            Format::Unknown
        }
    }

    pub fn prg_read(&self, address: u16) -> u8 {
        match self.mapper.as_ref().unwrap().prg_read_address(address) {
            None => 0x00, // panic!("Invalid address."),
            Some(address) => self.prg_rom[address as usize],
        }
    }

    pub fn prg_write(&mut self, address: u16, byte: u8) {
        match self.mapper.as_ref().unwrap().prg_write_address(address) {
            None => panic!("Invalid address."),
            Some(address) => {
                self.prg_rom[address as usize] = byte;
            },
        };
    }

    pub fn chr_read(&self, address: u16) -> u8 {
        match self.mapper.as_ref().unwrap().chr_read_address(address) {
            None => panic!("Invalid address."),
            Some(address) => self.prg_rom[address as usize],
        }
    }

    pub fn chr_write(&mut self, address: u16, byte: u8) {
        match self.mapper.as_ref().unwrap().chr_write_address(address) {
            None => panic!("Invalid address."),
            Some(address) => {
                self.prg_rom[address as usize] = byte;
            },
        };
    }
}
