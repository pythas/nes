#![allow(non_camel_case_types)]

pub trait Mapper {
    fn prg_read_address(&self, address: u16) -> Option<u16>;
    fn prg_write_address(&self, address: u16) -> Option<u16>;
    fn chr_read_address(&self, address: u16) -> Option<u16>;
    fn chr_write_address(&self, address: u16) -> Option<u16>;
}

pub struct iNES_000 {
    pub prg_banks: u8,
    pub chr_banks: u8,
}

impl Mapper for iNES_000 {
    fn prg_read_address(&self, address: u16) -> Option<u16> {
        match address {
            0x8000..=0xffff => {
                if self.prg_banks > 1 {
                    Some(address & 0x7fff)
                } else {
                    Some(address & 0x3fff)
                }
            }
            _ => None,
        }
    }

    fn prg_write_address(&self, address: u16) -> Option<u16> {
        match address {
            0x8000..=0xffff => {
                if self.prg_banks > 1 {
                    Some(address & 0x7fff)
                } else {
                    Some(address & 0x3fff)
                }
            }
            _ => None,
        }
    }

    fn chr_read_address(&self, address: u16) -> Option<u16> {
        match address {
            0x0000..=0x1fff => {
                Some(address)
            }
            _ => None,
        }
    }

    fn chr_write_address(&self, address: u16) -> Option<u16> {
        match address {
            0x0000..=0x1fff => {
                if self.chr_banks == 0 {
                    Some(address)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
