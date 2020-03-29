use std::rc::Rc;
use std::cell::RefCell;

use crate::ppu::Ppu;
use crate::cartridge::Cartridge;

pub struct Bus {
    ram: [u8; 0x10000],
    pub ppu: Ppu,
    pub nmi: bool,
    cartridge: Rc<RefCell<Cartridge>>,
    ppu_clock: u32,
}

impl Bus {
    pub fn new() -> Bus {
        // TODO: Move
        let cartridge = Rc::new(RefCell::new(Cartridge::new()));
        // cartridge.borrow_mut().load("nes\\testroms\\instr_test-v5\\rom_singles\\01-basics.nes");
        // cartridge.borrow_mut().load("nes\\testroms\\nestest.nes");
        // cartridge.borrow_mut().load("debug\\roms\\Balloon Fight (USA).nes");
        cartridge.borrow_mut().load("debug\\roms\\Donkey Kong (World) (Rev A).nes");

        let mut ppu = Ppu::new(cartridge.clone());
        ppu.init();

        Bus {
            ram: [0; 0x10000],
            ppu,
            nmi: false,
            cartridge,
            ppu_clock: 0,
        }
    }

    pub fn load(&mut self, data: &[u8]) {
        self.ram[0x8000..0xbff0].clone_from_slice(data);
        self.ram[0xc000..0xfff0].clone_from_slice(data);
    }

    pub fn read(&mut self, address: u16, debug: bool) -> u8 {
        match address {
            0x0000..=0x1fff => self.ram[(address & 0x07ff) as usize],
            0x2000..=0x3fff => self.ppu.read(address & 0x0007, debug),
            0x4000..=0x401f => 0x00,
            0x4020..=0xffff => self.cartridge.borrow_mut().prg_read(address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.ram[(address & 0x07ff) as usize] = value,
            0x2000..=0x3fff => self.ppu.write(address & 0x0007, value),
            0x4000..=0x401f => (),
            0x4020..=0xffff => {
                self.cartridge.borrow_mut().prg_write(address, value);
            },
        }
    }

    pub fn step(&mut self, delta_cpu_clock: u32) -> u32 {
        for _ in 0..delta_cpu_clock * 3 {
            self.ppu_clock = self.ppu.step();

            if self.ppu.nmi {
                self.ppu.nmi = false;
                self.nmi = true;
            }
        }

        self.ppu_clock
   }
}