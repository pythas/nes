use crate::ppu::Ppu;
use crate::cartridge::Cartridge;

pub struct Bus {
    ram: [u8; 0x10000],
    ppu: Ppu,
    cartridge: Cartridge,
    ppu_clock: u32,
}

impl Bus {
    pub fn new() -> Bus {
        let mut cartridge = Cartridge::new();
        cartridge.load("debug\\roms\\Balloon Fight (USA).nes");

        Bus {
            ram: [0; 0x10000],
            ppu: Ppu::new(),
            cartridge,
            ppu_clock: 0,
        }
    }

    pub fn load(&mut self, data: &[u8]) {
        self.ram[0x8000..0xbff0].clone_from_slice(data);
        self.ram[0xc000..0xfff0].clone_from_slice(data);
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => self.ram[(address & 0x07ff) as usize],
            0x2000..=0x3fff => self.ppu.read(address & 0x0007),
            0x420..=0xffff => self.cartridge.prg_read(address),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1fff => self.ram[(address & 0x07ff) as usize] = value,
            0x2000..=0x3fff => self.ppu.write(address & 0x0007, value),
            0x420..=0xffff => self.cartridge.prg_write(address, value),
        }
    }

    pub fn step(&mut self, delta_cpu_clock: u32) -> u32 {
        for _ in 0..delta_cpu_clock * 3 {
            self.ppu_clock = self.ppu.step();
        }

        // println!("PPU: {}", self.ppu_clock);
        self.ppu_clock
   }
}