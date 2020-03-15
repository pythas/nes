pub struct Ppu {
    clock: u32,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            clock: 0,
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        println!("PPU READ {:04x}", address);
        0x00
    }

    pub fn write(&mut self, address: u16, value: u8) {
        println!("PPU WRITE {:04x} {:x}", address, value);
    }

    pub fn step(&mut self) -> u32 {
        self.tick(1);

        self.clock
    }

    fn tick(&mut self, cycles: u8) {
        self.clock += cycles as u32;
    }
}