pub struct Bus {
    ram: [u8; 0x10000],
}

impl Bus {
    pub fn new() -> Bus {
        Bus {
            ram: [0; 0x10000],
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.ram[address as usize] = value;
    }

    pub fn read(self, address: u16) -> u8 {
        self.ram[address as usize]
    }
}