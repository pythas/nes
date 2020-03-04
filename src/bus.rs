pub struct Bus {
    ram: [u8; 0x10000],
}

impl Bus {
    pub fn new() -> Bus {
        Bus {
            ram: [0; 0x10000],
        }
    }

    pub fn load(&mut self, data: &[u8]) {
        self.ram[0x8000..0xbff0].clone_from_slice(data);
        self.ram[0xc000..0xfff0].clone_from_slice(data);
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.ram[address as usize] = value;
    }

    pub fn read(&self, address: u16) -> u8 {
        self.ram[address as usize]
    }
}