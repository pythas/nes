pub struct Ppu {
    clock: u32,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            clock: 0,
        }
    }

    pub fn step(&mut self) -> u32 {
        self.tick(1);

        self.clock
    }

    fn tick(&mut self, cycles: u8) {
        self.clock += cycles as u32;
    }
}