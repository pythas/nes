use std::rc::Rc;
use std::cell::RefCell;

use crate::cpu::Cpu;
use crate::cartridge::Cartridge;

pub struct Nes {
    pub cpu: Cpu,
    cpu_clock: u32,
    ppu_clock: u32,
}

impl Nes {
    pub fn new(cpu: Cpu) -> Nes {
        Nes {
            cpu,
            cpu_clock: 7,
            ppu_clock: 0,
        }
    }

    pub fn insert_cartridge(&mut self, rom: &str) {
        let cartridge = Rc::new(RefCell::new(Cartridge::new()));

        cartridge.borrow_mut().load(rom);

        self.cpu.bus.insert_cartridge(cartridge);
    }

    pub fn step(&mut self) {
        let delta_cpu_clock = self.cpu.step() - self.cpu_clock;

        self.ppu_clock += self.cpu.bus.step(delta_cpu_clock);

        if self.cpu.bus.nmi {
            self.cpu.bus.nmi = false;
            self.cpu.nmi();
        }

        self.cpu_clock += delta_cpu_clock;
    }

    pub fn render_frame(&mut self) {
        self.cpu.bus.ppu.frame_ready = false;

        while !self.cpu.bus.ppu.frame_ready {
            self.step();
        }
    }
}