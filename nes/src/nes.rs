use cpu::cpu::Cpu;
use cpu::ppu::Ppu;

pub struct Nes {
    cpu: Cpu,
    ppu: Ppu,
}

impl Nes {
    pub fn new() -> Nes {
        Nes {
            cpu: Cpu::new(),
            ppu: Ppu::new(),
        }
    }

    pub fn run(&mut self) {

    }
}