use std::{thread, time};

use crate::cpu::Cpu;
use crate::ppu::Ppu;

pub struct Nes {
    cpu: Cpu,
    ppu: Ppu,
    cpu_clock: u32,
    ppu_clock: u32,
}

impl Nes {
    pub fn new() -> Nes {
        let mut cpu = Cpu::new();
        cpu.load("cpu\\testroms\\nestest.nes");
        cpu.pc(0xc000);
        cpu.sp(0xfd);
        cpu.p(0x24);
        cpu.clock(7);
        cpu.debug();

        Nes {
            cpu,
            ppu: Ppu::new(),
            cpu_clock: 7,
            ppu_clock: 0,
        }
    }

    pub fn step(&mut self) {
         let delta_cpu_clock = self.cpu.step() - self.cpu_clock;

        for _ in 0..delta_cpu_clock * 3 {
            self.ppu_clock = self.ppu.step();
        }

        self.cpu_clock += delta_cpu_clock;

        println!("CPU: {}", self.cpu_clock);
        println!("PPU: {}", self.ppu_clock);

        thread::sleep(time::Duration::from_secs(1));
    }
}