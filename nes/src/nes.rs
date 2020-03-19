use std::{thread, time};

use crate::cpu::Cpu;
use crate::ppu::Ppu;

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

    pub fn step(&mut self) {
        let delta_cpu_clock = self.cpu.step() - self.cpu_clock;

        self.ppu_clock += self.cpu.bus.step(delta_cpu_clock);

        self.cpu_clock += delta_cpu_clock;

        // println!("CPU: {}", self.cpu_clock);

        // thread::sleep(time::Duration::from_millis(100));
    }
}