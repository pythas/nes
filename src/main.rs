mod bus;
mod cpu;
mod disassembler;

use bus::Bus;
use cpu::Cpu;
use disassembler::Disassembler;

fn main() {
    let mut cpu = Cpu::new(Bus::new(), Disassembler::new());

    cpu.load("roms\\nestest.nes");
    cpu.pc = 0xc000;

    loop {
        cpu.clock();
    }
}
