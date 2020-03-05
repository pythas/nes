use mos6502::bus::Bus;
use mos6502::cpu::Cpu;
use mos6502::disassembler::Disassembler;

fn main() {
    let mut cpu = Cpu::new(Bus::new(), Disassembler::new());

    cpu.load("debug\\roms\\nestest.nes");
    cpu.pc = 0xc000;

    loop {
        cpu.clock();
    }
}
