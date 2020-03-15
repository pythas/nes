use nes::nes::Nes;
use nes::cpu::Cpu;

fn main() {
    let mut cpu = Cpu::new();
    // cpu.load("nes\\testroms\\nestest.nes");
    // cpu.pc(0xc000);
    // cpu.sp(0xfd);
    // cpu.p(0x24);
    // cpu.clock(7);
    // cpu.debug();

    let mut nes = Nes::new(cpu);

    loop {
        nes.step();
    }
}
