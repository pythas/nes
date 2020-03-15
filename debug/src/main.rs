use nes::nes::Nes;
use nes::cpu::Cpu;

fn main() {
    let mut cpu = Cpu::new();
    // cpu.load("nes\\testroms\\nestest.nes");
    // cpu.pc(0xc000);
    // cpu.sp(0xfd);
    // cpu.p(0x24);
    // cpu.clock(7);

    cpu.reset();
    cpu.debug();

    let mut nes = Nes::new(cpu);

    loop {
        nes.step();

        let state = nes.cpu.debug_state().unwrap();
        let instruction = format!("{:10}", format!("{:02X?}", state.instruction).replace("[", "").replace("]", "").replace(",", ""));
        let debug_str = format!("{:48} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU: {:3}  X CYC:XXX", format!("{:04X}  {}{}", state.pc, instruction, "---"), state.a, state.x, state.y, state.p, state.sp, "X");

        println!("{}", debug_str);
    }
}
