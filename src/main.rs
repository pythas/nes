mod bus;
mod cpu;

use bus::Bus;
use cpu::Cpu;

use crossterm::event::{read, Event};

fn main() -> crossterm::Result<()> {
    let mut cpu = Cpu::new(Bus::new());

    cpu.load("roms\\6502_functional_test.bin");

    loop {
        match read()? {
            Event::Key(_) => cpu.step(),
            Event::Mouse(_) => (),
            Event::Resize(_, _) => (),
        }
    }

    Ok(())
}
