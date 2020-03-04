mod bus;
mod cpu;
mod disassembler;

use bus::Bus;
use cpu::Cpu;
use disassembler::Disassembler;

use std::time::Duration;

use crossterm::event::{poll, read, Event, KeyCode};

fn main() -> crossterm::Result<()> {
    let mut cpu = Cpu::new(Bus::new(), Disassembler::new());

    // cpu.load("roms\\6502_functional_test.bin");
    cpu.load("roms\\nestest.nes");
    cpu.pc = 0xc000;

    loop {
        if poll(Duration::from_micros(1))? {
            match read()? {
                Event::Key(event) => {
                    match event.code {
                        KeyCode::Enter => cpu.halt = !cpu.halt,
                        KeyCode::Char('s') => cpu.step(),
                        _ => (),
                    }
                },
                Event::Mouse(_) => (),
                Event::Resize(_, _) => (),
            }
        } else {
            cpu.clock();
        }
    }

    Ok(())
}
