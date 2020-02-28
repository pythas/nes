mod bus;
mod cpu;

use bus::Bus;
use cpu::Cpu;

use std::time::Duration;

use crossterm::event::{poll, read, Event, KeyCode};

fn main() -> crossterm::Result<()> {
    let mut cpu = Cpu::new(Bus::new());

    cpu.load("roms\\6502_functional_test.bin");

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
