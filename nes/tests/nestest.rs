use std::io::{BufReader};
use std::io::prelude::*;
use std::fs::File;

use nes::nes::Nes;
use nes::cpu::Cpu;

// TODO:
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\01-basics.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\02-implied.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\03-immediate.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\04-zero_page.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\05-zp_xy.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\06-absolute.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\07-abs_xy.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\08-ind_x.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\09-ind_y.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\10-branches.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\11-stack.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\12-jmp_jsr.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\13-rts.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\14-rti.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\15-brk.nes");
// nes.insert_cartridge("nes\\testroms\\instr_test-v5\\rom_singles\\16-special.nes");

#[test]
fn nestest_instructions() {
    let log = File::open("testroms\\nestest.log").expect("Could not open nestest.log");
    let mut log = BufReader::new(log);
    let mut log_buffer = String::new();

    let cpu = Cpu::new();
    let mut nes = Nes::new(cpu);

    nes.insert_cartridge("testroms\\nestest.nes");
    nes.cpu.reset();
    nes.cpu.pc = 0xc000;
    nes.cpu.debug();

    loop {
        nes.step();

        let state = nes.cpu.debug_state().unwrap();
        let instruction = format!("{:10}", format!("{:02X?}", state.instruction).replace("[", "").replace("]", "").replace(",", ""));
        let debug_str = format!("{:48} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU: {:3}  X CYC:XXX", format!("{:04X}  {}{}", state.pc, instruction, "---"), state.a, state.x, state.y, state.p, state.sp, "X");

        let num_bytes = log.read_line(&mut log_buffer).expect("Could not read line");

        if num_bytes > 0 {
            assert_eq!(log_buffer.split_at(16).0, debug_str.split_at(16).0, "PC: {:02x}", state.pc);

            assert_eq!(log_buffer.split_at(log_buffer.find("A:").unwrap()).1.split_at(25).0, debug_str.split_at(debug_str.find("A:").unwrap()).1.split_at(25).0, "PC: {:02x}", state.pc);
        } else {
            break;
        }

        log_buffer.clear();
    }
}

#[test]
fn nestest_clock() {
    let log = File::open("testroms\\nestest.log").expect("Could not open nestest.log");
    let mut log = BufReader::new(log);
    let mut log_buffer = String::new();

    let cpu = Cpu::new();
    let mut nes = Nes::new(cpu);

    nes.insert_cartridge("testroms\\nestest.nes");
    nes.cpu.reset();
    nes.cpu.pc = 0xc000;
    nes.cpu.debug();

    loop {
        nes.step();

        let state = nes.cpu.debug_state().unwrap();
        let instruction = format!("{:10}", format!("{:02X?}", state.instruction).replace("[", "").replace("]", "").replace(",", ""));
        let debug_str = format!("{:48} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:3}, X CYC:{}", format!("{:04X}  {}{}", state.pc, instruction, "---"), state.a, state.x, state.y, state.p, state.sp, "X", state.clock);

        let num_bytes = log.read_line(&mut log_buffer).expect("Could not read line");

        if num_bytes > 0 {
            assert_eq!(log_buffer.replace("\r\n", "").split_at(86).1, debug_str.split_at(86).1, "PC: {:02x}", state.pc);
        } else {
            break;
        }

        log_buffer.clear();
    }
}