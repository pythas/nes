use nes::nes::Nes;

fn main() {
    let mut nes = Nes::new();

    loop {
        nes.run();
    }
}
