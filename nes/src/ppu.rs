bitfield!{
    struct Control(u8);
    nametable,   _: 1, 0;
    increment,   _:    2;
    sprite,      _:    3;
    background,  _:    4;
    sprite_size, _:    5;
    slave,       _:    6;
    nmi,         _:    7;
    get,             _: 7, 0;
}

bitfield!{
    struct Mask(u8);
    grayscale,       _:    0;
    background_left, _:    1;
    sprite_left,     _:    2;
    background,      _:    3;
    sprites,         _:    4;
    emphasis_red,    _:    5;
    emphasis_green,  _:    6;
    emphasis_blue,   _:    7;
    get,             _: 7, 0;
}

bitfield!{
    struct Status(u8);
    sprite_overflow, set_sprite_overflow:    5;
    sprite_zero_hit, set_sprite_zero_hit:    6;
    vertical_blank,  set_vertical_blank:     7;
    get,             _:                   7, 0;
}

pub struct Ppu {
    clock: u32,
    control: Control,
    mask: Mask,
    status: Status,
    nmi: bool,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            clock: 0,
            control: Control(0x00),
            mask: Mask(0x00),
            status: Status(0xff),
            nmi: false,
        }
    }

    pub fn read(&mut self, address: u16) -> u8 {
        println!("PPU READ {:04x}", address);

        match address % 8 {
            0x0000 => unimplemented!(),
            0x0001 => unimplemented!(),
            0x0002 => {
                // TODO: add sprite overflow
                self.status.set_vertical_blank(false);
                self.status.get()
            },
            _ => panic!("Invalid PPU register read"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        // println!("PPU WRITE {:04x} {:x}", address, value);

        // TODO: Ingore for 29 658 cycles after reset

        match address {
            0x0000 => {
                let control = Control(value);

                if self.status.vertical_blank() && !self.control.nmi() && control.nmi() {
                    self.nmi = true;
                }

                self.control = control;
            },
            0x0001 => {
                let mask = Mask(value);

                self.mask = mask;
            },
            0x0000 => unimplemented!(),
            _ => panic!("Invalid PPU register write"),
        }
    }

    pub fn step(&mut self) -> u32 {
        self.tick(1);

        self.clock
    }

    fn tick(&mut self, cycles: u8) {
        self.clock += cycles as u32;
    }
}