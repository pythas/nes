use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};

use crate::cartridge::Cartridge;

bitfield!{
    struct Control(u8);
    nametable,          _: 1, 0;
    increment,          _:    2;
    sprite,             _:    3;
    background,         _:    4;
    sprite_size,        _:    5;
    slave,              _:    6;
    nmi,                _:    7;
    get,                _: 7, 0;
}

bitfield!{
    struct Mask(u8);
    grayscale,          _:    0;
    background_left,    _:    1;
    sprite_left,        _:    2;
    background,         _:    3;
    sprites,            _:    4;
    emphasis_red,       _:    5;
    emphasis_green,     _:    6;
    emphasis_blue,      _:    7;
    get,                _: 7, 0;
}

bitfield!{
    struct Status(u8);
    sprite_overflow,    set_sprite_overflow:    5;
    sprite_zero_hit,    set_sprite_zero_hit:    6;
    vertical_blank,     set_vertical_blank:     7;
    get,                _:                   7, 0;
}

bitfield!{
    #[derive(Debug, Copy, Clone)]
    struct Address(u16);
    u8, coarse_x,       set_coarse_x:        4, 0;
    u8, coarse_y,       set_coarse_y:        9, 5;
    u8, nametable,      set_nametable:     11, 10;
    u8, fine_y,         set_fine_y:        14, 12;
    u16, get,           _:                  14, 0;
}

#[derive(Debug, Copy, Clone)]
pub struct Color(pub u8, pub u8, pub u8);

struct Palette {
    colors: Vec<Color>,
}

impl Palette {
    pub fn new() -> Palette {
        Palette {
            colors: vec!(
                Color(84, 84, 84),
                Color(0, 30, 116),
                Color(8, 16, 144),
                Color(48, 0, 136),
                Color(68, 0, 100),
                Color(92, 0, 48),
                Color(84, 4, 0),
                Color(60, 24, 0),
                Color(32, 42, 0),
                Color(8, 58, 0),
                Color(0, 64, 0),
                Color(0, 60, 0),
                Color(0, 50, 60),
                Color(0, 0, 0),
                Color(0, 0, 0),
                Color(0, 0, 0),

                Color(152, 150, 152),
                Color(8, 76, 196),
                Color(48, 50, 236),
                Color(92, 30, 228),
                Color(136, 20, 176),
                Color(160, 20, 100),
                Color(152, 34, 32),
                Color(120, 60, 0),
                Color(84, 90, 0),
                Color(40, 114, 0),
                Color(8, 124, 0),
                Color(0, 118, 40),
                Color(0, 102, 120),
                Color(0, 0, 0),
                Color(0, 0, 0),
                Color(0, 0, 0),

                Color(236, 238, 236),
                Color(76, 154, 236),
                Color(120, 124, 236),
                Color(176, 98, 236),
                Color(228, 84, 236),
                Color(236, 88, 180),
                Color(236, 106, 100),
                Color(212, 136, 32),
                Color(160, 170, 0),
                Color(116, 196, 0),
                Color(76, 208, 32),
                Color(56, 204, 108),
                Color(56, 180, 204),
                Color(60, 60, 60),
                Color(0, 0, 0),
                Color(0, 0, 0),

                Color(236, 238, 236),
                Color(168, 204, 236),
                Color(188, 188, 236),
                Color(212, 178, 236),
                Color(236, 174, 236),
                Color(236, 174, 212),
                Color(236, 180, 176),
                Color(228, 196, 144),
                Color(204, 210, 120),
                Color(180, 222, 120),
                Color(168, 226, 144),
                Color(152, 226, 180),
                Color(160, 214, 228),
                Color(160, 162, 160),
                Color(0, 0, 0),
                Color(0, 0, 0),
            ),
        }
    }
}

#[derive(Debug)]
pub struct Pixel {
    pub x: u16,
    pub y: u16,
    pub color: Color,
}

pub struct Ppu {
    cartridge: Rc<RefCell<Cartridge>>,
    pub clock: u32,
    control: Control,
    mask: Mask,
    status: Status,

    vram: Address,
    vram_temp: Address,
    fine_x: u8,

    pub scanline: i16,

    latch: bool,
    data_buffer: u8,
    address_buffer: u16,

    // pattern_table_lo: u16,
    // pattern_table_hi: u16,
    // palette_attribute_lo: u8,
    // palette_attribute_hi: u8,
    pub nmi: bool,

    palette: Palette,
    palette_table: [u8; 32],
    pattern_table: [[u8; 4096]; 2],
}

impl Ppu {
    pub fn new(cartridge: Rc<RefCell<Cartridge>>) -> Ppu {
        Ppu {
            cartridge,
            clock: 0,
            control: Control(0x00),
            mask: Mask(0x00),
            status: Status(0x00),
            vram: Address(0x00),
            vram_temp: Address(0x00),
            fine_x: 0,
            scanline: 240,
            latch: false,
            data_buffer: 0,
            address_buffer: 0,
            nmi: false,
            palette: Palette::new(),
            palette_table: [0; 32],
            pattern_table: [[0; 4096]; 2],
        }
    }

    pub fn read(&mut self, address: u16, debug: bool) -> u8 {
        // println!("PPU READ REGISTER {:04x}", address);

        match address % 8 {
            0x0000 => {
                0x00
            },
            0x0001 => {
                0x00
            },
            0x0002 => {
                let status = self.status.get() & 0xe0;
                // TODO: add sprite overflow

                if !debug {
                    self.status.set_vertical_blank(false);
                }

                status
            },
            0x0003 => {
                0x00
            },
            0x0004 => {
                0x00
            },
            0x0005 => {
                0x00
            },
            0x0006 => {
                0x00
            },
            0x0007 => {
                let mut byte = self.data_buffer;
                self.data_buffer = self.internal_read(self.vram.get());

                if address >= 0x3f00 {
                    byte = self.data_buffer;
                }

                self.vram = Address(self.vram.get() + if self.control.increment() { 32 } else { 1 });

                byte
            },
            _ => panic!("Invalid PPU register read"),
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        // println!("PPU WRITE REGISTER {:04x} {:x}", address, value);

        // TODO: Ingore for 29 658 cycles after reset

        match address {
            0x0000 => {
                self.control = Control(value);
                self.vram_temp.set_nametable(self.control.nametable());
                self.latch = false;
            },
            0x0001 => {
                self.mask = Mask(value);
            },
            0x0002 => unimplemented!(),
            0x0003 => {},
            0x0004 => unimplemented!(),
            0x0005 => {
                // if !self.latch {
                //     self.fine_x = value & 0x07;
                //     self.vram_temp.set_coarse_x(value >> 3);
                //     self.latch = true;
                // } else {
                //     self.vram_temp.set_fine_y(value & 0x07);
			    //     self.vram_temp.set_coarse_y(value >> 3);
                //     self.latch = false;
                // }
            }
            0x0006 => {
                if !self.latch {
                    self.vram_temp = Address((((value as u16 & 0x3f) << 8) | (self.vram_temp.get() & 0x00ff)) as u16);
                    self.latch = true;
                } else {
                    self.vram_temp = Address((self.vram_temp.get() & 0xff00) | value as u16);
			        self.vram = self.vram_temp;
                    self.latch = false;
                }
            },
            0x0007 => {

                self.internal_write(self.vram.get(), value);
                self.vram = Address(self.vram.get() + if self.control.increment() { 32 } else { 1 });
            },
            _ => panic!("Invalid PPU register write"),
        }
    }

    fn internal_read(&self, address: u16) -> u8 {
        // println!("PPU READ {:04x}", address);

        let address = address & 0x3fff;

        match self.cartridge.borrow().chr_read(address) {
            None => {
                match address {
                    0x0000..=0x1fff => {
                        // pattern
                        0x00
                    },
                    0x2000..=0x3eff => {
                        // table
                        0x00
                    },
                    0x3f00..=0x3fff => {
                        let address = address & 0x001F;
                        let address = match address {
                            0x0010 => 0x0000,
                            0x0014 => 0x0004,
                            0x0018 => 0x0008,
                            0x001c => 0x000c,
                            _ => address,
                        };

                        self.palette_table[address as usize] & if self.mask.grayscale() { 0x30 } else { 0xf3 }
                    }
                    _ => panic!("Invalid address {}", address),
                }
            }
            Some(byte) => byte,
        }
    }

    fn internal_write(&mut self, address: u16, value: u8) {
        // TODO: Cart write first

        match address {
            0x0000..=0x1fff => {
                self.pattern_table[((address & 0x1000) >> 12) as usize][(address & 0x0fff) as usize] = value;
            },
            0x2000..=0x3eff => {
            },
            0x3f00..=0x3fff => {
                let address = address & 0x001F;
                let address = match address {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001c => 0x000c,
                    _ => address,
                };

                self.palette_table[address as usize] = value;
            }
            _ => panic!("Invalid address {}", address),
        }
    }

    pub fn step(&mut self) -> u32 {
        if self.scanline >= -1 && self.scanline <= 239 {
            if self.scanline == 0 && self.clock == 0 {
                self.clock = 1;
            }

            if self.scanline == -1 && self.clock == 1 {
                self.status.set_vertical_blank(false);
            }
        }

        if self.scanline >= 241 && self.scanline <= 260 {
            if self.clock == 1 {
                self.status.set_vertical_blank(true);

                if self.control.nmi() {
                    self.nmi = true;
                }
            }
        }

        self.tick(1);

        if self.clock >= 341 {
            self.clock = 0;
            self.scanline += 1;

            if self.scanline >= 261 {
                self.scanline = -1;
            }
        }

        self.clock
    }

    fn tick(&mut self, cycles: u8) {
        self.clock += cycles as u32;
    }

    pub fn debug_pixels(&self, bank: u16, palette: u16) -> Vec<Pixel> {
        let mut pixels = Vec::new();

        let colors = vec!(
            Color(0, 0, 0),
            Color(50, 50, 50),
            Color(100, 100, 100),
            Color(150, 150, 150),
        );

        for y in 0..16 {
            for x in 0..16 {
                let offset = y * 256 + x * 16;

                for row in 0..8 {
                    let mut tile_lo = self.internal_read(bank * 0x1000 + offset + row);
                    let mut tile_hi = self.internal_read(bank * 0x1000 + offset + row + 8);

                    for column in 0..8 {
                        let pixel = ((tile_lo & 0x01) | ((tile_hi & 0x01)) << 1) as u16;

                        tile_lo >>= 1;
                        tile_hi >>= 1;

                        // let address = 0x3f00 + (palette << 2) + pixel;
                        // let color = self.palette.colors[self.internal_read(address) as usize & 0x3f];

                        let color = colors[pixel as usize];

                        pixels.push(Pixel {
                            x: x * 8 + (7 - column),
                            y: y * 8 + row,
                            color,
                        });
                    }
                }
            }
        }

        pixels
    }
}