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

pub struct Palette {
    colors: Vec<Color>,
}

impl Palette {
    pub fn new() -> Palette {
        Palette {
            colors: vec!(
                Color(0x7f, 0x7f, 0x7f),
                Color(0x00, 0x00, 0xff),
                Color(0x00, 0x00, 0xbf),
                Color(0x47, 0x2b, 0xbf),
                Color(0x97, 0x00, 0x87),
                Color(0xab, 0x00, 0x23),
                Color(0xab, 0x13, 0x00),
                Color(0x8b, 0x17, 0x00),
                Color(0x53, 0x30, 0x00),
                Color(0x00, 0x78, 0x00),
                Color(0x00, 0x6b, 0x00),
                Color(0x00, 0x5b, 0x00),
                Color(0x00, 0x43, 0x58),
                Color(0x00, 0x00, 0x00),
                Color(0x00, 0x00, 0x00),
                Color(0x00, 0x00, 0x00),
                Color(0xbf, 0xbf, 0xbf),
                Color(0x00, 0x78, 0xf8),
                Color(0x00, 0x58, 0xf8),
                Color(0x6b, 0x47, 0xff),
                Color(0xdb, 0x00, 0xcd),
                Color(0xe7, 0x00, 0x5b),
                Color(0xf8, 0x38, 0x00),
                Color(0xe7, 0x5f, 0x13),
                Color(0xaf, 0x7f, 0x00),
                Color(0x00, 0xb8, 0x00),
                Color(0x00, 0xab, 0x00),
                Color(0x00, 0xab, 0x47),
                Color(0x00, 0x8b, 0x8b),
                Color(0x00, 0x00, 0x00),
                Color(0x00, 0x00, 0x00),
                Color(0x00, 0x00, 0x00),
                Color(0xf8, 0xf8, 0xf8),
                Color(0x3f, 0xbf, 0xff),
                Color(0x6b, 0x88, 0xff),
                Color(0x98, 0x78, 0xf8),
                Color(0xf8, 0x78, 0xf8),
                Color(0xf8, 0x58, 0x98),
                Color(0xf8, 0x78, 0x58),
                Color(0xff, 0xa3, 0x47),
                Color(0xf8, 0xb8, 0x00),
                Color(0xb8, 0xf8, 0x18),
                Color(0x5b, 0xdb, 0x57),
                Color(0x58, 0xf8, 0x98),
                Color(0x00, 0xeb, 0xdb),
                Color(0x78, 0x78, 0x78),
                Color(0x00, 0x00, 0x00),
                Color(0x00, 0x00, 0x00),
                Color(0xff, 0xff, 0xff),
                Color(0xa7, 0xe7, 0xff),
                Color(0xb8, 0xb8, 0xf8),
                Color(0xd8, 0xb8, 0xf8),
                Color(0xf8, 0xb8, 0xf8),
                Color(0xfb, 0xa7, 0xc3),
                Color(0xf0, 0xd0, 0xb0),
                Color(0xff, 0xe3, 0xab),
                Color(0xfb, 0xdb, 0x7b),
                Color(0xd8, 0xf8, 0x78),
                Color(0xb8, 0xf8, 0xb8),
                Color(0xb8, 0xf8, 0xd8),
                Color(0x00, 0xff, 0xff),
                Color(0xf8, 0xd8, 0xf8),
                Color(0x00, 0x00, 0x00),
                Color(0x00, 0x00, 0x00),
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

    pub palette: Palette,
    pub palette_table: [u8; 32],
    pattern_table: [[u8; 4096]; 2],
    name_table: [[u8; 1024]; 2],
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
            scanline: -1,
            latch: false,
            data_buffer: 0,
            address_buffer: 0,
            nmi: false,
            palette: Palette::new(),
            palette_table: [0; 32],
            pattern_table: [[0; 4096]; 2],
            name_table: [[0; 1024]; 2],
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

    pub fn palette_color(&self, palette: u8, index: u8) -> Color {
        let address = 0x3f00 + ((palette << 2) + index) as u16;
        self.palette.colors[self.internal_read(address) as usize & 0x3f]
    }

    pub fn debug_pixels(&self, bank: u16, palette: u16) -> Vec<Pixel> {
        let mut pixels = Vec::new();

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

                        let color = self.palette_color(palette as u8, pixel as u8);

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