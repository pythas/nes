use std::rc::Rc;
use std::cell::{RefCell};

use crate::cartridge::Cartridge;

const NUM_DOTS: usize = 341;
const NUM_SCANLINES: usize = 262;

// TODO: u32
const EVENT_OUTPUT: u16                    = 0b0000_0000_0000_0001;
const EVENT_SHIFT_BACKGROUND: u16          = 0b0000_0000_0000_0010;
const EVENT_SHIFT_SPRITE: u16              = 0b0000_0000_0000_0100;
const EVENT_RELOAD_BACKGROUND: u16         = 0b0000_0000_0000_1000;
const EVENT_FETCH_NAME_TABLE: u16          = 0b0000_0000_0001_0000;
const EVENT_FETCH_ATTRIBUTE_TABLE: u16     = 0b0000_0000_0010_0000;
const EVENT_FETCH_LOW_BACKGORUND: u16      = 0b0000_0000_0100_0000;
const EVENT_FETCH_HIGH_BACKGROUND: u16     = 0b0000_0000_1000_0000;
const EVENT_SET_VERTICAL_BLANK: u16        = 0b0000_0001_0000_0000;
const EVENT_CLEAR_VERTICAL_BLANK: u16      = 0b0000_0010_0000_0000;
const EVENT_INCREMENT_HORIZONTAL: u16      = 0b0000_0100_0000_0000;
const EVENT_INCREMENT_VERTICAL: u16        = 0b0000_1000_0000_0000;
const EVENT_SET_HORIZONTAL: u16            = 0b0001_0000_0000_0000;
const EVENT_SET_VERTICAL: u16              = 0b0010_0000_0000_0000;
const EVENT_SECONDARY_OAM_CLEAR: u16       = 0b0100_0000_0000_0000;
const EVENT_SPRITE_EVALUATION: u16         = 0b1000_0000_0000_0000;
// const EVENT_FETCH_SPRITE: u16              = 0b0001_0000_0000_0000_0000;

bitfield!{
    struct Control(u8);
    nametable_h,        _:    0;
    nametable_v,        _:    1;
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
    sprite,             _:    4;
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
    u8, nametable_h,    set_nametable_h:       10;
    u8, nametable_v,    set_nametable_v:       11;
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

pub struct Data {
    name_table: u8,
    attribute_table: u8,
    background_low: u8,
    background_high: u8,
    shift_background_low: u16,
    shift_background_high: u16,
    shift_attribute_low: u16,
    shift_attribute_high: u16,
}

impl Data {
    pub fn new() -> Data {
        Data {
            name_table: 0,
            attribute_table: 0,
            background_low: 0,
            background_high: 0,
            shift_background_low: 0,
            shift_background_high: 0,
            shift_attribute_low: 0,
            shift_attribute_high: 0,
        }
    }
}

pub struct Ppu {
    cartridge: Rc<RefCell<Cartridge>>,
    pub h: usize,
    pub v: usize,
    control: Control,
    mask: Mask,
    status: Status,

    vram: Address,
    vram_temp: Address,
    fine_x: u8,


    latch: bool,
    data_buffer: u8,


    pub nmi: bool,

    pub palette: Palette,
    pub palette_table: [u8; 32],
    pub pattern_table: [[u8; 4096]; 2],
    pub name_table: [[u8; 1024]; 4],

    pub pixels: Box<[u8; 256 * 240 * 3]>,
    // pub debug_pixels: Box<[u8; NUM_DOTS * NUM_SCANLINES * 3]>,

    events: Box<[[u16; NUM_DOTS]; NUM_SCANLINES]>,

    odd_frame: bool,

    data: Data,

    pub frame_ready: bool,
}

impl Ppu {
    pub fn new(cartridge: Rc<RefCell<Cartridge>>) -> Ppu {
        Ppu {
            cartridge,
            h: 0,
            v: 0,
            control: Control(0x00),
            mask: Mask(0x00),
            status: Status(0x00),
            vram: Address(0x00),
            vram_temp: Address(0x00),
            fine_x: 0,
            latch: false,
            data_buffer: 0,
            nmi: false,
            palette: Palette::new(),
            palette_table: [0; 32],
            pattern_table: [[0; 4096]; 2],
            name_table: [[0; 1024]; 4],

            pixels: Box::new([0; 256 * 240 * 3]),
            // debug_pixels: Box::new([0; NUM_DOTS * NUM_SCANLINES * 3]),

            events: Box::new([[0; NUM_DOTS]; NUM_SCANLINES]),

            odd_frame: false,

            data: Data::new(),

            frame_ready: false,
        }
    }

    pub fn read(&mut self, address: u16, debug: bool) -> u8 {
        match address % 8 {
            0x0000 => {
                0x00
            },
            0x0001 => {
                0x00
            },
            0x0002 => {
                let status = self.status.get() & 0xe0;

                if !debug {
                    self.status.set_vertical_blank(false);
                    self.latch = false;
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
        // TODO: Ingore for 29 658 cycles after reset

        match address {
            0x0000 => {
                self.control = Control(value);
                self.vram_temp.set_nametable_h(self.control.nametable_h());
                self.vram_temp.set_nametable_v(self.control.nametable_v());
            },
            0x0001 => {
                self.mask = Mask(value);
            },
            0x0002 => {},
            0x0003 => {},
            0x0004 => {},
            0x0005 => {
                if !self.latch {
                    self.fine_x = value & 0x07;
                    self.vram_temp.set_coarse_x(value >> 3);
                    self.latch = true;
                } else {
                    self.vram_temp.set_fine_y(value & 0x07);
			        self.vram_temp.set_coarse_y(value >> 3);
                    self.latch = false;
                }
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
        let address = address & 0x3fff;

        match self.cartridge.borrow().chr_read(address) {
            None => {
                match address {
                    0x0000..=0x1fff => {
                        self.pattern_table[((address & 0x1000) >> 12) as usize][(address & 0x0fff) as usize]
                    },
                    0x2000..=0x3eff => {
                        let address = address & 0x0fff;

                        match address {
                            0x0000..=0x03ff => {
                                self.name_table[0][(address & 0x03ff) as usize]
                            },
                            0x0400..=0x07ff => {
                                self.name_table[0][(address & 0x03ff) as usize]
                            },
                            0x0800..=0x0bff => {
                                self.name_table[1][(address & 0x03ff) as usize]
                            },
                            0x0c00..=0x0fff => {
                                self.name_table[1][(address & 0x03ff) as usize]
                            }
                            _ => 0x00,
                        }
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
        let address = address & 0x3fff;

        match self.cartridge.borrow_mut().chr_write(address, value) {
            None => {
                match address {
                    0x0000..=0x1fff => {
                        self.pattern_table[((address & 0x1000) >> 12) as usize][(address & 0x0fff) as usize] = value;
                    },
                    0x2000..=0x3eff => {
                        let address = address & 0x0fff;

                        match address {
                            0x0000..=0x03ff => {
                                self.name_table[0][(address & 0x03ff) as usize] = value;
                            },
                            0x0400..=0x07ff => {
                                self.name_table[0][(address & 0x03ff) as usize] = value;
                            },
                            0x0800..=0x0bff => {
                                self.name_table[1][(address & 0x03ff) as usize] = value;
                            },
                            0x0c00..=0x0fff => {
                                self.name_table[1][(address & 0x03ff) as usize] = value;
                            }
                            _ => (),
                        }
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
            Some(_) => (),
        }
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

    fn output(&mut self) {
        if !self.mask.background() {
            return;
        }

        let offset = 0x8000 >> self.fine_x;

        let pixel = ((((self.data.shift_background_high & offset) > 0) as u8) << 1) | ((self.data.shift_background_low & offset) > 0) as u8;

        let palette = ((((self.data.shift_attribute_high & offset) > 0) as u8) << 1) | ((self.data.shift_attribute_low & offset) > 0) as u8;

        let color = self.palette_color(palette as u8, pixel as u8);

        let x = self.h as u32 - 2;
        let y = self.v as u32;

        if x < 256 && y < 240 {
            self.pixels[((y * 256 + x) * 3) as usize] = color.0;
            self.pixels[((y * 256 + x) * 3 + 1) as usize] = color.1;
            self.pixels[((y * 256 + x) * 3 + 2) as usize] = color.2;
        }
    }

    fn shift_background(&mut self) {
        self.data.shift_background_low <<= 1;
        self.data.shift_background_high <<= 1;
        self.data.shift_attribute_low <<= 1;
        self.data.shift_attribute_high <<= 1;
    }

    fn shift_sprite(&mut self) {
    }

    fn reload_background(&mut self) {
        self.data.shift_background_low = (self.data.shift_background_low & 0xFF00) | self.data.background_low as u16;
        self.data.shift_background_high = (self.data.shift_background_high & 0xFF00) | self.data.background_high as u16;

        self.data.shift_attribute_low = (self.data.shift_attribute_low & 0xff00) | if self.data.attribute_table & 0x01 > 0 { 0xff } else { 0x00 };
        self.data.shift_attribute_high = (self.data.shift_attribute_high & 0xff00) | if self.data.attribute_table & 0x02 > 0 { 0xff } else { 0x00 };
    }

    fn fetch_name_table(&mut self) {
        if !self.mask.background() {
            return;
        }

        let address = 0x2000 | (self.vram.get() & 0x0fff);

        self.data.name_table = self.internal_read(address);
    }

    fn fetch_attribute_table(&mut self) {
        if !self.mask.background() {
            return;
        }

        let address = 0x23c0
            | ((self.vram.nametable_h() as u16) << 10)
            | ((self.vram.nametable_v() as u16) << 11)
            | (((self.vram.coarse_y() as u16) >> 2)) << 3
            | ((self.vram.coarse_x() as u16) >> 2);

        self.data.attribute_table = self.internal_read(address);

        if self.vram.coarse_y() & 0x02 > 0 {
            self.data.attribute_table >>= 4;
        }

        if self.vram.coarse_x() & 0x02 > 0 {
            self.data.attribute_table >>= 2;
        }

        self.data.attribute_table &= 0x03;
    }

    fn fetch_low_backgorund(&mut self) {
        if !self.mask.background() {
            return;
        }

        let address = self.control.background() as u16 * 0x1000
            + self.data.name_table as u16 * 16
            + self.vram.fine_y() as u16;

        self.data.background_low = self.internal_read(address);
    }

    fn fetch_high_background(&mut self) {
        if !self.mask.background() {
            return;
        }

        let address = self.control.background() as u16 * 0x1000
            + self.data.name_table as u16 * 16
            + self.vram.fine_y() as u16
            + 8;

        self.data.background_low = self.internal_read(address);
    }

    fn set_vertical_blank(&mut self) {
        self.status.set_vertical_blank(true);

        if self.control.nmi() {
            self.nmi = true;
        }
    }

    fn clear_vertical_blank(&mut self) {
        self.status.set_vertical_blank(false);
    }

    fn increment_horizontal(&mut self) {
        if !self.mask.background() {
            return;
        }

        self.vram.set_coarse_x(self.vram.coarse_x() + 1);

        if self.vram.coarse_x() == 31 {
            self.vram.set_coarse_x(0);
            self.vram.set_nametable_h(!self.vram.nametable_h());
        }
    }

    fn increment_vertical(&mut self) {
        if !self.mask.background() {
            return;
        }

        if self.vram.fine_y() < 7 {
            self.vram.set_fine_y(self.vram.fine_y() + 1);
        } else {
            self.vram.set_fine_y(0);

            match self.vram.coarse_y() {
                29 => {
                    self.vram.set_coarse_y(0);
                    self.vram.set_nametable_v(!self.vram.nametable_v());
                },
                31 => {
                    self.vram.set_coarse_y(0);
                },
                _ => {
                    self.vram.set_coarse_y(self.vram.coarse_y() + 1);
                },
            }
        }
    }

    fn set_horizontal(&mut self) {
        if !self.mask.background() {
            return;
        }

        self.vram.set_nametable_h(self.vram_temp.nametable_h());
        self.vram.set_coarse_x(self.vram_temp.coarse_x());
    }

    fn set_vertical(&mut self) {
        if !self.mask.background() {
            return;
        }

        self.vram.set_nametable_v(self.vram_temp.nametable_v());
        self.vram.set_coarse_y(self.vram_temp.coarse_y());
        self.vram.set_fine_y(self.vram_temp.fine_y());
    }

    fn secondary_oam_clear(&mut self) {
        if !self.mask.sprite() {
            return;
        }
    }

    fn sprite_evaluation(&mut self) {
        if !self.mask.sprite() {
            return;
        }
    }


    fn put_debug_pixel(&mut self, x: usize, y: usize, color: u32) {
        self.pixels[(y * NUM_DOTS + x) * 3 + 0] = ((color >> 16) & 0xff) as u8;
        self.pixels[(y * NUM_DOTS + x) * 3 + 1] = ((color >> 8) & 0xff) as u8;
        self.pixels[(y * NUM_DOTS + x) * 3 + 2] = (color & 0xff) as u8;
    }

    pub fn step(&mut self) -> u32 {
        // self.tick(1);

        let event = self.events[self.v][self.h];

        if event & EVENT_OUTPUT > 0 { self.output(); }
        if event & EVENT_SHIFT_BACKGROUND > 0 { self.shift_background(); }
        if event & EVENT_SHIFT_SPRITE > 0 { self.shift_sprite(); }
        if event & EVENT_RELOAD_BACKGROUND > 0 { self.reload_background(); }
        if event & EVENT_FETCH_NAME_TABLE > 0 { self.fetch_name_table(); }
        if event & EVENT_FETCH_ATTRIBUTE_TABLE > 0 { self.fetch_attribute_table(); }
        if event & EVENT_FETCH_LOW_BACKGORUND > 0 { self.fetch_low_backgorund(); }
        if event & EVENT_FETCH_HIGH_BACKGROUND > 0 { self.fetch_high_background(); }
        if event & EVENT_SET_VERTICAL_BLANK > 0 { self.set_vertical_blank(); }
        if event & EVENT_CLEAR_VERTICAL_BLANK > 0 { self.clear_vertical_blank(); }
        if event & EVENT_INCREMENT_HORIZONTAL > 0 { self.increment_horizontal(); }
        if event & EVENT_INCREMENT_VERTICAL > 0 { self.increment_vertical(); }
        if event & EVENT_SET_HORIZONTAL > 0 { self.set_horizontal(); }
        if event & EVENT_SET_VERTICAL > 0 { self.set_vertical(); }
        if event & EVENT_SECONDARY_OAM_CLEAR > 0 { self.secondary_oam_clear(); }
        if event & EVENT_SPRITE_EVALUATION > 0 { self.sprite_evaluation(); }

        self.h += 1;

        if self.h >= NUM_DOTS {
            self.h = 0;
            self.v += 1;

            if self.v >= NUM_SCANLINES {
                self.v = 0;
                self.odd_frame = !self.odd_frame;

                if self.odd_frame && self.mask.background() {
                    self.h += 1;
                }

                self.frame_ready = true;
            }
        }

        self.h as u32
    }

    pub fn init(&mut self) {
        let pre_render_line_events = self.build_pre_render_line();
        let visible_line_events = self.build_visible_line();
        let post_render_line_events = self.build_post_render_line();

        for cycle in 0..240 {
            self.events[cycle] = visible_line_events;
        }

        self.events[241] = post_render_line_events;

        self.events[261] = pre_render_line_events;

        // for scanline in 0..NUM_SCANLINES {
        //     let events = self.events[scanline];

        //     for dot in 0..NUM_DOTS {
        //         let event = events[dot];

        //         if event & EVENT_OUTPUT > 0 {  }
        //         if event & EVENT_SHIFT_BACKGROUND > 0 {}
        //         if event & EVENT_SHIFT_SPRITE > 0 {}
        //         if event & EVENT_RELOAD_BACKGROUND > 0 {}
        //         if event & EVENT_FETCH_NAME_TABLE > 0 { self.put_debug_pixel(dot, scanline, 0xcc6f00); }
        //         if event & EVENT_FETCH_ATTRIBUTE_TABLE > 0 { self.put_debug_pixel(dot, scanline, 0xcc6f00); }
        //         if event & EVENT_FETCH_LOW_BACKGORUND > 0 { self.put_debug_pixel(dot, scanline, 0xcc6f00); }
        //         if event & EVENT_FETCH_HIGH_BACKGROUND > 0 { self.put_debug_pixel(dot, scanline, 0xcc6f00); }
        //         if event & EVENT_SET_VERTICAL_BLANK > 0 { self.put_debug_pixel(dot, scanline, 0x00ff00); }
        //         if event & EVENT_CLEAR_VERTICAL_BLANK > 0 { self.put_debug_pixel(dot, scanline, 0x00ff00); }
        //         if event & EVENT_INCREMENT_HORIZONTAL > 0 { self.put_debug_pixel(dot, scanline, 0xff0000); }
        //         if event & EVENT_INCREMENT_VERTICAL > 0 { self.put_debug_pixel(dot, scanline, 0x00ff00); }
        //         if event & EVENT_SET_HORIZONTAL > 0 { self.put_debug_pixel(dot, scanline, 0xffff00); }
        //         if event & EVENT_SET_VERTICAL > 0 { self.put_debug_pixel(dot, scanline, 0xffff00); }
        //         if event & EVENT_SECONDARY_OAM_CLEAR > 0 {}
        //         if event & EVENT_SPRITE_EVALUATION > 0 {}
        //     }
        // }
    }

    fn build_pre_render_line(&mut self) -> [u16; NUM_DOTS] {
        let mut events = [0u16; NUM_DOTS];

        events[1] |= EVENT_CLEAR_VERTICAL_BLANK;

        for cycle in (1..=256).step_by(8) {
            events[cycle + 0] |= EVENT_FETCH_NAME_TABLE;
            events[cycle + 2] |= EVENT_FETCH_ATTRIBUTE_TABLE;
            events[cycle + 4] |= EVENT_FETCH_LOW_BACKGORUND;
            events[cycle + 6] |= EVENT_FETCH_HIGH_BACKGROUND;
        }

        for cycle in 2..=257 {
            events[cycle] |= EVENT_SHIFT_BACKGROUND;
            events[cycle] |= EVENT_SHIFT_SPRITE;
        }

        for cycle in (9..=257).step_by(8) {
            events[cycle] |= EVENT_RELOAD_BACKGROUND;
        }

        for cycle in (8..=256).step_by(8) {
            events[cycle] |= EVENT_INCREMENT_HORIZONTAL;
        }

        events[256] |= EVENT_INCREMENT_VERTICAL;
        events[257] |= EVENT_SET_HORIZONTAL;

        // for cycle in (257..=319).step_by(8) {
        //     events[cycle] |= EVENT_FETCH_SPRITE;
        // }

        for cycle in 280..=304 {
            events[cycle] |= EVENT_SET_VERTICAL;
        }

        for cycle in (321..=336).step_by(8) {
            events[cycle + 0] |= EVENT_FETCH_NAME_TABLE;
            events[cycle + 2] |= EVENT_FETCH_ATTRIBUTE_TABLE;
            events[cycle + 4] |= EVENT_FETCH_LOW_BACKGORUND;
            events[cycle + 6] |= EVENT_FETCH_HIGH_BACKGROUND;
        }

        for cycle in 322..=337 {
            events[cycle] |= EVENT_SHIFT_BACKGROUND;
        }

        for cycle in (329..=337).step_by(8) {
            events[cycle] |= EVENT_RELOAD_BACKGROUND;
        }

        for cycle in (328..=336).step_by(8) {
            events[cycle] |= EVENT_INCREMENT_HORIZONTAL;
        }

        for cycle in (337..=340).step_by(4) {
            events[cycle] |= EVENT_FETCH_NAME_TABLE;
            events[cycle + 2] |= EVENT_FETCH_NAME_TABLE;
        }

        events
    }

    fn build_visible_line(&mut self) -> [u16; NUM_DOTS] {
        let mut events = [0u16; NUM_DOTS];

        events[1] |= EVENT_SECONDARY_OAM_CLEAR;
        events[65] |= EVENT_SPRITE_EVALUATION;

        for cycle in 2..=257 {
            events[cycle] |= EVENT_OUTPUT;
        }

        for cycle in (1..=256).step_by(8) {
            events[cycle + 0] |= EVENT_FETCH_NAME_TABLE;
            events[cycle + 2] |= EVENT_FETCH_ATTRIBUTE_TABLE;
            events[cycle + 4] |= EVENT_FETCH_LOW_BACKGORUND;
            events[cycle + 6] |= EVENT_FETCH_HIGH_BACKGROUND;
        }

        for cycle in 2..=257 {
            events[cycle] |= EVENT_SHIFT_BACKGROUND;
            events[cycle] |= EVENT_SHIFT_SPRITE;
        }

        for cycle in (9..=257).step_by(8) {
            events[cycle] |= EVENT_RELOAD_BACKGROUND;
        }

        for cycle in (8..=256).step_by(8) {
            events[cycle] |= EVENT_INCREMENT_HORIZONTAL;
        }

        events[256] |= EVENT_INCREMENT_VERTICAL;
        events[257] |= EVENT_SET_HORIZONTAL;

        // for cycle in (257..=319).step_by(8) {
        //     events[cycle] |= EVENT_FETCH_SPRITE;
        // }

        for cycle in (321..=336).step_by(8) {
            events[cycle + 0] |= EVENT_FETCH_NAME_TABLE;
            events[cycle + 2] |= EVENT_FETCH_ATTRIBUTE_TABLE;
            events[cycle + 4] |= EVENT_FETCH_LOW_BACKGORUND;
            events[cycle + 6] |= EVENT_FETCH_HIGH_BACKGROUND;
        }

        for cycle in 322..=337 {
            events[cycle] |= EVENT_SHIFT_BACKGROUND;
        }

        for cycle in (329..=337).step_by(8) {
            events[cycle] |= EVENT_RELOAD_BACKGROUND;
        }

        for cycle in (328..=336).step_by(8) {
            events[cycle] |= EVENT_INCREMENT_HORIZONTAL;
        }

        for cycle in (337..=340).step_by(4) {
            events[cycle] |= EVENT_FETCH_NAME_TABLE;
            events[cycle + 2] |= EVENT_FETCH_NAME_TABLE;
        }

        events
    }

    fn build_post_render_line(&mut self) -> [u16; NUM_DOTS] {
        let mut events = [0u16; NUM_DOTS];

        events[1] |= EVENT_SET_VERTICAL_BLANK;

        events
    }
}
