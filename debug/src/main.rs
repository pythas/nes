extern crate sdl2;

use sdl2::rect::Point;
use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::TextureQuery;
use sdl2::rect::Rect;

use std::time::Duration;

use nes::nes::Nes;
use nes::cpu::Cpu;

static SCREEN_WIDTH: u32 = 640;
static SCREEN_HEIGHT: u32 = 480;
static REAL_SCREEN_WIDTH: u32 = 1024;
static REAL_SCREEN_HEIGHT: u32 = 768;

struct TextRenderer<'a> {
    symbols_texture: sdl2::render::Texture<'a>,
    symbol_rects: Vec<(Rect, u32, u32)>,
}

impl<'a> TextRenderer<'a> {
    pub fn new(
        font: &sdl2::ttf::Font,
        texture_creator: &'a sdl2::render::TextureCreator<sdl2::video::WindowContext>,
    ) -> TextRenderer<'a> {
        let symbols = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ,./';[]=-`<>?:{}|+_)(*&^%$#@!~";
        let surface = font
            .render(symbols)
            .blended(Color::RGBA(255, 255, 255, 255))
            .unwrap();
        let symbols_texture = texture_creator
            .create_texture_from_surface(&surface)
            .unwrap();
        let mut symbol_width = 0;
        let mut symbol_rects = Vec::new();

        symbol_rects.resize(128, (Rect::new(0, 0, 0, 0), 0, 0));

        for symbol in symbols.chars() {
            let surface = font
                .render(&symbol.to_string())
                .blended(Color::RGBA(0, 0, 0, 255))
                .unwrap();
            let texture = texture_creator
                .create_texture_from_surface(&surface)
                .unwrap();

            let TextureQuery { height, .. } = texture.query();
            let metric = font.find_glyph_metrics(symbol).unwrap();

            let symbol_rect = Rect::new(
                symbol_width as i32 + metric.minx,
                0,
                (metric.maxx - metric.minx) as u32,
                height
            );

            symbol_rects[symbol as usize] = (symbol_rect, metric.minx as u32, (metric.advance) as u32);

            symbol_width += metric.advance;
        }

        TextRenderer {
            symbols_texture,
            symbol_rects,
        }
    }
    pub fn render(
        &mut self,
        p: Point,
        text: &str,
        color: Color,
        canvas: &mut sdl2::render::Canvas<sdl2::video::Window>,
    ) -> Rect {
        let mut x = p.x;
        let mut width = 0;
        let mut height = 0;

        for symbol in text.chars() {
            let position = self.symbol_rects[symbol as usize];
            let symbol_rect = position.0;
            let minx = position.1;
            let advance = position.2;

            self.symbols_texture.set_color_mod(color.r, color.g, color.b);

            canvas
                .copy(
                    &self.symbols_texture,
                    symbol_rect,
                    Rect::new(
                        x + minx as i32,
                        p.y,
                        symbol_rect.w as u32,
                        symbol_rect.h as u32,
                    ),
                )
                .unwrap();

            x += advance as i32;
            width = advance + minx;
            height = symbol_rect.h;
        }

        Rect::new(
            p.x,
            p.y,
            width as u32,
            height as u32,
        )
    }
}

pub fn main() {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem.window("NES debug", REAL_SCREEN_WIDTH, REAL_SCREEN_HEIGHT)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    canvas.set_draw_color(Color::RGB(139, 50, 168));
    canvas.clear();
    canvas.present();

    let texture_creator = canvas.texture_creator();

    // Fonts
    let ttf_context = sdl2::ttf::init().unwrap();
    let font = ttf_context.load_font("lucon.ttf", 13).unwrap();
    let mut text_renderer = TextRenderer::new(&font, &texture_creator);

    // ...
    let mut event_pump = sdl_context.event_pump().unwrap();

    // NES
    let cpu = Cpu::new();
    let mut nes = Nes::new(cpu);

    nes.cpu.reset();
    nes.cpu.debug();

    let disassembly = nes.cpu.disassemble(nes.cpu.pc, 0xffff);
    let mut halt = true;
    let mut draw_pattern_table = false;
    let color_white = Color::RGBA(255, 255, 255, 255);
    let color_gray = Color::RGBA(255, 255, 255, 150);
    let color_highlight = Color::RGBA(0, 255, 150, 255);

    // Timer
    let mut timer = sdl_context.timer().unwrap();

    'running: loop {
        let ticks = timer.ticks() as i32;

        canvas.set_draw_color(Color::RGB(139, 50, 168));
        canvas.clear();

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                Event::KeyDown { keycode: Some(Keycode::Right), .. } => {
                    nes.step();
                },
                Event::KeyDown { keycode: Some(Keycode::Return), .. } => {
                    halt = !halt;
                },
                Event::KeyDown { keycode: Some(Keycode::D), .. } => {
                    draw_pattern_table = !draw_pattern_table;
                },
                _ => {}
            }
        }

        if !halt {
            nes.step();
        }

        canvas.set_logical_size(REAL_SCREEN_WIDTH, REAL_SCREEN_HEIGHT).unwrap();

        // Draw CPU state
        let mut target = Rect::new(REAL_SCREEN_WIDTH as i32 - 256, 0, 0, 0);

        target = text_renderer.render(Point::new(target.right(), 0), "N", if nes.cpu.p & (1 << 7) > 0 { color_white } else { color_gray }, &mut canvas);
        target = text_renderer.render(Point::new(target.right(), 0), "V", if nes.cpu.p & (1 << 6) > 0 { color_white } else { color_gray }, &mut canvas);
        target = text_renderer.render(Point::new(target.right(), 0), "U", if nes.cpu.p & (1 << 5) > 0 { color_white } else { color_gray }, &mut canvas);
        target = text_renderer.render(Point::new(target.right(), 0), "B", if nes.cpu.p & (1 << 4) > 0 { color_white } else { color_gray }, &mut canvas);
        target = text_renderer.render(Point::new(target.right(), 0), "D", if nes.cpu.p & (1 << 3) > 0 { color_white } else { color_gray }, &mut canvas);
        target = text_renderer.render(Point::new(target.right(), 0), "I", if nes.cpu.p & (1 << 2) > 0 { color_white } else { color_gray }, &mut canvas);
        target = text_renderer.render(Point::new(target.right(), 0), "Z", if nes.cpu.p & (1 << 1) > 0 { color_white } else { color_gray }, &mut canvas);
        target = text_renderer.render(Point::new(target.right(), 0), "C", if nes.cpu.p & (1 << 0) > 0 { color_white } else { color_gray }, &mut canvas);

        // Draw CPU registers
        target = text_renderer.render(Point::new(REAL_SCREEN_WIDTH as i32 - 256, target.bottom()), &format!("S: {} C: {}", nes.cpu.bus.ppu.scanline, nes.cpu.bus.ppu.clock)[..], color_white, &mut canvas);

        // Draw PPU state
        target = text_renderer.render(Point::new(REAL_SCREEN_WIDTH as i32 - 256, target.bottom()), &format!("A: {:02X} X: {:02X} Y: {:02X}", nes.cpu.a, nes.cpu.x, nes.cpu.y)[..], color_white, &mut canvas);

        // Draw code
        let mut start = disassembly.iter().position(|x| x.0 == nes.cpu.pc).unwrap();
        let current = start;
        let mut stop = start + 15;

        if (start as i32) - 15 < 0 {
            start = 0;
        } else {
            start -= 15;
        }

        if stop > disassembly.len() {
            stop = disassembly.len() - 1;
        }


        target = Rect::new(target.x(), target.y(), target.width(), target.height() + 13);

        for i in start..stop {
            let text = &disassembly[i].1[..];

            if !text.is_empty() {
                target = text_renderer.render(Point::new(REAL_SCREEN_WIDTH as i32 - 256, target.bottom()), text, if i == current { color_highlight } else { color_white }, &mut canvas);
            }
        }

        // Palette
        if draw_pattern_table {
            canvas.set_logical_size(SCREEN_WIDTH, SCREEN_HEIGHT).unwrap();

            let x = SCREEN_WIDTH as i32 - 256;
            let y = SCREEN_HEIGHT as i32 - (128 + 10);

            for palette in 0..8 {
                for index in 0..4 {
                    let color = nes.cpu.bus.ppu.palette_color(palette, index);

                    canvas.set_draw_color(Color::RGB(color.0, color.1, color.2));
                    canvas.fill_rect(Rect::new(x + palette as i32 * 8 * 4 + index as i32 * 6, y, 4, 4)).unwrap();
                }
            }
        }

        // Pattern table
        if draw_pattern_table {
            let pixels_lo = nes.cpu.bus.ppu.debug_pixels(0, 0);
            let pixels_hi = nes.cpu.bus.ppu.debug_pixels(1, 0);

            for pixel in pixels_lo {
                canvas.set_draw_color(Color::RGB(pixel.color.0, pixel.color.1, pixel.color.2));
                canvas.draw_point(Point::new((SCREEN_WIDTH as i32 - 256) + pixel.x as i32, (SCREEN_HEIGHT as i32 - 128) + pixel.y as i32)).unwrap();
            }

            for pixel in pixels_hi {
                canvas.set_draw_color(Color::RGB(pixel.color.0, pixel.color.1, pixel.color.2));
                canvas.draw_point(Point::new((SCREEN_WIDTH as i32 - 128) + pixel.x as i32, (SCREEN_HEIGHT as i32 - 128) + pixel.y as i32)).unwrap();
            }
        }

        canvas.present();

        // let frame_ticks = timer.ticks() as i32 - ticks;
        // println!("{}", frame_ticks);

        // ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}