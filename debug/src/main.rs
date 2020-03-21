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

macro_rules! rect(
    ($x:expr, $y:expr, $w:expr, $h:expr) => (
        Rect::new($x as i32, $y as i32, $w as u32, $h as u32)
    )
);

fn draw_text(
    x: i32,
    y: i32,
    text: &str,
    color: Color,
    font: &sdl2::ttf::Font, texture_creator: &sdl2::render::TextureCreator<sdl2::video::WindowContext>,
    canvas: &mut sdl2::render::Canvas<sdl2::video::Window>
) -> Rect {
    let surface = font.render(text).blended(color).unwrap();
    let texture = texture_creator.create_texture_from_surface(&surface).unwrap();
    let TextureQuery { width, height, .. } = texture.query();
    let target = rect!(x, y, width, height);

    canvas.copy(&texture, None, Some(target)).unwrap();

    target
}

pub fn main() {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let ttf_context = sdl2::ttf::init().unwrap();

    let window = video_subsystem.window("NES debug", REAL_SCREEN_WIDTH, REAL_SCREEN_HEIGHT)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    canvas.set_draw_color(Color::RGB(139, 50, 168));
    canvas.clear();
    canvas.present();

    let texture_creator = canvas.texture_creator();

    let font = ttf_context.load_font("lucon.ttf", 13).unwrap();

    let mut event_pump = sdl_context.event_pump().unwrap();

    // NES
    let cpu = Cpu::new();
    let mut nes = Nes::new(cpu);

    nes.cpu.reset();
    nes.cpu.debug();

    let disassembly = nes.cpu.disassemble(nes.cpu.pc, 0xffff);
    let mut halt = true;
    let mut draw_pt = true;
    let mut draw_c = true;
    let color_white = Color::RGBA(255, 255, 255, 255);
    let color_gray = Color::RGBA(255, 255, 255, 150);
    let color_highlight = Color::RGBA(0, 255, 150, 255);

    'running: loop {
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
                    draw_pt = !draw_pt;
                },
                Event::KeyDown { keycode: Some(Keycode::C), .. } => {
                    draw_c = !draw_c;
                },
                _ => {}
            }
        }

        if !halt {
            nes.step();
        }


        if draw_c {
            canvas.set_logical_size(REAL_SCREEN_WIDTH, REAL_SCREEN_HEIGHT).unwrap();

            // Draw CPU state
            let mut target = rect!(REAL_SCREEN_WIDTH as i32 - 256, 0, 0, 0);

            target = draw_text(target.right(), 0, "N", if nes.cpu.p & (1 << 7) > 0 { color_white } else { color_gray }, &font, &texture_creator, &mut canvas);
            target = draw_text(target.right(), 0, "V", if nes.cpu.p & (1 << 6) > 0 { color_white } else { color_gray }, &font, &texture_creator, &mut canvas);
            target = draw_text(target.right(), 0, "U", if nes.cpu.p & (1 << 5) > 0 { color_white } else { color_gray }, &font, &texture_creator, &mut canvas);
            target = draw_text(target.right(), 0, "B", if nes.cpu.p & (1 << 4) > 0 { color_white } else { color_gray }, &font, &texture_creator, &mut canvas);
            target = draw_text(target.right(), 0, "D", if nes.cpu.p & (1 << 3) > 0 { color_white } else { color_gray }, &font, &texture_creator, &mut canvas);
            target = draw_text(target.right(), 0, "I", if nes.cpu.p & (1 << 2) > 0 { color_white } else { color_gray }, &font, &texture_creator, &mut canvas);
            target = draw_text(target.right(), 0, "Z", if nes.cpu.p & (1 << 1) > 0 { color_white } else { color_gray }, &font, &texture_creator, &mut canvas);
            target = draw_text(target.right(), 0, "C", if nes.cpu.p & (1 << 0) > 0 { color_white } else { color_gray }, &font, &texture_creator, &mut canvas);

            // Draw CPU registers
            target = draw_text(REAL_SCREEN_WIDTH as i32 - 256, target.bottom(), &format!("S: {} C: {}", nes.cpu.bus.ppu.scanline, nes.cpu.bus.ppu.clock)[..], color_white, &font, &texture_creator, &mut canvas);

            // Draw PPU state
            target = draw_text(REAL_SCREEN_WIDTH as i32 - 256, target.bottom(), &format!("A: {:02X} X: {:02X} Y: {:02X}", nes.cpu.a, nes.cpu.x, nes.cpu.y)[..], color_white, &font, &texture_creator, &mut canvas);

            // Draw code

            let mut start = disassembly.iter().position(|x| x.0 == nes.cpu.pc).unwrap();
            let current = start;
            let mut stop = start + 20;

            if (start as i32) - 20 < 0 {
                start = 0;
            } else {
                start -= 20;
            }

            if stop > disassembly.len() {
                stop = disassembly.len() - 1;
            }


            target = rect!(target.x(), target.y(), target.width(), target.height() + 13);

            for i in start..stop {
                let text = &disassembly[i].1[..];

                if !text.is_empty() {
                    target = draw_text(REAL_SCREEN_WIDTH as i32 - 256, target.bottom(), text, if i == current { color_highlight } else { color_white }, &font, &texture_creator, &mut canvas);
                }
            }
        }


        // Pattern table
        if draw_pt {
            canvas.set_logical_size(SCREEN_WIDTH, SCREEN_HEIGHT).unwrap();

            let pixels_lo = nes.cpu.bus.ppu.debug_pixels(0, 1);
            let pixels_hi = nes.cpu.bus.ppu.debug_pixels(1, 1);

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
        // ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}