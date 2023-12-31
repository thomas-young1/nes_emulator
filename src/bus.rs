use crate::catridge::Rom;
use crate::cpu::Mem;

pub struct Bus {
    cpu_vram: [u8; 2048],
    rom: Rom,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            cpu_vram: [0; 2048],
            rom,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            // mirror if needed
            addr %= 0x4000;
        }

        self.rom.prg_rom[addr as usize]
    }
}

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let sized_addr = addr & 0b00000111_11111111;
                self.cpu_vram[sized_addr as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _sized_addr = addr & 0b00100000_00000111;
                todo!("PPU not yet supported")
            }
            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => {
                println!("ignoring read at address {}", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let sized_addr = addr & 0b00000111_11111111;
                self.cpu_vram[sized_addr as usize] = data
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _sized_addr = addr & 0b00100000_00000111;
                todo!("PPU not yet supported")
            }
            0x8000..=0xFFFF => panic!("Attempt to write to cartridge ROM"),
            _ => {
                println!("ignoring write at address {}", addr)
            }
        }
    }
}
