#[allow(unused_imports)]
use crate::*;

struct TestRom {
    header: Vec<u8>,
    trainer: Option<Vec<u8>>,
    pgp_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}

fn create_rom(rom: TestRom) -> Vec<u8> {
    let mut result = Vec::with_capacity(
        rom.header.len()
            + rom.trainer.as_ref().map_or(0, |t| t.len())
            + rom.pgp_rom.len()
            + rom.chr_rom.len(),
    );

    result.extend(&rom.header);
    if let Some(t) = rom.trainer {
        result.extend(t);
    }
    result.extend(&rom.pgp_rom);
    result.extend(&rom.chr_rom);

    result
}

fn test_rom() -> Rom {
    let test_rom = create_rom(TestRom {
        header: vec![
            0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
        ],
        trainer: None,
        pgp_rom: vec![1; 2 * crate::catridge::PRG_ROM_PAGE_SIZE],
        chr_rom: vec![2; 1 * crate::catridge::CHR_ROM_PAGE_SIZE],
    });

    Rom::new(&test_rom).unwrap()
}

#[cfg(test)]
#[test]
fn test_format_trace() {
    let mut bus = Bus::new(test_rom());
    bus.mem_write(100, 0xa2);
    bus.mem_write(101, 0x01);
    bus.mem_write(102, 0xca);
    bus.mem_write(103, 0x88);
    bus.mem_write(104, 0x00);

    let mut cpu = CPU::new(bus);
    cpu.program_counter = 0x64;
    cpu.register_a = 1;
    cpu.register_x = 2;
    cpu.register_y = 3;
    let mut result: Vec<String> = vec![];
    cpu.run_with_callback(|cpu| {
        result.push(trace::trace(cpu));
    });
    assert_eq!(
        "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
        result[0]
    );
    assert_eq!(
        "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
        result[1]
    );
    assert_eq!(
        "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
        result[2]
    );
}

#[test]
fn test_format_mem_access() {
    let mut bus = Bus::new(test_rom());
    // ORA ($33), Y
    bus.mem_write(100, 0x11);
    bus.mem_write(101, 0x33);

    //data
    bus.mem_write(0x33, 00);
    bus.mem_write(0x34, 04);

    //target cell
    bus.mem_write(0x400, 0xAA);

    let mut cpu = CPU::new(bus);
    cpu.program_counter = 0x64;
    cpu.register_y = 0;
    let mut result: Vec<String> = vec![];
    cpu.run_with_callback(|cpu| {
        result.push(trace::trace(cpu));
    });
    assert_eq!(
        "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
        result[0]
    );
}
