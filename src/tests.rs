#[allow(unused_imports)]
use crate::*;

#[cfg(test)]
// TODO: clean up test cases and create more for additional assembly functions
/*
#[test]
fn test_lda_immediate_load() {
    let mut cpu = CPU::new(Bus::new(Rom::new(&vec![0xa9, 0x05, 0x00]).unwrap()));
    cpu.run_with_callback(|_| {});

    assert_eq!(cpu.status.bits() & 0b0000_0010, 0b00);
    assert_eq!(cpu.status.bits() & 0b1000_0000, 0);
}

#[test]
fn test_lda_immediate_zero_flag() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x00, 0x00]);

    assert_eq!(cpu.status.bits() & 0b0000_0010, 0b10);
}

#[test]
fn test_lda_from_mem() {
    let mut cpu = CPU::new();
    cpu.mem_write(0x10, 0x55);

    cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

    assert_eq!(cpu.register_a, 0x55)
}

#[test]
fn test_tax_move_a_to_x() {
    let mut cpu = CPU::new();
    cpu.register_a = 10;
    cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

    assert_eq!(cpu.register_x, 10);
}

#[test]
fn test_5_ops_together() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 0xc1);
}

#[test]
fn test_inx_overflow() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

    assert_eq!(cpu.register_x, 1);
}

#[test]
fn test_sta() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x01, 0x85, 0x10, 0x00]);

    assert_eq!(cpu.mem_read(0x10), 0x01);
}

#[test]
fn test_sta_x() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xe8, 0xa9, 0x01, 0x95, 0x10, 0x00]);

    assert_eq!(cpu.mem_read(0x11), 0x01);
}

*/
fn test_rom() -> Rom {
    todo!("configure a proper default rom to use in test cases");
}

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
