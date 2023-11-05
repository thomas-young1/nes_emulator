use crate::{
    cpu::{AddressingMode, Mem, CPU},
    opcodes,
};
use std::collections::HashMap;

pub fn trace(cpu: &CPU) -> String {
    let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OP_CODE_MAP;

    let code = cpu.mem_read(cpu.program_counter);
    let op = opcodes.get(&code).unwrap();

    let mut hex_dump = vec![];
    hex_dump.push(code);

    let (mem_addr, stored_val) = match op.mode {
        AddressingMode::Immediate | AddressingMode::NoneAddressing => (0, 0),
        _ => {
            let addr = cpu.get_absolute_address(&op.mode, cpu.program_counter + 1);
            (addr, cpu.mem_read(addr))
        }
    };

    let mem = match op.bytes {
        1 => match op.opcode {
            0x0a | 0x4a | 0x2a | 0x6a => String::from("A "),
            _ => String::from(""),
        },
        2 => {
            let addr = cpu.mem_read(cpu.program_counter + 1);
            hex_dump.push(addr);

            match op.mode {
                AddressingMode::Immediate => format!("#${:02x}", addr),
                AddressingMode::ZeroPage => format!("${:02x} = {:02x}", mem_addr, stored_val),
                AddressingMode::ZeroPage_X => {
                    format!("${:02x},X @ {:02x} = {:02x}", addr, mem_addr, stored_val)
                }
                AddressingMode::ZeroPage_Y => {
                    format!("${:02x},Y @ {:02x} = {:02x}", addr, mem_addr, stored_val)
                }
                AddressingMode::Indirect_X => format!(
                    "(${:02x},X) @ {:02x} = {:04x} = {:02x}",
                    addr,
                    addr.wrapping_add(cpu.register_x),
                    mem_addr,
                    stored_val
                ),
                AddressingMode::Indirect_Y => format!(
                    "(${:02x}),Y = {:04x} @ {:04x} = {:02x}",
                    addr,
                    mem_addr.wrapping_sub(cpu.register_y as u16),
                    mem_addr,
                    stored_val
                ),
                AddressingMode::NoneAddressing => {
                    let address =
                        (cpu.program_counter as usize + 2).wrapping_add((addr as i8) as usize);
                    format!("${:04x}", address)
                }
                _ => {
                    panic!(
                        "unexpected addressing mode {:?} has op length 2. code {:02x}",
                        op.mode, op.opcode
                    )
                }
            }
        }
        3 => {
            let address_lo = cpu.mem_read(cpu.program_counter + 1);
            let address_hi = cpu.mem_read(cpu.program_counter + 2);
            hex_dump.push(address_lo);
            hex_dump.push(address_hi);

            let address = cpu.mem_read_u16(cpu.program_counter + 1);

            match op.mode {
                AddressingMode::NoneAddressing => {
                    if op.opcode == 0x6c {
                        let jmp_addr = if address & 0x00ff == 0x00ff {
                            let lo = cpu.mem_read(address);
                            let hi = cpu.mem_read(address & 0xff00);
                            (hi as u16) << 8 | (lo as u16)
                        } else {
                            cpu.mem_read_u16(address)
                        };

                        format!("(${:04x}) = {:04x}", address, jmp_addr)
                    } else {
                        format!("${:04x}", address)
                    }
                }
                AddressingMode::Absolute => format!("${:04x} = {:02x}", mem_addr, stored_val),
                AddressingMode::Absolute_X => {
                    format!("{:04x},X @ {:04x} = {:02x}", address, mem_addr, stored_val)
                }
                AddressingMode::Absolute_Y => {
                    format!("{:04x},Y @ {:04x} = {:02x}", address, mem_addr, stored_val)
                }
                _ => panic!(
                    "unexpected addressing mode {:?} has op length 3. code {:02x}",
                    op.mode, op.opcode
                ),
            }
        }
        _ => String::from(""),
    };

    let hex_str = hex_dump
        .iter()
        .map(|f| format!("{:02x}", f))
        .collect::<Vec<String>>()
        .join(" ");

    let asm_str = format!(
        "{:04x}  {:8} {: >4} {}",
        cpu.program_counter, hex_str, op.name, mem
    )
    .trim()
    .to_string();

    format!(
        "{:47} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x}",
        asm_str, cpu.register_a, cpu.register_x, cpu.register_y, cpu.status, cpu.stack_pointer
    )
    .to_ascii_uppercase()
}
