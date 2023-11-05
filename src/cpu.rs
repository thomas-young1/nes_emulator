use crate::{bus::Bus, opcodes};
use std::collections::HashMap;

bitflags! {
    #[derive(Clone)]
    pub struct CpuFlags: u8 {
        const CARRY = 0b0000_0001;
        const ZERO = 0b0000_0010;
        const INTERRUPT_DISABLE = 0b0000_0100;
        const DECIMAL = 0b0000_1000;
        const BREAK = 0b0001_0000;
        const BREAK2 = 0b0010_0000;
        const OVERFLOW = 0b0100_0000;
        const NEGATIVE = 0b1000_0000;
    }
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
}

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    bus: Bus,
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data);
    }
}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            stack_pointer: STACK_RESET,
            bus,
        }
    }

    pub fn get_absolute_address(&self, mode: &AddressingMode, addr: u16) -> u16 {
        match mode {
            AddressingMode::ZeroPage => self.mem_read(addr) as u16,
            AddressingMode::Absolute => self.mem_read_u16(addr),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(addr);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(addr);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute_X => {
                let pos = self.mem_read_u16(addr);
                let addr = pos.wrapping_add(self.register_x as u16) as u16;
                addr
            }
            AddressingMode::Absolute_Y => {
                let pos = self.mem_read_u16(addr);
                let addr = pos.wrapping_add(self.register_y as u16) as u16;
                addr
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(addr);

                let ptr = base.wrapping_add(self.register_x) as u16;
                let lo = self.mem_read(ptr);
                let hi = self.mem_read(ptr.wrapping_add(1));
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(addr) as u16;

                let lo = self.mem_read(base);
                let hi = self.mem_read(base.wrapping_add(1));
                let deref_base = (hi as u16) << 8 | (lo as u16);
                deref_base.wrapping_add(self.register_y as u16)
            }
            _ => {
                panic!("mode {:?} not supported", mode)
            }
        }
    }

    fn add_to_register_a(&mut self, data: u8) {
        let sum = self.register_a as u16
            + data as u16
            + (if self.status.contains(CpuFlags::CARRY) {
                1
            } else {
                0
            });

        self.status.set(CpuFlags::CARRY, sum > 0xff);

        let res = sum as u8;

        if (data ^ res) & (res ^ self.register_a) & 0x00 != 0 {
            self.status.insert(CpuFlags::OVERFLOW);
        } else {
            self.status.remove(CpuFlags::OVERFLOW);
        }

        self.register_a = res;
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write(STACK as u16 + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read(STACK as u16 + self.stack_pointer as u16)
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;
        hi << 8 | lo
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let num = self.mem_read(addr);
        self.add_to_register_a(num);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_neg_flags(self.register_x);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_neg_flags(self.register_x);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let num = self.mem_read(addr);

        self.register_a &= num;
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        if data >> 7 == 1 {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        data <<= 1;
        self.mem_write(addr, data);
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn asl_a(&mut self) {
        if self.register_a >> 7 == 1 {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        self.register_a <<= 1;
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            let jump = self.mem_read(self.program_counter) as i8;
            let jump_addr = self
                .program_counter
                .wrapping_add(1)
                .wrapping_add(jump as u16);
            self.program_counter = jump_addr;
        }
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let num = self.mem_read(addr);

        let res = self.register_a & num;

        self.status.set(CpuFlags::ZERO, res == 0);
        self.status.set(CpuFlags::NEGATIVE, num & 0b1000_0000 > 0);
        self.status.set(CpuFlags::OVERFLOW, num & 0b0100_0000 > 0);
    }

    fn clear_flag(&mut self, flag: CpuFlags) {
        self.status.remove(flag)
    }

    fn cmp(&mut self, mode: &AddressingMode, compare_with: u8) {
        let addr = self.get_operand_address(mode);
        let num = self.mem_read(addr);

        self.status.set(CpuFlags::CARRY, num <= compare_with);

        self.update_zero_and_neg_flags(compare_with.wrapping_sub(num))
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let num = self.mem_read(addr);
        self.mem_write(addr, num.wrapping_sub(1));
        self.update_zero_and_neg_flags(num.wrapping_sub(1))
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_neg_flags(self.register_x);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_neg_flags(self.register_y);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.register_a ^= data;
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.mem_write(addr, data.wrapping_add(1));
        self.update_zero_and_neg_flags(data.wrapping_add(1));
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_neg_flags(self.register_y);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_neg_flags(self.register_x);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_neg_flags(self.register_y);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);

        self.status.set(CpuFlags::CARRY, data & 1 == 1);

        data >>= 1;
        self.mem_write(addr, data);
        self.update_zero_and_neg_flags(data);
    }

    fn lsr_acc(&mut self) {
        self.status.set(CpuFlags::CARRY, self.register_a & 1 == 1);

        self.register_a >>= 1;
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.register_a |= data;

        self.update_zero_and_neg_flags(self.register_a);
    }

    fn php(&mut self) {
        let mut flags = self.status.clone();
        flags.insert(CpuFlags::BREAK);
        flags.insert(CpuFlags::BREAK2);
        self.stack_push(flags.bits());
    }

    fn pla(&mut self) {
        let data = self.stack_pop();
        self.register_a = data;
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn plp(&mut self) {
        self.status = CpuFlags::from_bits_truncate(self.stack_pop());
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);
    }

    fn rol(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut num = self.mem_read(addr);

        let old_carry = self.status.contains(CpuFlags::CARRY);

        self.status.set(CpuFlags::CARRY, num >> 7 == 1);

        num <<= 1;
        if old_carry {
            num |= 1;
        }

        self.mem_write(addr, num);
        self.update_zero_and_neg_flags(num);
    }

    fn rol_acc(&mut self) {
        let old_carry = self.status.contains(CpuFlags::CARRY);
        self.status.set(CpuFlags::CARRY, self.register_a >> 7 == 1);

        self.register_a <<= 1;
        if old_carry {
            self.register_a |= 1;
        }
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn ror(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut num = self.mem_read(addr);

        let old_carry = self.status.contains(CpuFlags::CARRY);

        self.status.set(CpuFlags::CARRY, num << 7 == 0b1000_0000);

        num >>= 1;
        if old_carry {
            num |= 0b1000_0000;
        }

        self.mem_write(addr, num);
        self.update_zero_and_neg_flags(num);
    }

    fn ror_acc(&mut self) {
        let old_carry = self.status.contains(CpuFlags::CARRY);

        self.status
            .set(CpuFlags::CARRY, self.register_a << 7 == 0b1000_0000);

        self.register_a >>= 1;
        if old_carry {
            self.register_a |= 0b1000_0000;
        }

        self.update_zero_and_neg_flags(self.register_a);
    }

    fn rti(&mut self) {
        self.status = CpuFlags::from_bits_truncate(self.stack_pop());
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);

        self.program_counter = self.stack_pop_u16();
    }

    fn rts(&mut self) {
        self.program_counter = self.stack_pop_u16() + 1;
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let num = self.mem_read(addr);
        self.add_to_register_a((num as i8).wrapping_neg().wrapping_sub(1) as u8);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_neg_flags(self.register_y);
    }

    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_neg_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_neg_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_neg_flags(self.register_a)
    }

    fn update_zero_and_neg_flags(&mut self, register: u8) {
        if register == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        if register & 0b1000_0000 != 0 {
            self.status.insert(CpuFlags::NEGATIVE);
        } else {
            self.status.remove(CpuFlags::NEGATIVE);
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        for i in 0..(program.len() as u16) {
            self.mem_write(0x8600 + i, program[i as usize])
        }

        self.mem_write_u16(0xFFFC, 0x600);
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = CpuFlags::from_bits_truncate(0b100100);

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OP_CODE_MAP;

        loop {
            callback(self);
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;

            let starting_counter = self.program_counter;

            let opcode = opcodes
                .get(&code)
                .expect(&format!("Code {:?} not recognized", code));

            match code {
                /* ADC */
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => self.adc(&opcode.mode),
                /* LDA */
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => self.lda(&opcode.mode),
                /* STA */
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => self.sta(&opcode.mode),
                /* AND */
                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => self.and(&opcode.mode),
                /* ASL */
                0x0a => self.asl_a(),
                0x06 | 0x16 | 0x0e | 0x1e => self.asl(&opcode.mode),
                /* BCC */
                0x90 => self.branch(!self.status.contains(CpuFlags::CARRY)),
                /* BCS */
                0xb0 => self.branch(self.status.contains(CpuFlags::CARRY)),
                /* BEQ */
                0xf0 => self.branch(self.status.contains(CpuFlags::ZERO)),
                /* BMI */
                0x30 => self.branch(self.status.contains(CpuFlags::NEGATIVE)),
                /* BNE */
                0xd0 => self.branch(!self.status.contains(CpuFlags::ZERO)),
                /* BPL */
                0x10 => self.branch(!self.status.contains(CpuFlags::NEGATIVE)),
                /* BVC */
                0x50 => self.branch(!self.status.contains(CpuFlags::OVERFLOW)),
                /* BVS */
                0x70 => self.branch(!self.status.contains(CpuFlags::OVERFLOW)),
                /* CLC */
                0x18 => self.clear_flag(CpuFlags::CARRY),
                /* CLD */
                0xd8 => self.clear_flag(CpuFlags::DECIMAL),
                /* CLI */
                0x58 => self.clear_flag(CpuFlags::INTERRUPT_DISABLE),
                /* CLV */
                0xb8 => self.clear_flag(CpuFlags::OVERFLOW),
                /* CMP */
                0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd | 0xd9 | 0xc1 | 0xd1 => {
                    self.cmp(&opcode.mode, self.register_a)
                }
                /* CPX */
                0xe0 | 0xe4 | 0xec => self.cmp(&opcode.mode, self.register_x),
                /* CPY */
                0xc0 | 0xc4 | 0xcc => self.cmp(&opcode.mode, self.register_y),
                /* BIT */
                0x24 | 0x2c => self.bit(&opcode.mode),
                /* DEC */
                0xc6 | 0xd6 | 0xce | 0xde => self.dec(&opcode.mode),
                /* DEX */
                0xca => self.dex(),
                /* DEY */
                0x88 => self.dey(),
                /* EOR */
                0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => self.eor(&opcode.mode),
                /* INC */
                0xe6 | 0xf6 | 0xee | 0xfe => self.inc(&opcode.mode),
                /* INX */
                0xe8 => self.inx(),
                /* INY */
                0xc8 => self.iny(),
                /* Absolute JMP */
                0x4c => {
                    let addr = self.mem_read_u16(self.program_counter);
                    self.program_counter = addr;
                }
                /* Indirect JMP */
                0x6c => {
                    let ptr = self.mem_read_u16(self.program_counter);
                    let addr = if ptr & 0x00FF == 0x00FF {
                        let lo = self.mem_read(ptr);
                        let hi = self.mem_read(ptr & 0xFF00);
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        self.mem_read_u16(ptr)
                    };

                    self.program_counter = addr;
                }
                /* JSR */
                0x20 => {
                    self.stack_push_u16(self.program_counter + 1);
                    let target_address = self.mem_read_u16(self.program_counter);
                    self.program_counter = target_address
                }
                /* LDX */
                0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => self.ldx(&opcode.mode),
                /* LDY */
                0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => self.ldy(&opcode.mode),
                /* LSR */
                0x4a => self.lsr_acc(),
                0x46 | 0x56 | 0x4e | 0x5e => self.lsr(&opcode.mode),
                /* NOP */
                0xea => { /* nothing */ }
                /* ORA */
                0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => self.ora(&opcode.mode),
                /* PHA */
                0x48 => self.stack_push(self.register_a),
                /* PHP */
                0x08 => self.php(),
                /* PLA */
                0x68 => self.pla(),
                /* PLP */
                0x28 => self.plp(),
                /* ROL */
                0x2a => self.rol_acc(),
                0x26 | 0x36 | 0x2e | 0x3e => self.rol(&opcode.mode),
                /* ROR */
                0x6a => self.ror_acc(),
                0x66 | 0x76 | 0x6e | 0x7e => self.ror(&opcode.mode),
                /* RTI */
                0x40 => self.rti(),
                /* RTS */
                0x60 => self.rts(),
                /* SBC */
                0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => self.sbc(&opcode.mode),
                /* SEC */
                0x38 => self.status.insert(CpuFlags::CARRY),
                /* SED */
                0xf8 => self.status.insert(CpuFlags::DECIMAL),
                /* SEI */
                0x78 => self.status.insert(CpuFlags::INTERRUPT_DISABLE),
                /* STX */
                0x86 | 0x96 | 0x8e => self.stx(&opcode.mode),
                /* STY */
                0x84 | 0x94 | 0x8c => self.sty(&opcode.mode),
                /* TAX */
                0xaa => self.tax(),
                /* TAY */
                0xa8 => self.tay(),
                /* TSX */
                0xba => self.tsx(),
                /* TXA */
                0x8a => self.txa(),
                /* TXS */
                0x9a => self.txs(),
                /* TYA */
                0x98 => self.tya(),

                0x00 => return,
                _ => todo!(),
            }

            if starting_counter == self.program_counter {
                self.program_counter += (opcode.bytes - 1) as u16;
            }
        }
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);
                let ptr = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read(base.wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | lo as u16;
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            AddressingMode::NoneAddressing => panic!("Mode {:?} has not been implemented", mode),
        }
    }
}
