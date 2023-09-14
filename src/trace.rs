use crate::{cpu::CPU, opcodes};
use std::collections::HashMap;

pub fn trace(cpu: &CPU) -> String {
    let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OP_CODE_MAP;

    todo!("still needs to be implemented")
}
