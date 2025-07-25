use crate::instruction::Instr;
use crate::vm::{Address, Register};

#[derive(Clone, PartialEq)]
pub enum Object<'a> {
    Function {
        address: Address,
        capacity: usize,
        upvalues: Vec<(&'a str, Register)>,
        instructions: Vec<Instr<'a>>,
    },
}
