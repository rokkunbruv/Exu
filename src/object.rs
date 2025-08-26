use crate::instruction::Instr;
use crate::types::Type;
use crate::vm::Address;

#[derive(Clone, PartialEq, Debug)]
pub enum Object<'a> {
    Function(Func<'a>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Func<'a> {
    pub name: &'a str,
    pub label: String,
    pub ret_type: Option<Type>,
    pub instructions: Vec<Instr<'a>>,
    pub address: Address,
}

impl<'a> Func<'a> {
    pub fn new(
        name: &'a str,
        label: String,
        ret_type: Option<Type>,
        instructions: Option<Vec<Instr<'a>>>,
        address: Address,
    ) -> Self {
        Self {
            name,
            label,
            ret_type,
            instructions: match instructions {
                Some(instrs) => instrs,
                None => Vec::new(),
            },
            address,
        }
    }

    pub fn load_instrs(&mut self, instrs: Vec<Instr<'a>>) {
        self.instructions = instrs;
    }
}
