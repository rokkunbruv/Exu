use crate::instruction::Instr;
use crate::types::Type;

#[derive(Clone, PartialEq)]
pub enum Object<'a> {
    Function(Func<'a>),
}

#[derive(Clone, PartialEq)]
pub struct Func<'a> {
    pub name: &'a str,
    pub ret_type: Option<Type>,
    pub instructions: Vec<Instr<'a>>,
}

impl<'a> Func<'a> {
    pub fn new(
        name: &'a str,
        ret_type: Option<Type>,
        instructions: Option<Vec<Instr<'a>>>,
    ) -> Self {
        Self {
            name,
            ret_type,
            instructions: match instructions {
                Some(instrs) => instrs,
                None => Vec::new(),
            },
        }
    }

    pub fn load_instrs(&mut self, instrs: Vec<Instr<'a>>) {
        self.instructions = instrs;
    }
}
