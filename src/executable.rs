use crate::instruction::Instr;
use crate::vm::Address;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Executable<'prog> {
    /// A map of labels and their indices in the instructions list
    /// for global functions and actions
    pub labels: HashMap<String, Address>,
    /// Instructions to be executed
    pub instructions: Vec<Instr<'prog>>,
}

impl<'prog> Executable<'prog> {
    pub fn new(labels: HashMap<String, Address>, instructions: Vec<Instr<'prog>>) -> Self {
        Self {
            labels,
            instructions,
        }
    }
}
