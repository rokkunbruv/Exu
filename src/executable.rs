use crate::instruction::Instr;
use crate::vm::Address;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Executable<'prog> {
    /// A map of labels and their indices in the instructions list
    /// Labels are typically used by functions and actions
    pub labels: HashMap<&'prog str, Address>,
    /// Instructions to be executed
    pub instructions: Vec<Instr<'prog>>,
}

impl<'prog> Executable<'prog> {
    pub fn new(labels: HashMap<&'prog str, Address>, instructions: Vec<Instr<'prog>>) -> Self {
        Self {
            labels,
            instructions,
        }
    }
}
