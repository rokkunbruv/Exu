use crate::value::Value;
use crate::vm::{Address, Register};

#[derive(Debug, Clone, PartialEq)]
pub enum Instr<'a> {
    /// Copies the value stored in src register to dest register.
    Mov {
        dest: Register,
        src: Register,
    },
    /// Loads a value stored at addr in the heap to dest register.
    Load {
        dest: Register,
        addr: Address,
    },
    /// Loads an immediate value val to dest register.
    LoadI {
        dest: Register,
        val: Value,
    },
    /// Loads an immediate address addr to dest register.
    LoadAddr {
        dest: Register,
        addr: Address,
    },
    /// Stores a value in src register to addr in the heap.
    Store {
        src: Register,
        addr: Address,
    },
    Add {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    Sub {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    Mul {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    Div {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    Gret {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    GretEq {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    Less {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    LessEq {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    Eq {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    NotEq {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    And {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    Or {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    Not {
        dest: Register,
        src: Register,
    },
    /// Pushes the value stored in src register to the stack.
    PushStack {
        src: Register,
    },
    PushStackPC {
        offset: usize,
    },
    /// Pops the value in the stack to dest register.
    PopStack {
        dest: Register,
    },
    /// Jumps to location in instructions relative to offset.
    /// A positive offset jumps downward of the instructions list.
    /// A negative offset jumps upward of the instructions list.
    Jmp {
        offset: usize,
    },
    /// Jumps to location in instructions relative to offset if the value in src register evaluates to false.
    /// A positive offset jumps downward of the instructions list.
    /// A negative offset jumps upward of the instructions list.
    JmpOnFalse {
        src: Register,
        offset: usize,
    },
    /// Jumps to a label in the instructions list
    JmpLabel {
        label: &'a str,
    },
    /// Jumps to the program counter stored in the stack.
    /// Must be paired with PushStackPC
    JmpToCaller,
    /// Jumps to the address in the instructions list stored in src register.
    JmpAddr {
        src: Register,
    },
    /// Labels a block of code. Immediately skipped by the VM during execution.
    Label(String),
    /// Halts the VM in a success state.
    HaltSuccess,

    // Temporary Instructions
    /// Writes the value stored in src register to the output buffer.
    Println {
        src: Register,
    },
}
