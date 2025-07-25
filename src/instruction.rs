use crate::value::Value;
use crate::vm::{Address, Register};

#[derive(Debug, Clone, PartialEq)]
pub enum Instr<'a> {
    Mov {
        dest: Register,
        src: Register,
    },
    Load {
        dest: Register,
        addr: Address,
    },
    LoadI {
        dest: Register,
        val: Value,
    },
    LoadAddr {
        dest: Register,
        addr: Address,
    },
    Store {
        src: Register,
        addr: Address,
    },
    Add {
        dest: Register,
        src1: Register,
        src2: Register,
    },
    AddI {
        dest: Register,
        src: Register,
        imm: Value,
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
    PushStack {
        src: Register,
    },
    PushStackPC {
        offset: usize,
    },
    PopStack {
        dest: Register,
    },
    Jmp {
        offset: usize,
    },
    JmpOnFalse {
        src: Register,
        offset: usize,
    },
    JmpLabel {
        label: &'a str,
    },
    JmpToCaller,
    Label(&'a str),
    HaltSuccess,

    // Temporary Instructions
    Println {
        src: Register,
    },
}
