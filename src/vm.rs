use crate::cli::{display_debug_info_toggle, display_vm_states_toggle, Cli};
use crate::compiler::Executable;
use crate::instruction::Instr;
use crate::value::Value;
use std::fmt;

pub type Register = usize;
pub type Address = usize;

#[derive(Clone, PartialEq)]
enum Slot {
    Val(Value),
    Addr(Address),
}

impl fmt::Debug for Slot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Slot::Val(v) => write!(f, "{:?}", v),
            Slot::Addr(a) => write!(f, "{}", a),
        }
    }
}

pub struct VM {
    registers: [Slot; 24],
    stack: Vec<Slot>,
    heap: Vec<Slot>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            registers: core::array::from_fn(|_| Slot::Val(Value::Num(0.0))),
            stack: Vec::new(),
            heap: Vec::new(),
        }
    }

    pub fn run(&mut self, cli: &Cli, program: Executable) {
        let display_debug_info = display_debug_info_toggle(cli);
        let display_vm_states = display_vm_states_toggle(cli);

        let instrs = program.instructions;

        let mut pc = instrs.iter().enumerate().skip(0);

        if display_debug_info {
            println!("=====EXECUTING INSTRUCTIONS=====");
        }

        while let Some((pc_loc, instr)) = pc.next() {
            if display_debug_info {
                println!("PC={:?} {:?}", pc_loc, instr);
            }

            match instr.clone() {
                Instr::Mov { dest, src } => {
                    self.load_to_reg(dest, self.registers[src].clone());
                }
                Instr::Load { dest, addr } => {
                    self.load_to_reg(dest, self.heap[addr].clone());
                }
                Instr::LoadI { dest, val } => {
                    self.load_to_reg(dest, Slot::Val(val));
                }
                Instr::LoadAddr { dest, addr } => {
                    self.load_to_reg(dest, Slot::Addr(addr));
                }
                Instr::Store { src, addr } => {
                    self.store_to_heap(addr, self.registers[src].clone());
                }
                Instr::Add { dest, src1, src2 } => {
                    if let (Slot::Val(value1), Slot::Val(value2)) =
                        (self.registers[src1].clone(), self.registers[src2].clone())
                    {
                        if let (Value::Str(str1), Value::Str(str2)) = (&value1, &value2) {
                            self.load_to_reg(dest, Slot::Val(Value::Str(str1.clone() + str2)));
                        } else if let (Value::Num(num1), Value::Num(num2)) = (&value1, &value2) {
                            self.load_to_reg(dest, Slot::Val(Value::Num(num1 + num2)));
                        } else {
                            panic!("Invalid use of plus operator on non-numbers and non-strings");
                        }
                    } else {
                        panic!("Invalid use of plus operator on non-values");
                    }
                }
                Instr::AddI { dest, src, imm } => {
                    let sum = self.get_num(src) + self.get_imm(imm);
                    self.load_to_reg(dest, Slot::Val(Value::Num(sum)));
                }
                Instr::Sub { dest, src1, src2 } => {
                    let diff = self.get_num(src1) - self.get_num(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Num(diff)));
                }
                Instr::Mul { dest, src1, src2 } => {
                    let prod = self.get_num(src1) * self.get_num(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Num(prod)));
                }
                Instr::Div { dest, src1, src2 } => {
                    let quo = self.get_num(src1) / self.get_num(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Num(quo)));
                }
                Instr::Gret { dest, src1, src2 } => {
                    let is_gret = self.get_num(src1) > self.get_num(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_gret)));
                }
                Instr::GretEq { dest, src1, src2 } => {
                    let is_gret_or_eq = self.get_num(src1) >= self.get_num(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_gret_or_eq)));
                }
                Instr::Less { dest, src1, src2 } => {
                    let is_less = self.get_num(src1) < self.get_num(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_less)));
                }
                Instr::LessEq { dest, src1, src2 } => {
                    let is_less_or_eq = self.get_num(src1) <= self.get_num(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_less_or_eq)));
                }
                Instr::Eq { dest, src1, src2 } => {
                    let is_eq = self.registers[src1] == self.registers[src2];
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_eq)));
                }
                Instr::NotEq { dest, src1, src2 } => {
                    let is_not_eq = self.registers[src1] != self.registers[src2];
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_not_eq)));
                }
                Instr::PushStack { src } => {
                    self.stack.push(self.registers[src].clone());
                }
                Instr::PushStackPC { offset } => self.stack.push(Slot::Addr(pc_loc + offset)),
                Instr::PopStack { dest } => {
                    self.registers[dest] =
                        self.stack.pop().expect("Cannot pop from an empty stack.");
                }
                Instr::Jmp { offset } => {
                    pc = instrs.iter().enumerate().skip(pc_loc + offset);
                }
                Instr::JmpOnFalse { src, offset } => {
                    let boolean = self.get_bool(src);
                    if !boolean {
                        pc = instrs.iter().enumerate().skip(pc_loc + offset);
                    }
                }
                Instr::JmpLabel { label } => {
                    let (_, label_addr) = program
                        .labels
                        .get_key_value(label)
                        .expect("Invalid jump to undefined label.");

                    pc = instrs.iter().enumerate().skip(*label_addr);
                }
                Instr::JmpToCaller => {
                    let addr_after_call =
                        self.stack.pop().expect("Cannot pop from an empty stack.");

                    if let Slot::Addr(a) = &addr_after_call {
                        pc = instrs.iter().enumerate().skip(*a);
                    } else {
                        panic!("ERROR");
                    }
                }
                Instr::Label(_) => {
                    continue;
                }
                Instr::HaltSuccess => {
                    break;
                }
                Instr::Println { src } => {
                    println!("{:?}", &self.registers[src]);
                }
                _ => {}
            }

            if display_vm_states {
                for reg in self.registers.iter() {
                    print!("{:?} ", reg);
                }
                println!("\n");
            }
        }
    }

    fn get_num(&self, reg: Register) -> f64 {
        match self.registers[reg] {
            Slot::Val(Value::Num(n)) => n,
            _ => panic!("Value in register not a number."),
        }
    }

    fn get_bool(&self, reg: Register) -> bool {
        match self.registers[reg] {
            Slot::Val(Value::Bool(b)) => b,
            _ => panic!("Value in register not a boolean."),
        }
    }

    fn get_imm(&self, imm: Value) -> f64 {
        match imm {
            Value::Num(n) => n,
            _ => 0.0,
        }
    }

    fn store_to_heap(&mut self, addr: Address, val: Slot) {
        if addr >= self.heap.len() {
            self.heap.resize(addr + 1, Slot::Val(Value::Num(0.0)));
        }

        self.heap[addr] = val;
    }

    fn load_to_reg(&mut self, reg: Register, val: Slot) {
        self.registers[reg] = val;
    }
}
