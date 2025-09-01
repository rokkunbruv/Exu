use crate::debug::DebugConfig;
use crate::executable::Executable;
use crate::instruction::Instr;
use crate::value::Value;
use std::fmt;

pub type Register = usize;
pub type Address = usize;
pub type Label<'prog> = &'prog str;

pub type RegisterType = [Slot; 24];

#[derive(Clone, PartialEq)]
pub enum Slot {
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
    registers: RegisterType,
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

    pub fn run(&mut self, debug_config: Option<DebugConfig>, program: Executable) {
        let instrs = program.instructions;

        let mut pc = instrs.iter().enumerate().skip(0);

        if let Some(dbg_cfg) = &debug_config {
            if dbg_cfg.debug_flags.debug_config {
                println!("=====EXECUTING INSTRUCTIONS=====");
            }
        }

        let mut instr_count: u32 = 0;

        while let Some((pc_loc, instr)) = pc.next() {
            if let Some(dbg_cfg) = &debug_config {
                dbg_cfg.log_instr(instr, instr_count);
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
                Instr::And { dest, src1, src2 } => {
                    let is_and = self.get_bool(src1) && self.get_bool(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_and)));
                }
                Instr::Or { dest, src1, src2 } => {
                    let is_or = self.get_bool(src1) || self.get_bool(src2);
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_or)));
                }
                Instr::Not { dest, src } => {
                    let is_not = !self.get_bool(src);
                    self.load_to_reg(dest, Slot::Val(Value::Bool(is_not)));
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
                Instr::JmpAddr { src } => {
                    let addr;

                    match self.registers[src] {
                        Slot::Addr(a) => addr = a,
                        _ => panic!("ERROR"),
                    }

                    pc = instrs.iter().enumerate().skip(addr);
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
            }

            if let Some(dbg_cfg) = &debug_config {
                dbg_cfg.log_vm_state(&self.registers);
            }

            instr_count += 1;
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
