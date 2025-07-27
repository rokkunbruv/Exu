mod object;

use crate::ast::{Expr, ProgramItem, Stmt};
use crate::instruction::Instr;
use crate::types::Type;
use crate::value::Value;
use crate::vm::{Address, Register};
use crate::Program;
use crate::Spanned;
use object::Object;
use std::collections::HashMap;
use std::error::Error;
use std::io;

/// The register index to store the return value of a procedure/function/action call
const RETURN_REGISTER: Register = 0;
// The starting register index to store values
const START_REGISTER: Register = 1;

// The starting address index to store objects
const START_ADDRESS: Address = 0;

#[derive(Debug)]
pub struct Executable<'prog> {
    pub labels: HashMap<&'prog str, Address>,
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

struct Procedure<'prog> {
    pub instrs: Vec<Instr<'prog>>,
    pub ret_type: Option<Type>,
}

impl<'prog> Procedure<'prog> {
    pub fn new(ret_type: Option<Type>) -> Self {
        Self {
            instrs: Vec::new(),
            ret_type,
        }
    }

    pub fn load_instrs(&mut self, instrs: Vec<Instr<'prog>>) {
        self.instrs = instrs;
    }
}

#[derive(Debug, Clone)]
struct CompileContext {
    pub is_save_upvalues: bool,
    pub is_generate_scope: bool,
}

impl CompileContext {
    pub fn new() -> Self {
        Self {
            is_save_upvalues: false,
            is_generate_scope: true,
        }
    }

    pub fn fn_mode(mut self) -> Self {
        self.is_save_upvalues = true;
        self.is_generate_scope = false;
        self
    }
}

pub struct Compiler<'prog> {
    curr_reg: Register,
    curr_addr: Address,
    objs: HashMap<&'prog str, Object<'prog>>,
    vars: Vec<(&'prog str, Register)>,
    upvalues: Vec<(&'prog str, Register)>,
    scopes: Vec<Register>,
    procedures: HashMap<&'prog str, Procedure<'prog>>,
    main: Option<Procedure<'prog>>,
}

impl<'prog> Compiler<'prog> {
    pub fn new() -> Self {
        Self {
            curr_reg: START_REGISTER,
            curr_addr: START_ADDRESS,
            objs: HashMap::new(),
            vars: Vec::new(),
            upvalues: Vec::new(),
            scopes: Vec::new(),
            procedures: HashMap::new(),
            main: None,
        }
    }

    pub fn compile(
        &mut self,
        program: &'prog Program<'prog>,
    ) -> Result<Executable<'prog>, Box<dyn Error>> {
        // Tallies all procedures to be defined
        for item in &program.items {
            match &item.0 {
                ProgramItem::Proc {
                    name,
                    params: _,
                    ret_type,
                    body: _,
                } => {
                    if *name == "main" {
                        continue;
                    }

                    let r_type = match ret_type {
                        Some((type_, _)) => Some(type_.clone()),
                        None => None,
                    };

                    self.procedures.insert(name, Procedure::new(r_type))
                }
            };
        }

        // Compile procedures
        for item in &program.items {
            match &item.0 {
                ProgramItem::Proc {
                    name,
                    params,
                    ret_type: _,
                    body,
                } => {
                    self.compile_procedure(name, params, body)?;
                }
            };
        }

        // Merge procedure instructions to executable
        let mut compiled_instrs = Vec::new();
        let mut labels: HashMap<&'prog str, Address> = HashMap::new();

        if let Some(main) = &mut self.main {
            compiled_instrs.append(&mut main.instrs);
        } else {
            return Err(Box::new(io::Error::new(
                io::ErrorKind::Other,
                "Main procedure doesn't exists.",
            )));
        }

        for (name, procedure) in self.procedures.iter_mut() {
            labels.insert(*name, compiled_instrs.len());
            compiled_instrs.append(&mut procedure.instrs);
        }

        for (name, object) in self.objs.iter_mut() {
            let mut obj_instrs;

            match object {
                Object::Function {
                    address: _,
                    capacity: _,
                    upvalues: _,
                    instructions,
                } => {
                    obj_instrs = instructions;
                }
            }

            labels.insert(*name, compiled_instrs.len());
            compiled_instrs.append(&mut obj_instrs);
        }

        let executable = Executable::new(labels, compiled_instrs);

        Ok(executable)
    }

    fn compile_procedure(
        &mut self,
        name: &&'prog str,
        params: &Vec<Spanned<(&'prog str, Type)>>,
        body: &Vec<Spanned<Stmt<'prog>>>,
    ) -> Result<(), Box<dyn Error>> {
        // Move curr register ptr at first register
        self.init_compiler();

        let context = CompileContext::new();

        // Throws an error if a main procedure contains parameters
        if *name == "main" && params.len() != 0 {
            return Err(Box::new(io::Error::new(
                io::ErrorKind::Other,
                "Main procedure should not have parameters.",
            )));
        }

        // Define parameters at first couple of registers
        for ((param, _), _) in params {
            self.define_var(param, self.curr_reg.clone())?;
            self.curr_reg += 1;
        }

        // Compile procedure body
        let body_instrs = self.compile_block(&context, body)?;

        // Add lable to procedure instructions
        let label_instr = vec![Instr::Label(name)];

        let return_instrs;

        // If compiling a main procedure, end the procedure instruction block with an instruction to halt program execution
        // Otherwise, end the procedure instruction block with an instruction to return to caller
        if *name == "main" {
            return_instrs = vec![Instr::HaltSuccess];
        } else {
            return_instrs = vec![Instr::JmpToCaller];
        }

        // Compile instructions
        let compiled_instrs =
            self.compile_instrs_blocks(vec![label_instr, body_instrs, return_instrs]);

        // Create new procedure struct
        let mut new_procedure = Procedure::new(None);
        new_procedure.load_instrs(compiled_instrs);

        if *name == "main" {
            self.main = Some(new_procedure);
        } else {
            self.procedures.insert(name, new_procedure);
        }

        Ok(())
    }

    fn compile_stmt(
        &mut self,
        context: &CompileContext,
        ast: &Spanned<Stmt<'prog>>,
    ) -> Result<Vec<Instr<'prog>>, Box<dyn Error>> {
        let mut compiled_instrs = match &ast.0 {
            Stmt::Expr { expr } => {
                let instrs = self.compile_expr(context, expr)?;
                self.curr_reg -= 1;
                instrs
            }
            Stmt::Block(block_items) => self.compile_block(&CompileContext::new(), block_items)?,
            Stmt::Return { value } => {
                let mut ret_instrs = Vec::new();

                if let Some(ret_val) = value {
                    let mut instrs = self.compile_expr(context, ret_val)?;
                    ret_instrs.append(&mut instrs);
                }

                self.curr_reg -= 1;

                ret_instrs.append(&mut vec![
                    Instr::Mov {
                        dest: RETURN_REGISTER,
                        src: self.curr_reg,
                    },
                    Instr::JmpToCaller,
                ]);

                ret_instrs
            }
            Stmt::Println { content } => {
                let mut content_instrs = self.compile_expr(context, content)?;
                self.curr_reg -= 1;
                content_instrs.append(&mut vec![Instr::Println { src: self.curr_reg }]);
                content_instrs
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition_instrs = self.compile_expr(context, condition)?;
                self.curr_reg -= 1;

                let condition_reg = self.curr_reg;

                let then_instrs = self.compile_block(context, then_block)?;
                let mut else_instrs = Vec::new();
                if let Some(e) = else_block {
                    else_instrs = self.compile_block(context, e)?;
                }

                let skip_then_instr = vec![Instr::JmpOnFalse {
                    src: condition_reg,
                    offset: then_instrs.len() + 2,
                }];
                let skip_else_instr = vec![Instr::Jmp {
                    offset: else_instrs.len() + 1,
                }];

                self.compile_instrs_blocks(vec![
                    condition_instrs,
                    skip_then_instr,
                    then_instrs,
                    skip_else_instr,
                    else_instrs,
                ])
            }
            Stmt::VarDecl {
                name,
                var_type: _,
                init,
            } => {
                let init_instrs = match init {
                    Some(init_expr) => self.compile_expr(context, init_expr)?,
                    None => Vec::new(),
                };

                if let Some(_) = self.get_var(name) {
                    return Err(Box::new(io::Error::new(
                        io::ErrorKind::Other,
                        "Variable already initialized",
                    )));
                }

                self.define_var(name, self.curr_reg - 1)?;

                init_instrs
            }
            Stmt::FnDecl {
                name,
                params,
                ret_type: _,
                body,
            } => {
                // Declare a new scope over the function definition
                self.begin_scope();

                self.upvalues = Vec::new();

                self.define_var(name, self.curr_reg)?;
                self.curr_reg += 1;

                let fn_addr = self.curr_addr;

                // Define parameters at first couple of registers
                for ((param, _), _) in params {
                    self.define_var(param, self.curr_reg.clone())?;
                    self.curr_reg += 1;
                }

                // Compile function body
                let body_instrs = self.compile_block(&context.clone().fn_mode(), body)?;

                // Add label to function instructions
                let label_instr = vec![Instr::Label(name)];

                let return_instr = vec![Instr::JmpToCaller];

                // Generate instructions to store the function's upvalues
                let store_upvalues_instrs = self.save_upvalues(&fn_addr);

                self.end_scope();

                let define_fn_val_instrs = vec![Instr::LoadAddr {
                    dest: self.curr_reg,
                    addr: fn_addr,
                }];
                self.curr_reg += 1;

                // Compile instructions
                let fn_instrs =
                    self.compile_instrs_blocks(vec![label_instr, body_instrs, return_instr]);

                let compiled_instrs =
                    self.compile_instrs_blocks(vec![store_upvalues_instrs, define_fn_val_instrs]);

                let fn_obj = Object::Function {
                    address: fn_addr,
                    capacity: self.upvalues.len(),
                    upvalues: self.upvalues.clone(),
                    instructions: fn_instrs,
                };

                self.objs.insert(name, fn_obj);

                compiled_instrs
            }
        };

        let mut base_instrs = Vec::new();
        base_instrs.append(&mut compiled_instrs);

        Ok(base_instrs)
    }

    fn compile_block(
        &mut self,
        context: &CompileContext,
        block: &Vec<Spanned<Stmt<'prog>>>,
    ) -> Result<Vec<Instr<'prog>>, Box<dyn Error>> {
        let mut compiled_instrs = Vec::new();

        if context.is_generate_scope {
            self.begin_scope();
        }

        for stmt in block {
            let mut instrs = self.compile_stmt(context, stmt)?;

            compiled_instrs.append(&mut instrs);
        }

        if context.is_generate_scope {
            self.end_scope();
        }

        Ok(compiled_instrs)
    }

    fn compile_expr(
        &mut self,
        context: &CompileContext,
        expr: &Spanned<Expr<'prog>>,
    ) -> Result<Vec<Instr<'prog>>, Box<dyn Error>> {
        match &expr.0 {
            Expr::Val(v) => {
                self.curr_reg += 1;

                Ok(vec![Instr::LoadI {
                    dest: self.curr_reg - 1,
                    val: v.clone(),
                }])
            }
            Expr::Ident(var) => {
                if let Some((var_name, src)) = self.get_var(var) {
                    let res;

                    if context.is_save_upvalues && self.is_nonlocal(var_name) {
                        println!("{}", var_name);
                        self.add_upvalue(var_name, *src);
                        res = vec![Instr::Load {
                            dest: self.curr_reg,
                            addr: self.curr_addr,
                        }];

                        self.curr_addr += 1;
                    } else {
                        res = vec![Instr::Mov {
                            dest: self.curr_reg,
                            src: *src,
                        }];
                    }

                    self.curr_reg += 1;

                    return Ok(res);
                }
                Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    "Undefined variable",
                )))
            }
            Expr::Add(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::Add {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::Sub(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::Sub {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::Mul(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::Mul {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::Div(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::Div {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::Neg(expr1) => {
                let mut instrs = self.compile_expr(context, &expr1)?;

                let curr_reg = self.curr_reg - 1;

                instrs.append(&mut vec![
                    Instr::LoadI {
                        dest: curr_reg + 1,
                        val: Value::Num(2.0),
                    },
                    Instr::Mul {
                        dest: curr_reg + 1,
                        src1: curr_reg,
                        src2: curr_reg + 1,
                    },
                    Instr::Sub {
                        dest: curr_reg,
                        src1: curr_reg,
                        src2: curr_reg + 1,
                    },
                ]);

                Ok(instrs)
            }
            Expr::Gret(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::Gret {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::GretEq(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::GretEq {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::Less(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::Less {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::LessEq(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::LessEq {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::Eq(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::Eq {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::NotEq(expr1, expr2) => {
                let mut instrs = self.compile_expr(context, &expr1)?;
                instrs.append(&mut self.compile_expr(context, &expr2)?);

                self.curr_reg -= 1;

                instrs.append(&mut vec![Instr::NotEq {
                    dest: self.curr_reg - 1,
                    src1: self.curr_reg - 1,
                    src2: self.curr_reg,
                }]);

                Ok(instrs)
            }
            Expr::Call { name, args } => {
                let return_exists;

                // Check if callee is a function
                if let Some(_) = self.get_var(name) {
                    if let Some(obj) = self.get_obj(name) {
                        if let Object::Function {
                            address: _,
                            capacity: _,
                            upvalues: _,
                            instructions: _,
                        } = obj
                        {
                            return_exists = true;
                        } else {
                            return Err(Box::new(io::Error::new(
                                io::ErrorKind::Other,
                                "Fatal error: Attempting to call a non-function",
                            )));
                        }
                    } else {
                        return Err(Box::new(io::Error::new(
                            io::ErrorKind::Other,
                            "Fatal error: Attempting to call a non-function",
                        )));
                    }
                }
                // Check if callee is a procedure
                else if let Some((_, proc)) = self.procedures.get_key_value(name) {
                    match proc.ret_type {
                        Some(_) => return_exists = true,
                        None => return_exists = false,
                    }
                } else {
                    return Err(Box::new(io::Error::new(
                        io::ErrorKind::Other,
                        "Undefined callee",
                    )));
                }

                let mut arg_instrs = Vec::new();

                // Saves the last register being pointed by curr register ptr
                // Before entering call
                let arg_reg_start = self.curr_reg;
                println!("{}", arg_reg_start);

                // Evaluate call arguments
                for arg in args {
                    let mut instrs = self.compile_expr(context, arg)?;
                    arg_instrs.append(&mut instrs);
                }

                // Save caller values to stack
                let locals_count = &arg_reg_start;
                let save_instrs = self.save_local_values(locals_count);

                // Move argument values to parameter registers
                let mut move_arg_instrs = Vec::new();

                for i in 0..args.len() {
                    move_arg_instrs.push(Instr::Mov {
                        dest: i + 1,
                        src: arg_reg_start + i,
                    });
                }

                // Jump to callee; also saves program counter at after jump instruction
                let jump_instrs = vec![
                    Instr::PushStackPC { offset: 2 },
                    Instr::JmpLabel { label: name },
                ];

                // Load locals after call
                let load_instrs = self.load_local_values(locals_count);

                self.curr_reg = arg_reg_start;

                // Move return value to current register
                let mut get_ret_val_instrs = Vec::new();
                if return_exists {
                    get_ret_val_instrs = vec![Instr::Mov {
                        dest: self.curr_reg,
                        src: RETURN_REGISTER,
                    }];
                }

                self.curr_reg += 1;

                Ok(self.compile_instrs_blocks(vec![
                    arg_instrs,
                    save_instrs,
                    move_arg_instrs,
                    jump_instrs,
                    load_instrs,
                    get_ret_val_instrs,
                ]))
            }
            Expr::Error => panic!("Fatal error: Attempting to compile an error expression"),
        }
    }

    fn compile_instrs_blocks(&self, instrs_blocks: Vec<Vec<Instr<'prog>>>) -> Vec<Instr<'prog>> {
        return instrs_blocks
            .into_iter()
            .flat_map(|instrs| instrs.into_iter())
            .collect::<Vec<_>>();
    }

    fn save_upvalues(&self, start_addr: &Address) -> Vec<Instr<'prog>> {
        let mut save_upvals_instrs = Vec::new();

        let mut store_addr = *start_addr;

        for (_, reg) in self.upvalues.iter() {
            save_upvals_instrs.push(Instr::Store {
                src: *reg,
                addr: store_addr,
            });
            store_addr += 1;
        }

        return save_upvals_instrs;
    }

    fn add_upvalue(&mut self, name: &'prog str, reg: Register) {
        self.upvalues.push((name, reg));
    }

    // Returns true if a variable is initialized in an outer scope, else returns false.
    fn is_nonlocal(&self, name: &'prog str) -> bool {
        let inner_scope = self
            .scopes
            .last()
            .expect("Fatal error: Cannot peek from an empty scopes stack.");

        for (var_name, reg_pos) in self.vars.iter().rev() {
            if &name == var_name && inner_scope > reg_pos {
                return true;
            }
        }

        false
    }

    fn begin_scope(&mut self) {
        self.scopes.push(self.curr_reg);
    }

    fn end_scope(&mut self) {
        self.curr_reg = self
            .scopes
            .pop()
            .expect("Fatal error: Cannot pop from an empty scopes stack");
    }

    fn save_local_values(&self, count: &usize) -> Vec<Instr<'prog>> {
        let mut save_locals_instrs = Vec::new();

        for reg in START_REGISTER..*count {
            save_locals_instrs.push(Instr::PushStack { src: reg });
        }

        return save_locals_instrs;
    }

    fn load_local_values(&self, count: &usize) -> Vec<Instr<'prog>> {
        let mut load_locals_instrs = Vec::new();

        for reg in START_REGISTER..*count {
            load_locals_instrs.push(Instr::PopStack { dest: reg });
        }

        return load_locals_instrs;
    }

    fn get_obj(&self, name: &'prog str) -> Option<&Object<'prog>> {
        self.objs.get(name)
    }

    fn get_var(&self, name: &'prog str) -> Option<&(&'prog str, Register)> {
        for value in self.vars.iter().rev() {
            if value.0 == name {
                return Some(value);
            }
        }

        None
    }

    fn define_var(&mut self, name: &'prog str, reg: Register) -> Result<(), Box<dyn Error>> {
        if self.procedures.contains_key(name) {
            return Err(Box::new(io::Error::new(
                io::ErrorKind::Other,
                "Identifier already being used by a procedure.",
            )));
        }

        self.vars.push((name, reg));

        Ok(())
    }

    fn init_compiler(&mut self) {
        self.curr_reg = START_REGISTER;
        self.vars = Vec::new();
    }
}
