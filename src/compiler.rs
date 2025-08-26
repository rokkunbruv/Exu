use chumsky::label;

use crate::ast::{Expr, Global, Stmt};
use crate::executable::Executable;
use crate::instruction::Instr;
use crate::object::{Func, Object};
use crate::types::Type;
use crate::value::Value;
use crate::vm::{Address, Register};
use crate::Program;
use crate::Spanned;
use std::collections::HashMap;
use std::error::Error;
use std::io;

/// The register index to store the return value of a global_fn/function/action call
const RETURN_REGISTER: Register = 0;
// The starting register index to store values
const START_REGISTER: Register = 1;

// The register where the function object will be stored during a function call
const FN_OBJ_REGISTER: Register = 1;

// The starting address index to store objects
const START_ADDRESS: Address = 0;

// A placeholder address that will be changed later
const PLACEHOLDER_ADDRESS: Address = 0;

/// Context object to keep track of compilation flags
/// These compilation flags allow the compiler to behave differently
/// when compiling certain objects or statements
#[derive(Debug, Clone)]
struct CompileContext {
    /// When true, keeps track of any upvalues to the compiler's upvalues vector
    pub enable_save_upvalues: bool,
    /// When true, generates a new scope when entering a block
    pub enable_generate_scope: bool,
}

impl CompileContext {
    pub fn new() -> Self {
        Self {
            enable_save_upvalues: false,
            enable_generate_scope: true,
        }
    }

    /// Configures compiler context when compiling local functions
    /// In compiling functions, upvalues are to be tracked and scopes aren't generated around function body block
    pub fn local_fn_mode(mut self) -> Self {
        self.enable_save_upvalues = true;
        self.enable_generate_scope = false;
        self
    }
}

/// Stores information to resolve labels with their addresses in the instruction list
/// To store the addresses of the labels to the heap
struct LabelFixup {
    /// The label to be resolved
    pub label: String,
    /// The register where the label address will be stored to
    pub load_reg: Register,
    /// The offset position of the load label address instruction with the placeholder address
    /// in which it will be replaced when the label address is resolved
    pub load_label_addr_instr_offset: usize,
}

impl LabelFixup {
    pub fn new(label: String, load_reg: Register, load_label_addr_instr_offset: usize) -> Self {
        Self {
            label,
            load_reg,
            load_label_addr_instr_offset,
        }
    }

    /// Helper function to update the offset position of the load instruction
    /// To be used in blocks
    pub fn adjust_load_instr_offset(&mut self, additional_offset: usize) {
        self.load_label_addr_instr_offset += additional_offset;
    }
}

pub struct Compiler<'prog> {
    /// The current register index being tracked
    curr_reg: Register,

    /// The current heap index being tracked
    curr_addr: Address,

    /// Tracks all declared local objects inside a global function
    local_objs: Vec<Object<'prog>>,

    /// Tracks all declared variables inside a global function
    vars: Vec<(&'prog str, Register)>,

    /// Tracks all upvalues being accessed by a local function
    upvalues: Vec<(&'prog str, Register)>,

    /// Stack of defined scopes
    scopes: Vec<Register>,

    /// Stack of function calls
    call_stack: Vec<Register>,

    /// Tracks all declared global objects
    globals: HashMap<&'prog str, Object<'prog>>,

    /// The main global function
    main: Option<Func<'prog>>,

    /// Tracks all function labels to be resolved
    fn_label_fixups: HashMap<&'prog str, LabelFixup>,
}

impl<'prog> Compiler<'prog> {
    pub fn new() -> Self {
        Self {
            curr_reg: START_REGISTER,
            curr_addr: START_ADDRESS,
            local_objs: Vec::new(),
            vars: Vec::new(),
            upvalues: Vec::new(),
            scopes: Vec::new(),
            call_stack: Vec::new(),
            globals: HashMap::new(),
            main: None,
            fn_label_fixups: HashMap::new(),
        }
    }

    /// Converts an AST into a vector of bytecode instructions
    pub fn compile(
        &mut self,
        program: &'prog Program<'prog>,
    ) -> Result<Executable<'prog>, Box<dyn Error>> {
        // Tallies all globals to be defined
        for item in &program.items {
            match &item.0 {
                Global::Fn {
                    name,
                    params: _,
                    ret_type,
                    body: _,
                } => {
                    // Skip the main global function since it doesn't need to be tallied
                    if *name == "main" {
                        continue;
                    }

                    let r_type = match ret_type {
                        Some((type_, _)) => Some(type_.clone()),
                        None => None,
                    };

                    self.globals.insert(
                        name,
                        Object::Function(Func::new(
                            name,
                            name.to_string(),
                            r_type,
                            None,
                            self.curr_addr,
                        )),
                    );

                    self.curr_addr += 1;
                    // Don't pass the instructions yet since they will be added once they are generated
                }
            };
        }

        // Compile globals
        for item in &program.items {
            match &item.0 {
                Global::Fn {
                    name,
                    params,
                    ret_type,
                    body,
                } => {
                    self.compile_global_fn(name, params, ret_type, body)?;
                }
            };
        }

        // Merge globals instructions to executable instructions
        let mut compiled_instrs = Vec::new();
        let mut globals_labels: HashMap<String, Address> = HashMap::new();

        // Append the main function first so that the VM will execute it first
        if let Some(main) = &mut self.main {
            compiled_instrs.append(&mut main.instructions);
        } else {
            return Err(Box::new(io::Error::new(
                io::ErrorKind::Other,
                "Main global function doesn't exists.",
            )));
        }

        // Append the global objects after the main function
        for (_, global) in self.globals.iter_mut() {
            match global {
                Object::Function(func) => {
                    globals_labels.insert(func.name.to_string(), compiled_instrs.len());
                    compiled_instrs.append(&mut func.instructions);
                }
            }
        }

        let mut fn_obj_labels: HashMap<String, Address> = HashMap::new();

        for object in self.local_objs.iter_mut() {
            let mut obj_instrs;

            match object {
                Object::Function(func) => {
                    obj_instrs = func.instructions.clone();
                    fn_obj_labels.insert(func.label.clone(), compiled_instrs.len());
                }
            }

            compiled_instrs.append(&mut obj_instrs);
        }

        // Resolve the labels of the function objects
        for (_, label_fixup) in self.fn_label_fixups.iter() {
            let address;
            match fn_obj_labels.get(&label_fixup.label) {
                Some(addr) => address = addr,
                None => {
                    return Err(Box::new(io::Error::new(
                        io::ErrorKind::Other,
                        "Fatal error: Cannot resolve address of function object label.",
                    )))
                }
            };

            // Adding 1 to the instruction offset takes into account the label instruction
            // of the global function
            compiled_instrs[label_fixup.load_label_addr_instr_offset] = Instr::LoadAddr {
                dest: label_fixup.load_reg,
                addr: *address,
            };
        }

        let executable = Executable::new(globals_labels, compiled_instrs);

        Ok(executable)
    }

    /// Compiles global functions
    fn compile_global_fn(
        &mut self,
        name: &&'prog str,
        params: &Vec<Spanned<(&'prog str, Type)>>,
        ret_type: &Option<Spanned<Type>>,
        body: &Vec<Spanned<Stmt<'prog>>>,
    ) -> Result<(), Box<dyn Error>> {
        // Clear the register file and scopes
        self.init_compiler();

        let context = CompileContext::new();

        // Throw an error if the main function contains parameters or returns a value
        if *name == "main" && params.len() != 0 {
            if params.len() != 0 {
                return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    "Main global function should not have parameters.",
                )));
            }

            if let None = ret_type {
                return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    "Main global function should not return a value.",
                )));
            }
        }

        let label = name.to_string();

        let global_fn_addr = self.curr_addr;

        // Define parameters at first couple of registers
        for ((param, _), _) in params {
            self.define_var(param, self.curr_reg.clone())?;
            self.curr_reg += 1;
        }

        // Compile global function body
        let body_instrs = self.compile_block(&context, body)?;

        // Add label to global function instructions
        let label_instr = vec![Instr::Label(label)];

        let return_instrs;

        // If compiling a main global function, end the global function instruction block with an instruction to halt program execution
        // Otherwise, end the global function instruction block with an instruction to return to caller
        if *name == "main" {
            return_instrs = vec![Instr::HaltSuccess];
        } else {
            return_instrs = vec![Instr::JmpToCaller];
        }

        // let define_fn_val_instrs = vec![
        //     Instr::LoadAddr {
        //         dest: self.curr_reg,
        //         addr: PLACEHOLDER_ADDRESS,
        //     },
        //     Instr::Store {
        //         src: self.curr_reg,
        //         addr: global_fn_addr,
        //     },
        // ];

        // if name != "main" {
        //     let label_fixup = LabelFixup::new(
        //     label.clone(),
        //     self.curr_reg,
        //     store_upvalues_instrs.len() + 1,
        // );
        // self.fn_label_fixups.insert(name, label_fixup);
        // }

        // Compile instructions
        let compiled_instrs =
            self.compile_instrs_blocks(vec![label_instr, body_instrs, return_instrs]);

        let r_type = match ret_type {
            Some((type_, _)) => Some(type_.clone()),
            None => None,
        };

        // Create new global function object
        let new_global_fn = Func::new(
            name,
            name.to_string(),
            r_type,
            Some(compiled_instrs),
            self.curr_addr,
        );

        self.curr_addr += 1;

        // Declare main function if found
        if *name == "main" {
            self.main = Some(new_global_fn);
        }
        // Else declare global function in global scope
        else {
            let global_fn_obj = Object::Function(new_global_fn);
            self.globals.insert(name, global_fn_obj);
        }

        Ok(())
    }

    /// Compiles statements
    fn compile_stmt(
        &mut self,
        context: &CompileContext,
        ast: &Spanned<Stmt<'prog>>,
    ) -> Result<Vec<Instr<'prog>>, Box<dyn Error>> {
        let mut compiled_instrs = match &ast.0 {
            Stmt::Expr { expr } => {
                let instrs = self.compile_expr(context, expr)?;
                self.curr_reg -= 1; // Ignore the evaluated value of the expression since statements don't save values
                instrs
            }
            Stmt::Block(block_items) => self.compile_block(&CompileContext::new(), block_items)?, // Create a new context around the block since contexts don't apply to nested blocks
            Stmt::Return { value } => {
                let mut ret_instrs = Vec::new();

                // Evaluate return expression
                if let Some(ret_val) = value {
                    let mut instrs = self.compile_expr(context, ret_val)?;
                    ret_instrs.append(&mut instrs);
                }
                self.curr_reg -= 1;

                // Save the return value and go back to the caller function
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
                // Evaluate condition
                let condition_instrs = self.compile_expr(context, condition)?;
                self.curr_reg -= 1;

                let condition_reg = self.curr_reg;

                // Compile then and else blocks
                let then_instrs = self.compile_block(&CompileContext::new(), then_block)?;
                let mut else_instrs = Vec::new();
                if let Some(e) = else_block {
                    else_instrs = self.compile_block(&CompileContext::new(), e)?;
                }

                // Generate instructions to skip then and else blocks
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
                // Evaluate initializer
                let init_instrs = match init {
                    Some(init_expr) => self.compile_expr(context, init_expr)?,
                    None => Vec::new(),
                };

                // Throw an error if the variable has already been defined
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
                ret_type,
                body,
            } => todo!(),
        };

        let mut base_instrs = Vec::new();
        base_instrs.append(&mut compiled_instrs);

        Ok(base_instrs)
    }

    /// Compiles blocks
    fn compile_block(
        &mut self,
        context: &CompileContext,
        block: &Vec<Spanned<Stmt<'prog>>>,
    ) -> Result<Vec<Instr<'prog>>, Box<dyn Error>> {
        let mut compiled_instrs = Vec::new();

        if context.enable_generate_scope {
            self.begin_scope();
        }

        for stmt in block {
            let mut instrs = self.compile_stmt(context, stmt)?;

            // Update the instruction offset of the label fixup after compiling a function declaration
            if let Stmt::FnDecl {
                name,
                params: _,
                ret_type: _,
                body: _,
            } = stmt.0
            {
                match self.fn_label_fixups.get_mut(name) {
                    Some(label_fixup) => {
                        label_fixup.adjust_load_instr_offset(compiled_instrs.len())
                    }
                    None => {
                        return Err(Box::new(io::Error::new(
                            io::ErrorKind::Other,
                            "Fatal error: Attempting to access an undefined label fixup.",
                        )))
                    }
                }
            }

            compiled_instrs.append(&mut instrs);
        }

        if context.enable_generate_scope {
            self.end_scope();
        }

        Ok(compiled_instrs)
    }

    /// Compiles expressions
    fn compile_expr(
        &mut self,
        context: &CompileContext,
        expr: &Spanned<Expr<'prog>>,
    ) -> Result<Vec<Instr<'prog>>, Box<dyn Error>> {
        match &expr.0 {
            Expr::Val(v) => {
                // Loads the value to the current register and move to the next empty register
                self.curr_reg += 1;

                Ok(vec![Instr::LoadI {
                    dest: self.curr_reg - 1,
                    val: v.clone(),
                }])
            }
            Expr::Ident(var) => {
                // Check if the variable is defined in scope
                if let Some((var_name, src)) = self.get_var(var) {
                    let res;

                    // If the variable is an upvalue, load them from the heap
                    if context.enable_save_upvalues && self.is_nonlocal(var_name) {
                        self.add_upvalue(var_name, *src);
                        res = vec![Instr::Load {
                            dest: self.curr_reg,
                            addr: self.curr_addr,
                        }];

                        self.curr_addr += 1;
                    }
                    // Else access them to where they are stored in the register file
                    else {
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
                // This track whether the callee returns a value or not (true if it returns a value)
                let return_exists;

                let load_fn_obj_instr;

                // Check if callee is a function
                if let Some((_, reg)) = self.get_var(name) {
                    return_exists = true;

                    load_fn_obj_instr = vec![Instr::Mov {
                        dest: FN_OBJ_REGISTER,
                        src: *reg,
                    }]
                } else if let Some(global) = self.get_global(name) {
                    match global {
                        // Check if the callee is a global function
                        Object::Function(global_fn) => {
                            match global_fn.ret_type {
                                Some(_) => return_exists = true,
                                None => return_exists = false,
                            };
                            load_fn_obj_instr = vec![Instr::LoadAddr {
                                dest: FN_OBJ_REGISTER,
                                addr: global_fn.address,
                            }]
                        }
                    }
                } else {
                    return Err(Box::new(io::Error::new(
                        io::ErrorKind::Other,
                        "Undefined callee",
                    )));
                }

                let mut arg_instrs = Vec::new();

                // Saves the last register being pointed by current register pointer before entering the call
                let arg_reg_start = self.curr_reg;

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
                        dest: FN_OBJ_REGISTER + i + 1,
                        src: arg_reg_start + i,
                    });
                }

                self.use_curr_reg();

                // Jump to callee; also saves program counter at after jump instruction
                let jump_instrs = vec![
                    Instr::PushStackPC { offset: 2 },
                    Instr::JmpAddr {
                        src: FN_OBJ_REGISTER,
                    },
                ];

                // Load locals after call
                let load_instrs = self.load_local_values(locals_count);

                self.curr_reg = arg_reg_start;

                // Move return value to current register
                let mut get_ret_val_instrs = Vec::new();
                if return_exists {
                    get_ret_val_instrs = vec![Instr::Mov {
                        dest: self.use_curr_reg(),
                        src: RETURN_REGISTER,
                    }];
                }

                Ok(self.compile_instrs_blocks(vec![
                    arg_instrs,
                    save_instrs,
                    load_fn_obj_instr,
                    move_arg_instrs,
                    jump_instrs,
                    load_instrs,
                    get_ret_val_instrs,
                ]))
            }
            Expr::Error => panic!("Fatal error: Attempting to compile an error expression"),
        }
    }

    fn compile_function(
        &mut self,
        context: &CompileContext,
        name: &'prog str,
        params: &Vec<Spanned<(&'prog str, Type)>>,
        ret_type: &Option<Spanned<Type>>,
        body: &Vec<Spanned<Stmt<'prog>>>,
    ) -> Result<Vec<Instr<'prog>>, Box<dyn Error>> {
        for scope in &self.scopes {
            print!("{} ", scope);
        }
        println!();
        println!();
        for (name, reg) in &self.vars {
            print!("{}-{}", name, reg);
        }
        println!();
        println!();

        // Declare a new scope over the function definition
        self.begin_call();

        // Initialize upvalues
        self.upvalues = Vec::new();

        // Set label of the function body
        // This will be used to create the label instruction and the label fixup
        let label = format!("{}_fn_obj", name);

        // Define the function within its scope (to allow calling itself within its body)
        self.define_var(name, self.curr_reg)?;
        self.use_curr_reg();

        // Saves the address where the function will be stored in the heap
        // This is because the current address pointer will change as the function
        // body encounters upvalues
        let fn_addr = self.use_curr_addr();

        // Define parameters at first couple of registers
        for ((param, _), _) in params {
            let curr_reg = self.use_curr_reg();
            self.define_var(param, curr_reg)?;
        }

        // Compile function body
        let body_instrs = self.compile_block(&context.clone().local_fn_mode(), body)?;

        // Add label to function instructions
        let label_instr = vec![Instr::Label(label.clone())];

        let return_instr = match ret_type {
            Some(_) => Vec::new(),
            None => vec![Instr::JmpToCaller],
        };

        // Generate instructions to store the function's upvalues
        let store_upvalues_instrs = self.save_upvalues(&fn_addr);

        self.end_call();

        for scope in &self.scopes {
            print!("{} ", scope);
        }
        println!();
        println!();

        let define_fn_val_instrs = vec![
            Instr::LoadAddr {
                dest: self.curr_reg,
                addr: PLACEHOLDER_ADDRESS,
            },
            Instr::Store {
                src: self.curr_reg,
                addr: fn_addr,
            },
        ];

        // Create the label fixup object to be used for label address resolution
        let label_fixup = LabelFixup::new(
            label.clone(),
            self.use_curr_reg(),
            store_upvalues_instrs.len() + 1,
        );
        self.fn_label_fixups.insert(name, label_fixup);

        // Compile instructions

        // These are the instructions for the function's body
        let fn_instrs = self.compile_instrs_blocks(vec![label_instr, body_instrs, return_instr]);

        // These are instructions to initialize the function object and store them to the heap
        let compiled_instrs;

        // When compiling the main function, it is not necessary to initialize the function object
        // as it will be automatically executed during runtime
        compiled_instrs =
            self.compile_instrs_blocks(vec![store_upvalues_instrs, define_fn_val_instrs]);

        let r_type = match ret_type {
            Some((type_, _)) => Some(type_.clone()),
            None => None,
        };

        let func = Func::new(name, label, r_type, Some(fn_instrs), fn_addr);
        let fn_obj = Object::Function(func);

        // Add the function to the objects vector
        self.local_objs.push(fn_obj.clone());

        // Define the local function variable
        self.define_var(name, self.curr_reg - 1)?;

        Ok(compiled_instrs)
    }

    /// Helper function to combine multiple vectors of instructions into a single instructions vector.
    /// Accepts a vector of instructions vectors, returns an instructions vector.
    // TODO: Convert this to a macro
    fn compile_instrs_blocks(&self, instrs_blocks: Vec<Vec<Instr<'prog>>>) -> Vec<Instr<'prog>> {
        return instrs_blocks
            .into_iter()
            .flat_map(|instrs| instrs.into_iter())
            .collect::<Vec<_>>();
    }

    /// Generates instructions for saving upvalues to the heap.
    /// Accepts the address to store the first upvalue, returns instructions to store the upvalues.
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

    /// Saves a variable to the upvalues vector
    fn add_upvalue(&mut self, name: &'prog str, reg: Register) {
        self.upvalues.push((name, reg));
    }

    /// Accepts a variable name, returns true if a variable is initialized in an outer scope else returns false.
    fn is_nonlocal(&self, name: &'prog str) -> bool {
        // Obtain the starting index of the localmost scope
        let inner_scope = self
            .scopes
            .last()
            .expect("Fatal error: Cannot peek from an empty scopes stack.");

        for (var_name, reg_pos) in self.vars.iter().rev() {
            if &name == var_name {
                // Return true if the variable is defined before the localmost scope is set
                if inner_scope > reg_pos {
                    return true;
                } else {
                    break;
                }
            }
        }

        false
    }

    fn begin_call(&mut self) {
        self.begin_scope();
        self.call_stack.push(self.scopes.len());
        self.curr_reg = START_REGISTER;
    }

    fn end_call(&mut self) {
        let scope_from_outer_fn = self
            .call_stack
            .pop()
            .expect("Fatal error: Cannot pop from an empty call stack");

        self.scopes.truncate(scope_from_outer_fn);

        self.end_scope();
    }

    /// Creates a new scope by pushing the current register pointer to the scopes stack.
    /// This register pointer will point to the first variable defined in this new scope.
    fn begin_scope(&mut self) {
        self.scopes.push(self.curr_reg);
    }

    /// Exits the current scope by popping the register pointer of the current scope off the scopes stack.
    fn end_scope(&mut self) {
        self.curr_reg = self
            .scopes
            .pop()
            .expect("Fatal error: Cannot pop from an empty scopes stack");

        self.vars.truncate(self.curr_reg); // Truncate vars stack to get rid of local variables from the previous scope
    }

    /// Generates instructions to store the local variables to the stack.
    /// Accepts the number of local variables to be saved
    fn save_local_values(&self, count: &usize) -> Vec<Instr<'prog>> {
        let mut save_locals_instrs = Vec::new();

        for reg in (START_REGISTER..*count).rev() {
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

    /// Gets the global object from the globals hashmap
    fn get_global(&self, name: &'prog str) -> Option<&Object<'prog>> {
        self.globals.get(name)
    }

    /// Gets the object
    // fn get_obj(&self, name: &'prog str) -> Option<&Object<'prog>> {
    //     for obj in self.local_objs.iter() {
    //         if obj.
    //     }
    // }

    /// Gets the variable from the variables stack
    fn get_var(&self, name: &'prog str) -> Option<&(&'prog str, Register)> {
        for value in self.vars.iter().rev() {
            if value.0 == name {
                return Some(value);
            }
        }

        None
    }

    /// Adds a new variable to the variable stack
    fn define_var(&mut self, name: &'prog str, reg: Register) -> Result<(), Box<dyn Error>> {
        if self.globals.contains_key(name) {
            return Err(Box::new(io::Error::new(
                io::ErrorKind::Other,
                "Identifier already being used by a global function.",
            )));
        }

        self.vars.push((name, reg));

        Ok(())
    }

    /// Helper function to store the value to the current address being pointed
    /// in the heap and move to the next register
    fn use_curr_addr(&mut self) -> Address {
        self.curr_addr += 1;
        self.curr_addr - 1
    }

    /// Helper function to store the value to the current register being pointed
    /// and move to the next register
    fn use_curr_reg(&mut self) -> Register {
        self.curr_reg += 1;
        self.curr_reg - 1
    }

    /// Clears the register pointer and the local variables stack
    fn init_compiler(&mut self) {
        self.curr_reg = START_REGISTER;
        self.vars = Vec::new();
    }
}
