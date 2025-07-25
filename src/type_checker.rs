use crate::ast::{Expr, ProgramItem, Stmt};
use crate::types::Type;
use crate::value::Value;
use crate::Program;
use crate::Spanned;
use std::collections::HashMap;
use std::error::Error;
use std::io;

/// Constant to denote that a block does not contain a return statement
const NO_RETURN_BLOCK: Option<&Type> = None;

#[derive(Clone)]
pub struct TypeChecker<'prog> {
    objs: Vec<(&'prog str, Type)>,
    var_stack: Vec<(&'prog str, Type)>,
    scopes: Vec<usize>,
    procedures: HashMap<&'prog str, Type>,
}

impl<'prog> TypeChecker<'prog> {
    pub fn new() -> Self {
        Self {
            objs: Vec::new(),
            var_stack: Vec::new(),
            scopes: Vec::new(),
            procedures: HashMap::new(),
        }
    }

    pub fn check_type(&mut self, program: &'prog Program<'prog>) -> Result<(), Box<dyn Error>> {
        // Brings all defined procedures to the global scope before performing type checking
        // Perform type checking on procedure definitions
        for (program_item, _) in program.items.iter() {
            match program_item {
                ProgramItem::Proc {
                    name,
                    params,
                    ret_type,
                    body: _,
                } => {
                    let params_type = params
                        .iter()
                        .map(|((_, type_), _)| type_.clone())
                        .collect::<Vec<_>>();

                    let r_type = match ret_type {
                        Some((type_, _)) => Some(type_.clone()),
                        None => None,
                    };

                    self.define_procedure_type(name, params_type, r_type);
                }
            }
        }

        // Perform type checking on procedure definitions
        for (program_item, _) in program.items.iter() {
            match program_item {
                ProgramItem::Proc {
                    name: _,
                    params,
                    ret_type,
                    body,
                } => {
                    self.init_objs();

                    for ((param_name, param_type), _) in params {
                        self.define_var_type(param_name, param_type.clone());
                    }

                    let mut expected_ret_type = NO_RETURN_BLOCK;
                    if let Some((r, _)) = ret_type {
                        expected_ret_type = Some(r);
                    }

                    self.check_block_type(body, expected_ret_type)?;
                }
            }
        }

        Ok(())
    }

    fn check_block_type(
        &mut self,
        block: &'prog Vec<Spanned<Stmt<'prog>>>,
        expected_ret_type: Option<&Type>,
    ) -> Result<(), Box<dyn Error>> {
        self.begin_scope();

        let mut block_iter = block.iter().peekable();

        while let Some(stmt) = block_iter.next() {
            if let Stmt::Return { value } = &stmt.0 {
                self.check_ret_stmt(value, expected_ret_type)?;

                // Returns an error if the return statement is not the last statement in the block
                if !block_iter.peek().is_none() {
                    return Err(Box::new(io::Error::new(
                        io::ErrorKind::Other,
                        "Unreachable code below. Return statements must not be followed by more statements in the block body.",
                    )));
                }

                break;
            }

            self.check_stmt_type(stmt, expected_ret_type)?;
        }

        self.end_scope();

        Ok(())
    }

    fn check_stmt_type(
        &mut self,
        stmt: &'prog Spanned<Stmt<'prog>>,
        expected_ret_type: Option<&Type>,
    ) -> Result<(), Box<dyn Error>> {
        match &stmt.0 {
            Stmt::Expr { expr } => self.check_expr_type(expr)?,
            Stmt::Block(block) => self.check_block_type(block, expected_ret_type)?,
            Stmt::Println { content } => self.check_expr_type(content)?,
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition_type = self.get_expr_type(condition)?;
                expect_type(vec![Type::Bool], &condition_type)?;
                self.check_block_type(then_block, expected_ret_type)?;
                if let Some(e) = else_block {
                    self.check_block_type(e, expected_ret_type)?;
                }
            }
            Stmt::VarDecl {
                name,
                var_type: (var_type, _),
                init,
            } => {
                if let Some(init_expr) = init {
                    let init_type = self.get_expr_type(init_expr)?;
                    expect_type(vec![var_type.clone()], &init_type)?;
                }

                self.define_var_type(name, var_type.clone());
            }
            Stmt::FnDecl {
                name,
                params,
                ret_type: (ret_type, _),
                body,
            } => {
                let mut params_type = Vec::new();

                for ((param_name, param_type), _) in params {
                    self.define_var_type(param_name, param_type.clone());
                    params_type.push(param_type.clone());
                }

                self.check_block_type(body, Some(ret_type))?;

                let ret_type = Box::new(ret_type.clone());

                let fn_type = Type::Fn {
                    params: params_type,
                    ret_type: ret_type,
                };

                self.define_var_type(name, fn_type);
            }
            Stmt::Return { value: _ } => 
                return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    "Fatal error: Typechecker's check_stmt_type is not supposed to check for return statements as they are already checked in check_block_type.",
                )))
        }

        Ok(())
    }

    fn check_ret_stmt(
        &self,
        ret_val: &Option<Spanned<Expr<'prog>>>,
        expected_type: Option<&Type>,
    ) -> Result<(), Box<dyn Error>> {
        // CASE 1: Return contains a returned expression
        if let Some(ret) = ret_val {
            let actual = self.get_expr_type(ret)?;
            if let Some(expected) = expected_type {
                if expected != &actual {
                    return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    format!("Actual expression type returned doesn't match with the expected return type. Expected {:?}, got {:?}.", expected, actual),
                )));
                }
            } else if let None = expected_type {
                return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    format!("Actual expression type returned doesn't match with the expected return type. Expected None, got {:?}.", actual),
                )));
            }
        }
        // CASE 2: Return doesn't contain an expression
        else {
            if let Some(expected) = expected_type {
                return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    format!("Actual expression type returned doesn't match with the expected return type. Expected {:?}, got None.", expected),
                )));
            }
        }

        Ok(())
    }

    /// Performs type checking on an expression. Returns a type error if there is any.
    fn check_expr_type(&mut self, expr: &Spanned<Expr<'prog>>) -> Result<(), Box<dyn Error>> {
        if let Err(e) = self.get_expr_type(expr) {
            Err(e)
        } else {
            Ok(())
        }
    }

    /// Performs type checking on expressions. If there are no type errors,
    /// returns the resulting type of the expression. Otherwise, returns an error.
    fn get_expr_type(&'prog self, expr: &Spanned<Expr<'prog>>) -> Result<Type, Box<dyn Error>> {
        match &expr.0 {
            Expr::Val(Value::Num(_)) => Ok(Type::Num),
            Expr::Val(Value::Str(_)) => Ok(Type::Str),
            Expr::Val(Value::Bool(_)) => Ok(Type::Bool),
            Expr::Ident(name) => self.get_var_type(name),
            Expr::Call { name, args } => {
                if let Ok(fn_type) = self.get_var_type(name) {
                    self.check_fn_call_type(&fn_type, args)
                } else if let Ok(proc_type) = self.get_procedure_type(*name) {
                    self.check_proc_call_type(proc_type, args)
                } else {
                    Err(Box::new(io::Error::new(
                        io::ErrorKind::Other,
                        "Call object not found",
                    )))
                }
            }

            Expr::Add(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num, Type::Str], &type1, &type2)
            }
            Expr::Sub(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num], &type1, &type2)
            }
            Expr::Mul(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num], &type1, &type2)
            }
            Expr::Div(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num], &type1, &type2)
            }
            Expr::Neg(expr1) => {
                let type_ = self.get_expr_type(&expr1)?;
                expect_type(vec![Type::Num], &type_)
            }
            Expr::Gret(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num], &type1, &type2)?;
                Ok(Type::Bool)
            }
            Expr::GretEq(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num], &type1, &type2)?;
                Ok(Type::Bool)
            }
            Expr::Less(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num], &type1, &type2)?;
                Ok(Type::Bool)
            }
            Expr::LessEq(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num], &type1, &type2)?;
                Ok(Type::Bool)
            }
            Expr::Eq(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num, Type::Str, Type::Bool], &type1, &type2)?;
                Ok(Type::Bool)
            }
            Expr::NotEq(expr1, expr2) => {
                let type1 = self.get_expr_type(&expr1)?;
                let type2 = self.get_expr_type(&expr2)?;
                expect_matched_types(vec![Type::Num, Type::Str, Type::Bool], &type1, &type2)?;
                Ok(Type::Bool)
            }
        }
    }

    fn check_fn_call_type(
        &self,
        fn_type: &'prog Type,
        args: &Vec<Spanned<Expr<'prog>>>,
    ) -> Result<Type, Box<dyn Error>> {
        if let Type::Fn { params, ret_type } = fn_type {
            if params.len() != args.len() {
                return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    "Passed arguments don't match function's parameter count.",
                )));
            }

            for (param_type, arg) in params.iter().zip(args) {
                let arg_type = self.get_expr_type(&arg)?;
                if *param_type != arg_type.clone() {
                    return Err(Box::new(io::Error::new(
                        io::ErrorKind::Other,
                        "Passed arguments don't match function's parameter types",
                    )));
                }
            }

            Ok(*ret_type.clone())
        } else {
            Err(Box::new(io::Error::new(
                io::ErrorKind::Other,
                "Unexpected type on function",
            )))
        }
    }

    fn check_proc_call_type(
        &self,
        proc_type: &'prog Type,
        args: &Vec<Spanned<Expr<'prog>>>,
    ) -> Result<Type, Box<dyn Error>> {
        if let Type::Procedure { params, ret } = proc_type {
            if params.len() != args.len() {
                return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    "Passed arguments don't match procedure's parameter count.",
                )));
            }

            for (param_type, arg) in params.iter().zip(args) {
                let arg_type = self.get_expr_type(&arg)?;
                if *param_type != arg_type.clone() {
                    return Err(Box::new(io::Error::new(
                        io::ErrorKind::Other,
                        "Passed arguments don't match procedure's parameter types",
                    )));
                }
            }

            match ret {
                Some(ret_type) => Ok(*ret_type.clone()),
                None => Ok(Type::None),
            }
        } else {
            Err(Box::new(io::Error::new(
                io::ErrorKind::Other,
                "Unexpected type on procedure",
            )))
        }
    }

    fn define_procedure_type(
        &mut self,
        name: &'prog str,
        params_type: Vec<Type>,
        ret_type: Option<Type>,
    ) {
        self.procedures.insert(
            name,
            Type::Procedure {
                params: params_type,
                ret: match ret_type {
                    Some(t) => Some(Box::new(t)),
                    None => None,
                },
            },
        );
    }

    fn init_objs(&mut self) {
        self.var_stack = Vec::new();
        self.scopes = Vec::new();
    }

    fn begin_scope(&mut self) {
        self.scopes.push(self.var_stack.len());
    }

    fn end_scope(&mut self) {
        let current_scope_marker = self
            .scopes
            .pop()
            .expect("Typecheck error: Cannot pop from an empty scopes stack");
        self.var_stack.truncate(current_scope_marker);
    }

    fn define_var_type(&mut self, name: &'prog str, var_type: Type) {
        self.var_stack.push((name, var_type));
    }

    fn get_procedure_type(&self, name: &'prog str) -> Result<&Type, Box<dyn Error>> {
        if let Some((_, type_)) = self.procedures.get_key_value(name) {
            Ok(type_)
        } else {
            Err(Box::new(io::Error::new(
                io::ErrorKind::Other,
                "Procedure is not defined",
            )))
        }
    }

    fn get_var_type(&self, name: &str) -> Result<Type, Box<dyn Error>> {
        for (name_, type_) in self.var_stack.iter().rev() {
            if *name_ == name {
                return Ok(type_.clone().clone());
            }
        }

        Err(Box::new(io::Error::new(
            io::ErrorKind::Other,
            "Variable not defined",
        )))
    }
}

/// Merges functionalities of expect_type() and match_types() methods into one function:
/// Checks if the passed two types match. If it does not, returns an error.
/// Otherwise, checks if it matches the expected type. If it doesn't match, returns an error.
/// Otherwise, returns the matched type
fn expect_matched_types<'prog>(
    expected: Vec<Type>,
    type1: &'prog Type,
    type2: &'prog Type,
) -> Result<Type, Box<dyn Error>> {
    let matched_type = match_types(type1, type2)?;
    expect_type(expected, matched_type)
}

/// Checks if a type matches an expected type. If it does, returns true, otherwise returns false.
fn expect_type<'prog>(expected: Vec<Type>, actual: &'prog Type) -> Result<Type, Box<dyn Error>> {
    if expected.contains(&actual) {
        Ok(actual.clone())
    } else {
        Err(Box::new(io::Error::new(
            io::ErrorKind::Other,
            "Type does not match with expected type",
        )))
    }
}

/// Given two types, consumes and returns the matched type when the two types match (or are equal).
/// If one of the types are None, return the type of the other. If the two types do not match, returns an error.
fn match_types<'prog>(type1: &'prog Type, type2: &Type) -> Result<&'prog Type, Box<dyn Error>> {
    if type1 == type2 {
        Ok(type1)
    } else {
        Err(Box::new(io::Error::new(
            io::ErrorKind::Other,
            "Types do not match",
        )))
    }
}
