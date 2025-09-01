use crate::types::Type;
use crate::value::Value;
use crate::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum Global<'a> {
    Fn {
        name: &'a str,
        params: Vec<Spanned<(&'a str, Type)>>,
        ret_type: Option<Spanned<Type>>,
        body: Vec<Spanned<Stmt<'a>>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    // Statements
    Expr {
        expr: Box<Spanned<Expr<'a>>>,
    },
    Assign {
        ident: &'a str,
        value: Box<Spanned<Expr<'a>>>,
    },
    Return {
        value: Option<Spanned<Expr<'a>>>,
    },
    Println {
        content: Box<Spanned<Expr<'a>>>,
    },
    If {
        condition: Box<Spanned<Expr<'a>>>,
        then_block: Vec<Spanned<Self>>,
        else_block: Option<Vec<Spanned<Self>>>,
    },
    Block(Vec<Spanned<Self>>),

    // Declarations
    VarDecl {
        name: &'a str,
        var_type: Spanned<Type>,
        init: Option<Box<Spanned<Expr<'a>>>>,
    },
    FnDecl {
        name: &'a str,
        params: Vec<Spanned<(&'a str, Type)>>,
        ret_type: Option<Spanned<Type>>,
        body: Vec<Spanned<Stmt<'a>>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Val(Value),
    Ident(&'a str),
    Call {
        name: &'a str,
        args: Vec<Spanned<Self>>,
    },

    // Arithmetic expressions
    Add(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Sub(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Mul(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Div(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Neg(Box<Spanned<Self>>),
    Gret(Box<Spanned<Self>>, Box<Spanned<Self>>),
    GretEq(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Less(Box<Spanned<Self>>, Box<Spanned<Self>>),
    LessEq(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Eq(Box<Spanned<Self>>, Box<Spanned<Self>>),
    NotEq(Box<Spanned<Self>>, Box<Spanned<Self>>),
    And(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Or(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Not(Box<Spanned<Self>>),

    Error,
}
