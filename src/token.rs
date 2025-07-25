use crate::types::Type;
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Val(Value),
    Ident(&'a str),

    // Operator Tokens
    Plus,
    Minus,
    Star,
    Slash,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Colon,
    Period,

    // Syntactic Tokens
    Semicolon,
    Comma,
    RightArrow,
    DoubleLess, // TEMPORARY

    // Grouping Tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Type Specifiers
    Num,
    Str,
    Bool,
    Fn,

    // Keywords
    Let,
    Proc,
    Ret,
    Println, // TEMPORARY
    If,
    Then,
    Else,
}
