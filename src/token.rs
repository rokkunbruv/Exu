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
    // Period,

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

impl<'a> ToString for Token<'a> {
    fn to_string(&self) -> String {
        match self {
            Self::Val(v) => v.to_string(),
            Self::Ident(i) => String::from(*i),
            Self::Plus => String::from("+"),
            Self::Minus => String::from("-"),
            Self::Star => String::from("*"),
            Self::Slash => String::from("/"),
            Self::Greater => String::from(">"),
            Self::GreaterEqual => String::from(">="),
            Self::Less => String::from("<"),
            Self::LessEqual => String::from("<="),
            Self::Equal => String::from("="),
            Self::NotEqual => String::from("!="),
            Self::Colon => String::from(":"),
            Self::Semicolon => String::from(";"),
            Self::Comma => String::from(","),
            Self::RightArrow => String::from("->"),
            Self::DoubleLess => String::from("<<"),
            Self::LeftParen => String::from("("),
            Self::RightParen => String::from(")"),
            Self::LeftBrace => String::from("{"),
            Self::RightBrace => String::from("}"),
            Self::Num => String::from("num"),
            Self::Str => String::from("str"),
            Self::Bool => String::from("bool"),
            Self::Fn => String::from("fn"),
            Self::Let => String::from("let"),
            Self::Proc => String::from("proc"),
            Self::Ret => String::from("ret"),
            Self::Println => String::from("println"),
            Self::If => String::from("if"),
            Self::Then => String::from("then"),
            Self::Else => String::from("else"),
        }
    }
}
