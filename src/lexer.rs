use chumsky::{extra, prelude::*};

use crate::token::Token;
use crate::types::Type;
use crate::value::Value;
use crate::Span;
use crate::Spanned;

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    // Tokenize numeric literals
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(|n: f64| Token::Val(Value::Num(n)));

    // Tokenize string literals
    let str_ = just('"')
        .ignore_then(none_of('"').repeated().to_slice())
        .then_ignore(just('"'))
        .map(|s: &str| Token::Val(Value::Str(s.to_string())));

    // Tokenize boolean literals
    let bool_ = text::keyword("true")
        .or(text::keyword("false"))
        .to_slice()
        .from_str()
        .unwrapped()
        .map(|b: bool| Token::Val(Value::Bool(b)));

    // Tokenize operators and syntactic tokens
    let op = choice((
        just('<').then(just('<')).to(Token::DoubleLess),
        just('-').then(just('>')).to(Token::RightArrow),
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Star),
        just('/').to(Token::Slash),
        just('<').then(just('=')).to(Token::LessEqual),
        just('<').to(Token::Less),
        just('>').then(just('=')).to(Token::GreaterEqual),
        just('>').to(Token::Greater),
        just('!').then(just('=')).to(Token::NotEqual),
        just('=').to(Token::Equal),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
    ));

    let grouping = choice((
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        just('{').to(Token::LeftBrace),
        just('}').to(Token::RightBrace),
    ));

    // Tokenize literals
    let literal = num.or(str_).or(bool_);

    // Tokenize type specifiers
    let type_ = choice((
        text::keyword("num").to(Token::Num),
        text::keyword("str").to(Token::Str),
        text::keyword("bool").to(Token::Bool),
        text::keyword("fn").to(Token::Fn),
    ));

    // Tokenize keywords
    let kword = choice((
        text::keyword("let").to(Token::Let),
        text::keyword("proc").to(Token::Proc),
        text::keyword("ret").to(Token::Ret),
        text::keyword("if").to(Token::If),
        text::keyword("then").to(Token::Then),
        text::keyword("else").to(Token::Else),
        text::keyword("println").to(Token::Println),
    ));

    // Tokenize identifiers
    let ident = text::ident().map(|s| Token::Ident(s));

    // Tokenize comments
    let comment = just('#')
        .then(any().and_is(text::newline().not()).repeated())
        .padded();

    let token = literal.or(op).or(grouping).or(type_).or(kword).or(ident);

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
