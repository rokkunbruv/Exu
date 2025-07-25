use crate::ast::{Expr, ProgramItem, Stmt};
use crate::token::Token;
use crate::types::Type;
use crate::value::Value;
use crate::Program;
use crate::Span;
use crate::Spanned;
use chumsky::{input::ValueInput, pratt::*, prelude::*};

pub fn parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Program<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let procedure = proc_decl_parser();

    procedure
        .repeated()
        .collect::<Vec<_>>()
        .map_with(|p, _| Program::new(p))
}

fn proc_decl_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<ProgramItem<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
       + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let name = ident_parser();

    let params = params_parser();

    let return_type = just(Token::RightArrow).ignore_then(type_parser()).or_not();

    let body = stmt_parser()
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
        .map(|block| block);

    just(Token::Proc)
        .ignore_then(name)
        .then(params)
        .then(return_type)
        .then(body)
        .map_with(|(((name, params), ret_type), body), e| {
            (
                ProgramItem::Proc {
                    name,
                    body,
                    params,
                    ret_type,
                },
                e.span(),
            )
        })
}

fn stmt_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Stmt<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    recursive(|s| {
        let block = s
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            .map(|block| block);

        let expr_stmt = expr_parser()
            .then_ignore(just(Token::Semicolon))
            .map_with(|expr, _| Stmt::Expr {
                expr: Box::new(expr),
            });

        let ret_stmt = just(Token::Ret)
            .ignore_then(expr_parser().or_not())
            .then_ignore(just(Token::Semicolon))
            .map_with(|expr, _| Stmt::Return { value: expr });

        let println_stmt = just(Token::Println)
            .ignore_then(just(Token::DoubleLess))
            .ignore_then(expr_parser())
            .then_ignore(just(Token::Semicolon))
            .map_with(|expr, _| Stmt::Println {
                content: Box::new(expr),
            });

        let if_stmt = just(Token::If)
            .ignore_then(
                expr_parser()
                    .map(|expr| Box::new(expr))
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then_ignore(just(Token::Then))
            .then(block.clone().map(|block| block))
            .then(
                just(Token::Else)
                    .ignore_then(block.clone().map(|block| block))
                    .or_not(),
            )
            .map_with(|((condition, then_block), else_block), _| Stmt::If {
                condition,
                then_block,
                else_block,
            });

        let block_stmt = block.clone().map(|block| Stmt::Block(block));

        let stmt = expr_stmt
            .or(ret_stmt)
            .or(println_stmt)
            .or(if_stmt)
            .or(block_stmt);

        let var_decl = var_decl_parser().map_with(|(decl, _), _| decl);

        let fn_decl = just(Token::Let)
            .ignore_then(just(Token::Fn))
            .ignore_then(ident_parser())
            .then_ignore(just(Token::Colon))
            .then_ignore(just(Token::Fn))
            .then(params_parser())
            .then(just(Token::RightArrow).ignore_then(type_parser()))
            .then(block.clone())
            .map_with(|(((name, params), ret_type), body), _| Stmt::FnDecl {
                name,
                params,
                ret_type,
                body,
            });

        let decl = var_decl.or(fn_decl);

        stmt.or(decl).map_with(|stmt, e| (stmt, e.span()))
    })
}

fn var_decl_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Stmt<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let ident = ident_parser();

    let var_type = type_parser();

    let init = just(Token::Colon)
        .ignore_then(expr_parser())
        .map_with(|expr, _| Box::new(expr))
        .or_not()
        .map_with(|expr, _| expr);

    just(Token::Let)
        .ignore_then(var_type)
        .then(ident)
        .then(init)
        .then_ignore(just(Token::Semicolon))
        .map_with(|((var_type, name), init), e| {
            (
                Stmt::VarDecl {
                    name,
                    var_type,
                    init,
                },
                e.span(),
            )
        })
}

fn expr_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let op = |op| just(op).map_with(|op, e| (op, e.span()));

    // Pratt expression parser in descending precedence
    recursive(|expr| {
        let atom = select! {
            Token::Val(Value::Num(x)) => Expr::Val(Value::Num(x)),
            Token::Val(Value::Str(x)) => Expr::Val(Value::Str(x)),
            Token::Val(Value::Bool(x)) => Expr::Val(Value::Bool(x)),
            Token::Ident(s) => Expr::Ident(s),
        }
        .map_with(|expr, e| (expr, e.span()));

        let grouping = expr
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let call = ident_parser()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .map_with(|(name, args), e| (Expr::Call { name, args }, e.span()));

        let primary = call.or(grouping).or(atom);

        let expr = primary.pratt((
            // Parsing negation
            prefix(6, op(Token::Minus), |_, rhs: Spanned<Expr<'src>>, _| {
                let span = rhs.1;
                (Expr::Neg(Box::new(rhs)), span.into())
            }),
            // Parsing multiplication
            infix(
                left(5),
                op(Token::Star),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::Mul(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing division
            infix(
                left(5),
                op(Token::Slash),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::Div(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing addition
            infix(
                left(4),
                op(Token::Plus),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::Add(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing subtraction
            infix(
                left(4),
                op(Token::Minus),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::Sub(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing greater than
            infix(
                left(3),
                op(Token::Greater),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::Gret(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing greater than or equal
            infix(
                left(3),
                op(Token::GreaterEqual),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::GretEq(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing less than
            infix(
                left(3),
                op(Token::Less),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::Less(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing less than or equal
            infix(
                left(3),
                op(Token::LessEqual),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::LessEq(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing equal
            infix(
                left(2),
                op(Token::Equal),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::Eq(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
            // Parsing not equal
            infix(
                left(2),
                op(Token::NotEqual),
                |lhs: Spanned<Expr<'src>>, _, rhs: Spanned<Expr<'src>>, _| {
                    let span = lhs.1.start..rhs.1.end;
                    (Expr::NotEq(Box::new(lhs), Box::new(rhs)), span.into())
                },
            ),
        ));

        expr
    })
}

fn params_parser<'tokens, 'src: 'tokens, I>() -> impl Parser<
    'tokens,
    I,
    Vec<Spanned<(&'src str, Type)>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    type_parser()
        .then(ident_parser())
        .map_with(|((type_, _), ident), e| ((ident, type_), e.span()))
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .or_not()
        .map_with(|params, _| match params {
            Some(p) => p,
            None => Vec::new(),
        })
        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
}

fn type_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Type>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    recursive(|t| {
        let num = just(Token::Num).to(Type::Num);
        let string = just(Token::Str).to(Type::Str);
        let boolean = just(Token::Bool).to(Type::Bool);
        let func = just(Token::Fn)
            .ignore_then(
                t.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then_ignore(just(Token::RightArrow))
            .then(t.clone())
            .map(|(params, ret_type)| Type::Fn {
                params,
                ret_type: Box::new(ret_type),
            });

        num.or(string).or(boolean).or(func)
    })
    .map_with(|type_, e| (type_, e.span()))
}

fn ident_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, &'src str, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    select! {
        Token::Ident(name) => name
    }
    .map(|n| n)
}
