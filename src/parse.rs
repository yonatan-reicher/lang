//! This module is responsible for parsing from source code to AST.

use crate::ast::{BinOp, Expr, Program, Statement};
use nessie_parse::{ParseResult, one_of};
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    // TODO: Make this not hard-coded, or at least made in some reasonable way.
    #[error(
        "Unrecognized token: valid tokens are numbers, identifiers, and symbols ';', ':', '=', '=>', '+', '(' and  ')'"
    )]
    UnregocnizedToken,
    #[error("Unclosed parenthesis")]
    UnclosedParen,
    #[error("Print statement expects a single argument (print x)")]
    PrintStatementExpectsSingleArgument,
    #[error("Assignment statement expected an expression")]
    AssignmentStatementExpectsExpression,
    #[error("Bad statement")]
    BadStatement,
    #[error("Did not find function body")]
    NoFunctionBody,
}

type Parser<'a, T, F = ()> = nessie_parse::Parser<'a, T, Error, F>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Token {
    Ident(String),
    Number(i64),
    Semicolon,
    Colon,
    Comma,
    Plus,
    FatArrow,
    Eq,
    Print,
    LParen,
    RParen,
}

/// Parse a token from the text!
fn token<'a>() -> Parser<'a, Token> {
    fn number<'a>() -> Parser<'a, i64> {
        fn vec_to_string(vec: Vec<char>) -> String {
            vec.iter().cloned().collect()
        }

        Parser::digit()
            .map_fail(|_| ())
            .repeat_1()
            .map(vec_to_string)
            .map(|s| s.parse::<i64>().unwrap())
    }

    fn identifier_or_keyword<'a>() -> Parser<'a, Token> {
        fn start_char<'a>() -> Parser<'a, char> {
            Parser::char()
                .map_fail(|_| ())
                .filter(|&c| c.is_alphabetic() || c == '_', ())
        }

        fn end_char<'a>() -> Parser<'a, char> {
            Parser::char()
                .map_fail(|_| ())
                .filter(|&c| c.is_alphanumeric() || c == '_', ())
        }

        start_char()
            .and_then(|start_char| {
                end_char().repeat_0().map(move |end_chars| {
                    let mut identifier = String::new();
                    identifier.push(start_char);
                    identifier.extend(end_chars);
                    identifier
                })
            })
            // Keywords should be added here!
            .map(|ident| match ident.as_str() {
                "print" => Token::Print,
                _ => Token::Ident(ident),
            })
    }

    fn symbol<'a>(s: &'static str, ret: Token) -> Parser<'a, Token> {
        Parser::expect_string(s)
            .map(move |()| ret.clone())
            .map_fail(|_| ())
    }

    Parser::skip_whitespace()
        .and_then(|()| Parser::eof().not().map_fail(|()| ()))
        .and_then(|()| {
            one_of![
                symbol(";", Token::Semicolon),
                symbol(":", Token::Colon),
                symbol("=>", Token::FatArrow),
                symbol("=", Token::Eq),
                symbol(",", Token::Comma),
                symbol("+", Token::Plus),
                symbol("(", Token::LParen),
                symbol(")", Token::RParen),
                identifier_or_keyword(),
                number().map(Token::Number),
                Parser::err(Error::UnregocnizedToken),
            ]
            .map_fail(|_| unreachable!())
        })
}

fn token_eq<'a>(t: Token) -> Parser<'a, ()> {
    token().filter(move |t1| t1 == &t, ()).map(|_| ())
}

fn token_ident<'a>() -> Parser<'a, String> {
    token().and_then(|token| match token {
        Token::Ident(ident) => Parser::ret(ident),
        _ => Parser::fail(()),
    })
}

fn atom<'a>() -> Parser<'a, Expr> {
    token().and_then(|token| match token {
        Token::LParen => expr().and_then(|expr| {
            token_eq(Token::RParen)
                .map(move |_| expr.clone())
                .or_err(Error::UnclosedParen)
        }),
        Token::Number(n) => Parser::ret(Expr::Int(n)),
        Token::Ident(ident) => Parser::ret(Expr::Var(ident)),
        _ => Parser::fail(()),
    })
}

fn function_expr<'a>() -> Parser<'a, Expr> {
    token_ident().and_then(|param_name| {
        token_eq(Token::FatArrow).and_then(move |()| {
            let param_name = param_name.clone();
            expr().or_err(Error::NoFunctionBody).map(move |body| {
                let name = param_name.clone();
                let body = body.clone();
                Expr::Func(name, body.into())
            })
        })
    })
}

fn expr<'a>() -> Parser<'a, Expr> {
    one_of![
        function_expr(),
        atom().and_then(|left| {
            token_eq(Token::Plus)
                .and_then(move |_| expr())
                .map(move |right| Expr::BinOp(Box::new(left.clone()), BinOp::Add, Box::new(right)))
        }),
        atom(),
    ]
    .map_fail(|_| ())
}

fn print_statement<'a>() -> Parser<'a, Statement> {
    token_eq(Token::Print).and_then(|()| {
        atom()
            .map(Statement::Print)
            .or_err(Error::PrintStatementExpectsSingleArgument)
    })
}

fn assignment_statement<'a>() -> Parser<'a, Statement> {
    token_ident().and_then(|ident| {
        token_eq(Token::Eq).and_then(move |_| {
            let ident = ident.clone();
            expr()
                .or_err(Error::AssignmentStatementExpectsExpression)
                .map(move |ex| Statement::Assignment(ident.clone(), ex))
        })
    })
}

/// This parser never fails, always returns or errs.
fn statement<'a>() -> Parser<'a, Statement> {
    one_of![
        print_statement(),
        assignment_statement(),
        Parser::err(Error::BadStatement)
    ]
    .map_fail(|_| unreachable!())
}

fn statement_but_fails_on_eof<'a>() -> Parser<'a, Statement> {
    (Parser::skip_whitespace().and_then(|()| Parser::eof().not())).and_then(|()| statement())
}

fn program<'a>() -> Parser<'a, Program> {
    statement_but_fails_on_eof()
        .repeat_0()
        .map(|statements| Program {
            name: "placeholder program name".to_string(),
            statement: statements,
        })
}

pub fn parse(text: &str) -> Result<Program, Error> {
    let state = text.into();
    match program().parse(state) {
        ParseResult::Ok(prog, _) => Ok(prog),
        ParseResult::Fail((), _) => {
            unreachable!(
                "The main program parser should always either succeed or err, never soft fail."
            );
        }
        ParseResult::Err(err, _) => Err(err),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use nessie_parse::{ParseResult, Pos};

    #[test]
    fn test_number_token() {
        let res = token().parse(
            indoc! {r"
                1234
            "}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Token::Number(1234),
                Pos {
                    row: 1,
                    col: 5,
                    offset: 4
                },
            ),
        );
    }

    #[test]
    fn token_left_paren() {
        let res = token().parse(
            indoc! {r"
                (
            "}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Token::LParen,
                Pos {
                    offset: 1,
                    row: 1,
                    col: 2,
                },
            ),
        );
    }

    #[test]
    fn number_atom() {
        let res = atom().parse(
            indoc! {r"
                3
            "}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Expr::Int(3),
                Pos {
                    offset: 1,
                    row: 1,
                    col: 2,
                },
            ),
        );
    }

    #[test]
    fn test_expr() {
        let res = expr().parse(
            indoc! {r"
                (1 + 2) + 3
            "}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Expr::BinOp(
                    Box::new(Expr::BinOp(
                        Box::new(Expr::Int(1)),
                        BinOp::Add,
                        Box::new(Expr::Int(2)),
                    )),
                    BinOp::Add,
                    Box::new(Expr::Int(3)),
                ),
                Pos {
                    row: 1,
                    col: 12,
                    offset: 11,
                },
            ),
        );
    }
}
