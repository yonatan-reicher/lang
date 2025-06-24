//! This module is responsible for parsing from source code to AST.

use crate::ast::{BinOp, Expr};
use indoc::indoc;
use nessie_parse::{ParseResult, Pos, one_of};
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("Unclosed parenthesis")]
    UnclosedParen,
}

type Parser<'a, T, F = ()> = nessie_parse::Parser<'a, T, Error, F>;

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

#[derive(Clone, Debug, PartialEq, Eq)]
enum Token {
    Ident(String),
    Number(i64),
    Semicolon,
    Colon,
    Comma,
    Plus,
    Eq,
    Print,
    LParen,
    RParen,
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

fn token<'a>() -> Parser<'a, Token> {
    Parser::skip_whitespace().and_then(|()| {
        one_of![
            symbol(";", Token::Semicolon),
            symbol(":", Token::Colon),
            symbol("=", Token::Eq),
            symbol(",", Token::Comma),
            symbol("+", Token::Plus),
            symbol("(", Token::LParen),
            symbol(")", Token::RParen),
            identifier_or_keyword(),
            number().map(Token::Number),
        ]
        .map_fail(|_| ())
    })
}

fn token_eq<'a>(t: Token) -> Parser<'a, ()> {
    token().filter(move |t1| t1 == &t, ()).map(|_| ())
}

fn atom<'a>() -> Parser<'a, Expr> {
    token()
        .and_then(|token| match token {
            Token::LParen => {
                expr().and_then(|expr| {
                    token_eq(Token::RParen)
                        .map(move |_| expr.clone())
                        .or_err(Error::UnclosedParen)
                })
            }
            Token::Number(n) => Parser::ret(Expr::Int(n)),
            Token::Ident(ident) => Parser::ret(Expr::Var(ident)),
            _ => Parser::fail(()),
        })
}

fn expr<'a>() -> Parser<'a, Expr> {
    one_of![
        atom().and_then(|left| {
            token_eq(Token::Plus)
                .and_then(move |_| expr())
                .map(move |right| Expr::BinOp(
                    Box::new(left.clone()),
                    BinOp::Add,
                    Box::new(right),
                ))
        }),
        atom(),
    ].map_fail(|_| ())
}

#[cfg(test)]
mod tests {
    use super::*;

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
                    col: 15,
                    offset: 14
                },
            ),
        );
    }
}
