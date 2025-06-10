//! This module is responsible for parsing from source code to AST.

use crate::ast::Expr;
use nessie_parse::one_of;
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {}

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
        .map(Token::Ident)
}

fn symbol<'a>(s: &'static str, ret: Token) -> Parser<'a, Token> {
    Parser::expect_string(";").map(move |()| ret.clone()).map_fail(|_| ())
}

fn token<'a>() -> Parser<'a, Token> {
    Parser::skip_whitespace().and_then(|()| {
        one_of![
            symbol(";", Token::Semicolon),
            symbol(":", Token::Colon),
            symbol("=", Token::Eq),
            identifier_or_keyword(),
            number().map(Token::Number),
        ]
        .map_fail(|_| ())
    })
}
