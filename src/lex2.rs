//! This module is responsible for lexing (tokenizing) source code into tokens.

use crate::char_reader::CharReader;
use crate::token::*;
use std::io::Result;

#[derive(derive_more::Debug)]
pub struct Lexer<'a> {
    chars: CharReader<'a>,
    next: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub const fn new(char_reader: CharReader<'a>) -> Self {
        Self {
            chars: char_reader,
            next: None,
        }
    }
    pub fn peek(&mut self) -> Result<Token> {
        if self.next.is_none() {
            self.next = Some(lex_single(&mut self.chars)?);
        }
        Ok(self.next.clone().unwrap())
    }

    pub fn pop(&mut self) -> Result<Token> {
        if let Some(token) = self.next.take() {
            Ok(token)
        } else {
            lex_single(&mut self.chars)
        }
    }
}

impl<'a, T: Into<CharReader<'a>>> From<T> for Lexer<'a> {
    fn from(x: T) -> Self {
        Self::new(x.into())
    }
}

pub fn lex(chars: &mut CharReader) -> Result<Vec<Token>> {
    let mut ret = vec![];
    loop {
        let token = lex_single(chars)?;
        let is_eof = matches!(token.kind, TokenKind::Eof);
        ret.push(token);
        if is_eof {
            return Ok(ret);
        }
    }
}

pub fn lex_single(chars: &mut CharReader) -> Result<Token> {
    skip_whitespace(chars)?;
    let start = chars.pos();
    let kind = match chars.peek()? {
        None => TokenKind::Eof,
        Some(c) if c.is_ascii_digit() => number(chars)?,
        Some(c) if is_ident_start(c) => ident(chars)?,
        Some('"') => string(chars)?,
        Some(c) if is_symbol_start(c) => symbol(chars)?,
        _ => {
            chars.pop()?;
            TokenKind::Err
        }
    };
    let end = chars.pos();
    Ok(Token { kind, start, end })
}

fn skip_whitespace(chars: &mut CharReader) -> Result<()> {
    while chars.peek()?.is_some_and(char::is_whitespace) {
        chars.pop()?;
    }
    Ok(())
}

fn number(chars: &mut CharReader) -> Result<TokenKind> {
    let mut i = 0;
    while chars.peek()?.is_some_and(|c| c.is_ascii_digit()) {
        let c = chars.pop()?.unwrap();
        i *= 10;
        i += c as i64 - '0' as i64;
    }
    Ok(TokenKind::from(i))
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

fn ident(chars: &mut CharReader) -> Result<TokenKind> {
    let mut string = String::new();
    while chars.peek()?.is_some_and(is_ident) {
        string.push(chars.pop()?.unwrap());
    }
    if string == "_" {
        return Ok(TokenKind::Sym(Symbol::Underscore));
    }
    if let Some(k) = Keyword::ALL
        .iter()
        .cloned()
        .find(|k| string.as_str() == k.as_ref())
    {
        return Ok(k.into());
    }
    Ok(TokenKind::Id(string.into()))
}

fn is_symbol_start(c: char) -> bool {
    Symbol::ALL.iter().any(|s| s.as_ref().starts_with(c))
}

fn symbol(chars: &mut CharReader) -> Result<TokenKind> {
    // Look for some longest prefix of some symbols.
    let mut prefix = String::new();
    prefix.push(chars.pop()?.unwrap());
    let symbol_str = loop {
        prefix.push(chars.peek()?.unwrap());
        if Symbol::ALL.iter().all(|s| !s.as_ref().starts_with(&prefix)) {
            // We went too long!
            prefix.pop();
            break prefix;
        }
        chars.pop()?;
    };
    Ok(Symbol::ALL
        .iter()
        .cloned()
        .find(|s| s.as_ref() == symbol_str)
        .expect("has to exist because string is longest prefix of Symbols")
        .into())
}

fn string(chars: &mut CharReader) -> Result<TokenKind> {
    assert_eq!(chars.pop()?, Some('"'));
    let mut s = String::new();
    loop {
        match chars.pop()? {
            None => return Ok(TokenKind::Err),
            Some('"') => break,
            Some(c) => s.push(c),
        }
    }
    Ok(TokenKind::Str(s.into()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::char_reader::CharReader;
    use crate::position::Position;
    use indoc::indoc;
    use std::rc::Rc;

    #[test]
    fn empty_source() {
        let source = "";
        let mut char_reader = CharReader::new(source.as_bytes());
        assert_eq!(
            lex_single(&mut char_reader).unwrap(),
            Token {
                kind: TokenKind::Eof,
                start: Position::new(source, 0),
                end: Position::new(source, 0),
            }
        );
    }

    #[test]
    fn number_token() {
        let source = "  12345 ";
        let mut char_reader = CharReader::new(source.as_bytes());
        assert_eq!(
            lex_single(&mut char_reader).unwrap(),
            Token {
                kind: TokenKind::Num(12345),
                start: Position::new(source, 2),
                end: Position::new(source, 7),
            }
        );
    }

    #[test]
    fn ident() {
        let source = "   hello_world  ";
        let mut char_reader = CharReader::from(source.as_bytes());
        assert_eq!(
            lex_single(&mut char_reader).unwrap(),
            Token {
                kind: TokenKind::Id(Rc::from("hello_world")),
                start: Position::new(source, 3),
                end: Position::new(source, 14),
            }
        );
    }

    #[test]
    fn string_literal() {
        let source = r#"   "hello, world!"  "#;
        let mut char_reader = CharReader::from(source.as_bytes());
        assert_eq!(
            lex_single(&mut char_reader).unwrap(),
            Token {
                kind: TokenKind::Str(Rc::from("hello, world!")),
                start: Position::new(source, 3),
                end: Position::new(source, 18),
            }
        );
    }

    fn symbol_token(symbol: Symbol, source: &str, expected_end: usize) {
        let mut char_reader = CharReader::from(source.as_bytes());
        assert_eq!(
            lex_single(&mut char_reader).unwrap(),
            Token {
                kind: TokenKind::Sym(symbol),
                start: Position::new(source, 0),
                end: Position::new(source, expected_end),
            }
        );
    }

    #[test]
    fn test_symbols() {
        for symbol in Symbol::ALL {
            let source = format!("{}  ", symbol.as_ref());
            symbol_token(*symbol, &source, symbol.as_ref().len());
        }
    }

    #[test]
    fn varied_tokens() {
        let source = indoc! {r#"
            import std exposing (Print)
            x = 42;
            Print(x)
        "#};
        let mut lexer = Lexer::from(source.as_bytes());
        macro_rules! token {
            ($kind:ident $( ( $( $arg:expr ),* ) )? , $start:expr, $end:expr) => {
                Token {
                    kind: TokenKind::$kind $( ( $($arg),* ) )?,
                    start: Position::new(source, $start),
                    end: Position::new(source, $end),
                }
            };
        }
        let expected_tokens = vec![
            token!(Kw(Keyword::Import), 0, 6),
            token!(Id(Rc::from("std")), 7, 10),
            token!(Kw(Keyword::Exposing), 11, 19),
            token!(Sym(Symbol::LParen), 20, 21),
            token!(Id(Rc::from("Print")), 21, 26),
            token!(Sym(Symbol::RParen), 26, 27),
            token!(Id(Rc::from("x")), 28, 29),
            token!(Sym(Symbol::Equal), 30, 31),
            token!(Num(42), 32, 34),
            token!(Sym(Symbol::Semicolon), 34, 35),
            token!(Id(Rc::from("Print")), 36, 41),
            token!(Sym(Symbol::LParen), 41, 42),
            token!(Id(Rc::from("x")), 42, 43),
            token!(Sym(Symbol::RParen), 43, 44),
            token!(Eof, 45, 45),
        ];
        for expected_kind in expected_tokens {
            let token = lexer.pop().unwrap();
            assert_eq!(token, expected_kind);
        }
    }
}
