//! This module is responsible for parsing from source code to tokens.

use std::rc::Rc;

// use libraries
use derive_more::From;
use nessie_parse::one_of;
use thiserror::Error;

// This parser is built on top of Nessie Parse. Parsers can either return, fail,
// or error. The difference between failure and error is that failures are
// supposed to be recovered from.
//
// This parser only fails on the top level (because tokens are very flexible
// syntactically). The failures are in a tree hierarchy and document when a case
// does not apply.

/// The parser type we use.
type Parser<'a, T, F, E = Error> = nessie_parse::Parser<'a, T, E, F>;

#[derive(Clone, Debug, From, PartialEq, Eq)]
pub enum Token {
    Ident(Rc<String>),
    #[from]
    Number(i64),
    #[from]
    String(Rc<str>),
    Colon,
    Comma,
    Exporting,
    Exposing,
    FatArrow,
    Import,
    /// `{`
    LCurly,
    /// `(`
    LParen,
    Module,
    Print,
    /// `}`
    RCurly,
    /// `)`
    RParen,
    Semicolon,
    Label,
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Match,
    /// `|`
    Pipe,
    /// `_`
    Underscore,
}

// Some helpers

macro_rules! derive_all {
    ( - Default /* Should be an item */ $($token:tt)* ) => {
        #[derive(Clone, Debug, Error, From, PartialEq, Eq)]
        $($token)*
    };
    ( /* Should be an item */ $($token:tt)* ) => {
        #[derive(Clone, Debug, Default, Error, From, PartialEq, Eq)]
        $($token)*
    };
}

fn vec_to_string(vec: Vec<char>) -> String {
    vec.iter().cloned().collect()
}

fn default<T: Default>() -> T {
    T::default()
}

// The implementation

derive_all![
    #[error("does not begin with a digit")]
    struct NoDigit;
];

fn number<'a>() -> Parser<'a, Token, NoDigit> {
    Parser::digit()
        .or_fail(NoDigit)
        .repeat_1()
        .map(vec_to_string)
        .map(|s| s.parse::<i64>().unwrap().into())
}

derive_all![
    #[error("does not begin with '\"'")]
    pub struct NoQuote;
];
derive_all![
    #[error("does begin not begin with a non-quote character")]
    pub struct YesQuoteOrEof;
];

derive_all![ - Default
    pub enum StringError {
        #[error("string is missing a closing quote")]
        NoClosingQuote,
    }
];

fn string<'a>() -> Parser<'a, Token, NoQuote, StringError> {
    fn not_quote<'a, E: 'a>() -> Parser<'a, char, YesQuoteOrEof, E> {
        Parser::char().or_fail(YesQuoteOrEof).filter(|c| *c != '"')
    }

    // Expect opening quote
    Parser::char_eq('"').and_then(|_| {
        // Some middle chars...
        not_quote().repeat_0().and_then(|chars| {
            let token = Token::String(vec_to_string(chars).into());
            // Closing quote!
            Parser::<_, (), _>::char_eq('"')
                .map(move |_| token.clone())
                .or_err(StringError::NoClosingQuote)
        })
    })
}

derive_all![
    #[error("does not begin with a letter or an underscore")]
    struct NoLetterOrUnderscore;
];
derive_all![
    #[error("does not begin with a letter, an underscore, or a digit")]
    struct NoLetterOrUnderscoreOrDigit;
];
derive_all![
    #[error("{0}")]
    struct NoIdentifier(NoLetterOrUnderscore);
];

fn identifier_or_keyword<'a>() -> Parser<'a, Token, NoIdentifier> {
    fn start_char<'a>() -> Parser<'a, char, NoLetterOrUnderscore> {
        Parser::char()
            .or_fail(default())
            .filter(|&c| c.is_alphabetic() || c == '_')
    }

    fn end_char<'a>() -> Parser<'a, char, NoLetterOrUnderscoreOrDigit> {
        Parser::char()
            .or_fail(default())
            .filter(|&c| c.is_alphanumeric() || c == '_')
    }

    start_char()
        .map_fail(Into::into)
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
            "exporting" => Token::Exporting,
            "exposing" => Token::Exposing,
            "import" => Token::Import,
            "module" => Token::Module,
            "print" => Token::Print,
            "label" => Token::Label,
            "match" => Token::Match,
            _ => Token::Ident(ident.into()),
        })
}

derive_all![ - Default
    #[error("does not begin with symbol '{0}'")]
    struct NoSymbol(&'static str);
];

fn symbol<'a>(s: &'static str, ret: Token) -> Parser<'a, Token, NoSymbol> {
    Parser::expect_string(s)
        .map(move |()| ret.clone())
        .or_fail(NoSymbol(s))
}

/// A monolith error type for this parser.
#[derive(Clone, Debug, Error, From, PartialEq)]
#[error("{0}")]
#[allow(private_interfaces)]
pub enum Error {
    #[error("{0}")]
    NoIdentifier(NoIdentifier),
    #[error("{0}")]
    NoSymbol(NoSymbol),
    #[error("{0}")]
    NoNumber(NoDigit),
    #[error("{0}")]
    NoQuote(NoQuote),
    #[error("{0}")]
    String(StringError),
    #[error("{}", .0.iter().map(|f| f.to_string()).collect::<Vec<_>>().join(", "))]
    Many(Vec<Error>),
}

derive_all![
    #[error("unexpected end of input")]
    pub struct EofFail;
];

/// Parse a token from the text!
pub fn token<'a>() -> Parser<'a, Token, EofFail> {
    Parser::skip_whitespace()
        .and_then(|()| Parser::eof().not::<EofFail>())
        .and_then(|()| {
            one_of![
                // Symbols should be added here
                symbol("_", Token::Underscore).map_fail(Error::from),
                symbol(";", Token::Semicolon).map_fail(Error::from),
                symbol(":", Token::Colon).map_fail(Error::from),
                symbol("=>", Token::FatArrow).map_fail(Error::from),
                symbol("=", Token::Eq).map_fail(Error::from),
                symbol(",", Token::Comma).map_fail(Error::from),
                symbol("+", Token::Plus).map_fail(Error::from),
                symbol("-", Token::Minus).map_fail(Error::from),
                symbol("*", Token::Star).map_fail(Error::from),
                symbol("/", Token::Slash).map_fail(Error::from),
                symbol("(", Token::LParen).map_fail(Error::from),
                symbol(")", Token::RParen).map_fail(Error::from),
                symbol("{", Token::LCurly).map_fail(Error::from),
                symbol("}", Token::RCurly).map_fail(Error::from),
                symbol("|", Token::Pipe).map_fail(Error::from),
                identifier_or_keyword().map_fail(Error::from),
                number().map_fail(Error::from),
                string().map_fail(Error::from).map_err(Error::from),
            ]
            .and_then_fail(|errors| Parser::err(Error::Many(errors)))
        })
}

pub fn token_eq<'a, F>(t: Token) -> Parser<'a, (), F>
where
    F: 'a + Clone + Default,
{
    token()
        .or_fail(default())
        .filter(move |t1| t1 == &t)
        .map(|_| ())
}

pub fn token_ident<'a, F>() -> Parser<'a, Rc<String>, F>
where
    F: 'a + Clone + Default,
{
    token().or_fail(default()).and_then(|token| match token {
        Token::Ident(ident) => Parser::ret(ident),
        _ => Parser::fail(F::default()),
    })
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
    fn token_string_literal() {
        let res = token().parse(
            indoc! {r#"
                "hello world1'!"
            "#}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Token::String("hello world1'!".into()),
                Pos {
                    offset: 16,
                    row: 1,
                    col: 17,
                },
            ),
        );
    }
}
