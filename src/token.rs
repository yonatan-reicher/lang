use crate::position::Position as Pos;
use derive_more::{Display, From};
use std::rc::Rc;

/// Implement an enum where each case maps to a string literal.
macro_rules! string_enum {
    (
      $( #[$($attr:meta)*] )*
      pub enum $name:ident {
        $($case:ident = $value:literal,)+
      }
    ) => {
        $( #[$($attr)*] )*
        #[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
        pub enum $name {
            $( $case, )+
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                match self {
                    $( Self::$case => $value, )+
                }
            }
        }

        impl std::str::FromStr for $name {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, ()> {
                match s {
                    $( $value => Ok(Self::$case), )+
                    _ => Err(()),
                }
            }
        }

        impl $name {
            pub const ALL: &'static [Self] = &[
                $( Self::$case, )+
            ];
        }
    };
}

string_enum! {
    pub enum Keyword {
        Import = "import",
        Exposing = "exposing",
        Module = "module",
        Exporting = "exporting",
        Label = "label",
        If = "if",
        Then = "then",
        Else = "else",
        True = "true",
        False = "false",
        Match = "match",
        // TODO: Remove
        Print = "print",
        And = "and",
        Or = "or",
    }
}

string_enum! {
    /// On naming the cases: Each case is named after the symbol. They are not named after the
    /// operation (e.g., "Plus" instead of "Add"). The capitalization is based on the name of the
    /// symbol as it appears in American English. Example, people right "Semicolon" instead of
    /// "Semi-Colon". Also, "L" is short for left, "R" for right.
    pub enum Symbol {
        // Note: Two-character symbols must come before their one-character prefix.
        FatArrow = "=>",
        BangEqual = "!=",
        Tilda = "~",
        Bang = "!",
        At = "@",
        Hashtag = "#",
        Dollar = "$",
        Percent = "%",
        Caret = "^",
        Ampersand = "&",
        Star = "*",
        LParen = "(",
        RParen = ")",
        Underscore = "_",
        Plus = "+",
        LCurly = "{",
        RCurly = "}",
        Pipe = "|",
        Colon = ":",
        // Quote = "\"",
        Less = "<",
        Greater = ">",
        QuestionMark = "?",
        Backtick = "`",
        Minus = "-",
        Equal = "=",
        LBracket = "[",
        RBracket = "]",
        Backslash = "\\",
        Semicolon = ";",
        SingleQuote = "'",
        Comma = ",",
        Period = ".",
        Slash = "/",
    }
}

#[derive(Clone, Debug, Default, Eq, From, PartialEq)]
pub enum TokenKind {
    #[default]
    Err,
    Eof,
    Id(Rc<str>),
    #[from]
    Num(i64),
    #[from]
    Str(Rc<str>),
    /// Keyword
    Kw(Keyword),
    /// Symbol
    Sym(Symbol),
}

#[derive(Clone, Debug, Default, Eq, From, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: Pos,
    pub end: Pos,
}

impl AsRef<TokenKind> for Token {
    fn as_ref(&self) -> &TokenKind {
        &self.kind
    }
}

impl<T> PartialEq<T> for Token
where
    TokenKind: PartialEq<T>,
{
    fn eq(&self, other: &T) -> bool {
        &self.kind == other
    }
}

impl From<Keyword> for TokenKind {
    fn from(value: Keyword) -> Self {
        TokenKind::Kw(value)
    }
}

impl From<Symbol> for TokenKind {
    fn from(value: Symbol) -> Self {
        TokenKind::Sym(value)
    }
}

impl PartialEq<Keyword> for TokenKind {
    fn eq(&self, other: &Keyword) -> bool {
        self == &TokenKind::Kw(*other)
    }
}

impl PartialEq<Symbol> for TokenKind {
    fn eq(&self, other: &Symbol) -> bool {
        self == &TokenKind::Sym(*other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_kind_size() {
        assert_eq!(size_of::<Rc<str>>(), 16); // a pointer and a length
        assert_eq!(size_of::<TokenKind>(), 24); // also a discriminant
    }
}
