use super::parser::{Parser, State};

impl<'a, T: 'a, F: 'a, E: 'a> Parser<'a, T, F, E> {
    pub fn of_bool(value: bool) -> Parser<'a, T, F, E>
    where
        T: Default + Clone,
        F: Default + Clone,
    {
        if value {
            Parser::ret(T::default())
        } else {
            Parser::fail(F::default())
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct EofFailure;

pub fn char<'a, E: 'a>() -> Parser<'a, char, EofFailure, E> {
    let get_char = |state: State| state.rest().chars().next();
    Parser::state(get_char).and_then(|ch| match ch {
        None => Parser::fail(EofFailure),
        Some(ch) => Parser::ret(ch),
    })
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct NotFound;

pub fn eof<'a, E: 'a>() -> Parser<'a, (), NotFound, E> {
    Parser::state(|state| state.rest().is_empty()).and_then(Parser::of_bool)
}

pub fn expect_string<'a, E: 'a>(expected: &'static str) -> Parser<'a, (), NotFound, E> {
    let s = move |state: State| state.rest().starts_with(expected);
    Parser::state(s).and_then(Parser::of_bool)
}
