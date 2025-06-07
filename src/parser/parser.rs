use super::pos::Pos;

use std::rc::Rc;

/// The state of a parser at a given input.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct State<'source> {
    pub text: &'source str,
    pub pos: Pos,
}

impl<'source> State<'source> {
    pub const fn new(text: &'source str) -> Self {
        let pos = Pos::start();
        State { text, pos }
    }

    pub const fn at(text: &'source str, pos: Pos) -> Self {
        State { text, pos }
    }

    pub const fn with_pos(self, pos: Pos) -> Self {
        State { pos, ..self }
    }

    pub fn rest(&self) -> &str {
        &self.text[self.pos.offset..]
    }
}

/// The result of a parsing operation. On success, contains the parsed value
/// and the new state's position. On failure, contains an error and the position
/// where the error occurred.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Result<T, F, E> {
    /// Called on a successful parse!
    Ok(T, Pos),
    /// Could not parse, but the input is recoverable.
    Fail(F, Pos),
    /// Could not parse, and this error must be reported! The input will never
    /// be valid.
    Err(E, Pos),
}

#[derive(Clone)]
pub struct Parser<'a, T, F, E> {
    name: String,
    parse: Rc<dyn std::ops::Fn(State) -> Result<T, F, E> + 'a>,
}

// How a parser is used

impl<'a, T, F, E> Parser<'a, T, F, E> {
    pub fn parse(&self, state: State) -> Result<T, F, E> {
        (self.parse)(state)
    }
}

// 3 basic constructors

impl<T, F, E> std::fmt::Debug for Parser<'_, T, F, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Parser").field("name", &self.name).finish()
    }
}

impl<'a, T, F, E> Parser<'a, T, F, E> {
    pub fn ret(value: T) -> Self
    where
        T: Clone + 'a,
    {
        Parser {
            name: "Ret".to_string(),
            parse: Rc::new(move |state| Result::Ok(value.clone(), state.pos)),
        }
    }

    pub fn fail(value: F) -> Self
    where
        F: Clone + 'a,
    {
        Parser {
            name: "Fail".to_string(),
            parse: Rc::new(move |state| Result::Fail(value.clone(), state.pos)),
        }
    }

    pub fn err(value: E) -> Self
    where
        E: Clone + 'a,
    {
        Parser {
            name: "Err".to_string(),
            parse: Rc::new(move |state| Result::Err(value.clone(), state.pos)),
        }
    }

    pub fn from_fn<Func>(name: String, func: Func) -> Self
    where
        Func: std::ops::Fn(State) -> Result<T, F, E> + 'a,
    {
        Parser {
            name,
            parse: Rc::new(func),
        }
    }
}

// More advanced constructors/combinators

/// `'a` is the lifetime of the parser. Objects that the parser references need
/// to live at least as long as `'a`.
impl<'a, T: 'a, F: 'a, E: 'a> Parser<'a, T, F, E> {
    pub fn map<U, Func>(self, f: Func) -> Parser<'a, U, F, E>
    where
        Func: Fn(T) -> U + 'a,
    {
        let name = format!("Map({})", self.name);
        Parser::from_fn(name, move |state| match self.parse(state) {
            Result::Ok(value, new_pos) => Result::Ok(f(value), new_pos),
            Result::Fail(err, pos) => Result::Fail(err, pos),
            Result::Err(err, pos) => Result::Err(err, pos),
        })
    }

    pub fn and_then<U, Func>(self, f: Func) -> Parser<'a, U, F, E>
    where
        Func: std::ops::Fn(T) -> Parser<'a, U, F, E> + 'a,
    {
        let name = format!("AndThen({})", self.name);
        Parser::from_fn(name, move |state| match self.parse(state) {
            Result::Ok(a, pos) => {
                // NOTE: In a lot of other parser combinator implementations, if
                // `f` were to return `Fail` here, we would map to `Err`. We
                // keep it as `Fail` to allow more flexible parsing, at the cost
                // of more look-ahead (allows worse performance).
                f(a).parse(State {
                    text: state.text,
                    pos,
                })
            }
            Result::Fail(err, pos) => Result::Fail(err, pos),
            Result::Err(err, pos) => Result::Err(err, pos),
        })
    }

    pub fn or<G: 'a, H: 'a>(
        self,
        other: Parser<'a, T, G, E>,
        combine_fail: impl Fn(F, State, G, State) -> H + 'a,
    ) -> Parser<'a, T, H, E> {
        let name = format!("Or({} | {})", self.name, other.name);
        Parser::from_fn(name, move |state| match self.parse(state) {
            Result::Ok(value, pos) => Result::Ok(value, pos),
            // Recoverable failure - we try the other parser.
            // Notice that we pass the original state.
            Result::Fail(f1, f1_pos) => match other.parse(state) {
                Result::Ok(value, pos) => Result::Ok(value, pos),
                Result::Fail(f2, f2_pos) => {
                    let f = combine_fail(f1, state.with_pos(f1_pos), f2, state.with_pos(f2_pos));
                    Result::Fail(f, state.pos)
                }
                Result::Err(err, pos) => Result::Err(err, pos),
            },
            Result::Err(err, pos) => Result::Err(err, pos),
        })
    }

    fn with_name(self, name: String) -> Parser<'a, T, F, E> {
        Parser { name, ..self }
    }

    pub fn one_of(
        parsers: impl IntoIterator<Item = Parser<'a, T, F, E>>,
    ) -> Parser<'a, T, Vec<(F, Pos)>, E>
    where
        F: Clone + 'a,
    {
        let mut ret = Parser::fail(vec![]);
        let mut names = vec![];
        for parser in parsers {
            names.push(parser.name.clone());
            ret = ret.or(parser, |f1, _f1_state, f2, f2_state| {
                let mut f = f1;
                f.push((f2, f2_state.pos));
                f
            });
        }
        let name = format!("OneOf({})", names.join(" | "));
        ret.with_name(name)
    }

    /// We can't write a parser that returns the current state directly, because
    /// the state is parameterized by an unknown lifetime. So instead, we use
    /// continuation-passing style to act on the state when we know it is alive.
    pub fn state(f: impl Fn(State) -> T + 'a) -> Self {
        let name = "State".to_string();
        Parser::from_fn(name, move |state| Result::Ok(f(state), state.pos))
    }

    pub fn filter(self, pred: impl Fn(T) -> bool, f: F) -> Parser<'a, T, F, E> {
        let name = format!("Filter({})", self.name);
        self.and_then(|value| {
            if pred(value) {
                Parser::ret(value)
            } else {
                Parser::fail(f.clone())
            }
        })
    }
}

