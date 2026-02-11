//! Provides functionality to parse and run code in a simple, interpreted PL.

mod position;
/// Define the tokens of the language
mod token;
mod char_reader;
/// Define the lexer.
mod lex;
mod parse;

/// Defines the Abstract Syntax Tree.
pub mod ast;
pub mod labeled;
pub mod value;
pub mod value_ref;
/// Defines the context of the interpreter while it is running.
pub mod context;
pub mod stdlib;
/// Evaluating ASTs to values.
pub mod eval;
/// Executing programs.
pub mod execute;
/// Working with types and inferring them!
pub mod typing;
/// Implements a Read-Eval-Print-Loop that can execute commands
pub mod repl;
/// Executing Io operations represented as values.
pub mod io_commands;

pub use lex::lex;
pub use token::*;
pub use position::Position;

use crate::stdlib::Stdlib;

/// This function returns a vector of values that were printed during execution.
/// The RETURN VALUE of the program is IGNORED.
pub fn execute_string<'text>(
    source_code: &'text str,
) -> Result<Vec<value::Value>, Box<dyn std::error::Error + 'text>> {
    let out = Default::default();
    let mut context = context::Context {
        vars: [].into(),
        out: context::PrintOutput::Vec(std::rc::Rc::clone(&out)),
        modules: [].into(),
    };
    Stdlib::new().attach(&mut context);
    let ast = parse::parse(source_code.as_bytes())?;
    ast.execute(&mut context)?;
    let vec = std::mem::take(out.borrow_mut().as_mut());
    Ok(vec)
}

// Re-export
pub mod prelude {
    pub use super::execute_string;
    use super::*;
    pub use ast::{BinOp, Expr, Program, Statement};
    pub use context::Context;
    pub use parse::parse;
    pub use value::Value;
}

#[cfg(test)]
mod tests;
