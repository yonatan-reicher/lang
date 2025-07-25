//! Provides functionality to parse and run code in a simple, interpreted PL.

/// Defines the Abstract Syntax Tree.
pub mod ast;
pub mod value;
pub mod parse;
/// Defines the context of the interpreter while it is running.
pub mod context;
pub mod stdlib;
/// Evaluating ASTs to values.
pub mod eval;
/// Executing programs.
pub mod execute;

pub fn execute_string(source_code: &str) -> Result<Vec<value::Value>, Box<dyn std::error::Error>> {
    let out = Default::default();
    let mut context = context::Context {
        vars: [].into(),
        out: context::PrintOutput::Vec(std::rc::Rc::clone(&out)),
        modules: [].into(),
    };
    context.add_stdlib();
    let ast = parse::parse(source_code)?;
    ast.execute(&mut context);
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
