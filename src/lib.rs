//! Provides functionality to parse and run code in a simple, interpreted PL.

/// Defines the Abstract Syntax Tree.
pub mod ast;
pub mod value;
pub mod parse;
/// Defines the context of the interpreter while it is running.
pub mod context;
/// Evaluating ASTs to values.
pub mod eval;
/// Executing programs.
pub mod execute;
