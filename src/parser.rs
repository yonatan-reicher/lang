//! This module is responsible for declaring the parser type, and ways to create
//! parsers. This modules does not include the parser for the language. This is
//! just a library for creating parsers.

mod pos;
#[allow(clippy::module_inception)]
mod parser;
mod primitives;
