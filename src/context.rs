use std::collections::HashMap;

use crate::value::Value;

#[derive(Debug, Default, Clone, PartialEq)]
pub enum PrintOutput {
    #[default]
    Stdout,
    Ignore,
    Vec(std::rc::Rc<std::cell::RefCell<Vec<Value>>>),
}

impl PrintOutput {
    pub fn print(&self, value: &Value) {
        match self {
            PrintOutput::Stdout => println!("{value}"),
            PrintOutput::Ignore => (),
            PrintOutput::Vec(vec) => vec.borrow_mut().push(value.clone()),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Module {
    pub values: HashMap<String, Value>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Context {
    pub vars: std::collections::HashMap<String, Value>,
    pub modules: HashMap<String, Module>,
    pub out: PrintOutput,
}
