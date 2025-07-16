use std::collections::HashMap;
use crate::context::{Context, Module};
use crate::value::{Func, Value};


impl Context {
    pub fn add_stdlib(&mut self) {
        self.modules.insert(
            "stdlib".to_string(),
            Module {
                values: HashMap::from([
                    ("print".to_string(), Value::Func(
                        Func {
                            name: "print".to_string(),
                            closure: todo!(),
                            body: todo!(),
                        }
                    )),
                ]),
            }
        );
    }
}
