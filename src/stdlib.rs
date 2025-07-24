use std::rc::Rc;

use crate::context::{Context, Module};
use crate::value::{BuiltinDefinition, Label, LabelFunc, PLabel, Value};

impl Module {
    pub fn insert(&mut self, builtin: BuiltinDefinition) {
        self.values.insert(builtin.name.clone(), builtin.into());
    }
}

fn stdlib() -> Module {
    let mut ret = Module { values: [].into() };
    ret.insert(BuiltinDefinition {
        name: "abs".into(),
        arity: 1,
        func: Rc::new(|values| {
            let [Value::Int(i)] = values else {
                panic!("AARG");
            };
            i.abs().into()
        }),
    });
    ret.insert(BuiltinDefinition {
        name: "neg".into(),
        arity: 1,
        func: Rc::new(|values| {
            let [Value::Int(i)] = values else {
                panic!("AARG");
            };
            (-i).into()
        }),
    });

    let none = PLabel::from(Label {
        name: "None".into(),
        parameters: vec![],
    });
    let print = PLabel::from(Label {
        name: "Print".into(),
        parameters: vec!["value".to_string(), "next".to_string()],
    });
    let input = PLabel::from(Label {
        name: "Input".into(),
        parameters: vec!["f".to_string()],
    });
    ret.values.extend([
        ("None".into(), Value::Labeled { label: none, arguments: vec![] }),
        ("Print".into(), LabelFunc::from(print).into()),
        ("Input".into(), LabelFunc::from(input).into()),
    ]);
    /*
    let print_monad = Rc::new(Type {
        name: "PrintM".into(),
        constructors: vec![
            Constructor {
                name: "Return".into(),
                parameters: vec!["value".into()],
            },
            Constructor {
                name: "Print".into(),
                parameters: vec!["value".into(), "next".into()],
            },
        ],
    });
    let print_return = PConstructor::new(print_monad.clone(), 0).unwrap();
    let print_print = PConstructor::new(print_monad.clone(), 1).unwrap();
    let print_bind = BuiltinDefinition {
        name: "print_bind".into(),
        arity: 2,
        func: |args| {
            let [Value::Func(f), mut x] = args else { panic!() };
            let mut to_print = vec![];
            let last = loop {
                let Value::Constructed(c, a) = x else { panic!() };
                if c == print_return {
                    let [value] = a.as_slice() else { panic!() };
                    break value
                } else if c == print_print {
                    let [value, next] = a.as_slice() else { panic!() };
                    to_print.push(value);
                    x = next
                } else {
                    panic!("this PrintM monad should only contain PrintM monads in it's 'next' field");
                }
            };
            f.apply(last);
            todo!()
        }
    };

    let input_monad = Type {
        name: "InputM".into(),
        constructors: vec![
            Constructor {
                name: "Return".into(),
                parameters: vec!["value".into()],
            },
            Constructor {
                name: "Print".into(),
                parameters: vec!["value".into(), "next".into()],
            },
        ],
    };
    */

    ret
}

impl Context {
    pub fn add_stdlib(&mut self) {
        self.modules.insert("stdlib".to_string(), stdlib());
    }
}
