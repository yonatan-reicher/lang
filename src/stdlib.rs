use std::cell::RefCell;
use std::rc::Rc;

use functionality::prelude::*;

use crate::context::{Context, Module};
use crate::value::{BuiltinDefinition, Func, Label, LabelFunc, PLabel, Value};

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
            Ok(i.abs().into())
        }),
    });
    ret.insert(BuiltinDefinition {
        name: "neg".into(),
        arity: 1,
        func: Rc::new(|values| {
            let [Value::Int(i)] = values else {
                panic!("AARG");
            };
            Ok((-i).into())
        }),
    });
    let fix: Rc<RefCell<Option<BuiltinDefinition>>> = Default::default();
    let fix0 = fix.clone();
    let fix1 = BuiltinDefinition {
        name: "fix".into(),
        arity: 1,
        func: Rc::new(move |values| {
            let [Value::Func(f)] = values else {
                panic!("fix should be called with a single function argument");
            };
            let fix0 = fix0.clone();
            let f0 = f.clone();
            f.apply(
                BuiltinDefinition {
                    name: "f".into(),
                    arity: 1,
                    func: Rc::new(move |args| {
                        let arg = args.first().unwrap();
                        fix0.borrow()
                            .clone()
                            .unwrap()
                            .pipe(Func::from)
                            .apply_all(&[f0.clone().into(), arg.clone()])
                    }),
                }
                .into(),
            )
        }),
    };
    *fix.borrow_mut() = Some(fix1);
    ret.insert(fix.borrow().clone().unwrap());

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
        (
            "None".into(),
            Value::Labeled {
                label: none,
                arguments: vec![],
            },
        ),
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
