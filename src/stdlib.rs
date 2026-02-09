use std::cell::RefCell;
use std::rc::Rc;

use functionality::prelude::*;

use crate::context::{Context, Module};
use crate::labeled::{Label, LabelInfo, Labeled};
use crate::value::{BuiltinDefinition, Func, LabelFunc, Value};

pub struct Stdlib {
    pub io_commands: IoCommands,
    pub list: List,
    pub other: Other,
}

#[derive(Clone, Debug)]
pub struct IoCommands {
    pub input: Label,
    pub ls: Label,
    pub none: Label,
    pub print: Label,
    pub read: Label,
    pub write: Label,
    pub is_dir: Label,
}

#[derive(Clone, Debug)]
pub struct List {
    pub cons: Label,
    pub nil: Label,
}

#[derive(Clone, Debug)]
pub struct Other {
    pub abs: Rc<Func>,
    pub neg: Rc<Func>,
    pub fix: Rc<Func>,
    pub not: Rc<Func>,
}

impl Module {
    pub fn insert(&mut self, builtin: BuiltinDefinition) {
        self.values.insert(builtin.name.clone(), builtin.into());
    }
}

impl Stdlib {
    pub const N_IO_COMMANDS: usize = 7;
    pub fn io_commands_all(&self) -> [Label; Self::N_IO_COMMANDS] {
        let IoCommands {
            input,
            ls,
            none,
            print,
            read,
            write,
            is_dir,
        } = &self.io_commands;
        [
            input.clone(),
            ls.clone(),
            none.clone(),
            print.clone(),
            read.clone(),
            write.clone(),
            is_dir.clone(),
        ]
    }
}

impl Stdlib {
    pub fn new() -> Stdlib {
        let abs = BuiltinDefinition {
            name: "abs".into(),
            arity: 1,
            func: Rc::new(|values| {
                let [Value::Int(i)] = values else {
                    panic!("AARG");
                };
                Ok(i.abs().into())
            }),
        }
        .pipe(Func::from)
        .pipe(Rc::new);
        let neg = BuiltinDefinition {
            name: "neg".into(),
            arity: 1,
            func: Rc::new(|values| {
                let [Value::Int(i)] = values else {
                    panic!("AARG");
                };
                Ok((-i).into())
            }),
        }
        .pipe(Func::from)
        .pipe(Rc::new);
        let fix_cell: Rc<RefCell<Option<Rc<Func>>>> = Default::default();
        let fix_cell_clone = fix_cell.clone();
        let fix = BuiltinDefinition {
            name: "fix".into(),
            arity: 1,
            func: Rc::new(move |values| {
                let [Value::Func(f)] = values else {
                    panic!("fix should be called with a single function argument");
                };
                let fix0 = fix_cell_clone.clone();
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
                                .apply_all(&[f0.clone().into(), arg.clone()])
                        }),
                    }
                    .into(),
                )
            }),
        }
        .pipe(Func::from)
        .pipe(Rc::new);
        *fix_cell.borrow_mut() = Some(fix.clone());
        let not = BuiltinDefinition {
            name: "not".into(),
            arity: 1,
            func: Rc::new(|values| {
                let arg = values.first().unwrap();
                match arg {
                    Value::Bool(b) => Ok(Value::Bool(!b)),
                    _ => todo!(),
                }
            }),
        }.pipe(Func::from).pipe(Rc::new);
        let other = Other {
            abs,
            neg,
            fix,
            not,
        };

        let io_commands = IoCommands {
            input: Label::new(LabelInfo {
                name: "Input".into(),
                params: vec!["f".to_string()],
            }),
            ls: Label::new(LabelInfo {
                name: "Ls".into(),
                params: vec!["path".into(), "f".into()],
            }),
            none: Label::new(LabelInfo {
                name: "None".into(),
                params: vec![],
            }),
            print: Label::new(LabelInfo {
                name: "Print".into(),
                params: vec!["value".to_string(), "next".to_string()],
            }),
            read: Label::new(LabelInfo {
                name: "Read".into(),
                params: vec!["path".to_string(), "f".into()],
            }),
            write: Label::new(LabelInfo {
                name: "Write".into(),
                params: vec!["path".to_string(), "text".into(), "next".into()],
            }),
            is_dir: Label::new(LabelInfo {
                name: "IsDir".into(),
                params: vec!["path".to_string(), "f".into()],
            }),
        };

        let list = List {
            cons: Label::new(LabelInfo {
                name: "Cons".into(),
                params: vec!["head".into(), "tail".into()],
            }),
            nil: Label::new(LabelInfo {
                name: "Nil".into(),
                params: vec![],
            }),
        };

        Stdlib {
            io_commands,
            list,
            other,
        }
    }

    pub fn module(&self) -> Module {
        // using this pattern lets us make sure we are using all of the things
        // we defined
        let Self {
            io_commands:
                IoCommands {
                    input,
                    ls,
                    none,
                    print,
                    read,
                    write,
                    is_dir,
                },
            list: List { cons, nil },
            other:
                Other {
                    abs,
                    neg,
                    fix,
                    not,
                },
        } = self;
        Module {
            values: [
                ("Input".into(), Value::from(LabelFunc::from(input.clone()))),
                ("Ls".into(), LabelFunc::from(ls.clone()).into()),
                ("None".into(), Labeled::new(none.clone(), vec![]).into()),
                ("Print".into(), LabelFunc::from(print.clone()).into()),
                ("Read".into(), LabelFunc::from(read.clone()).into()),
                ("Write".into(), LabelFunc::from(write.clone()).into()),
                ("IsDir".into(), LabelFunc::from(is_dir.clone()).into()),
                ("Abs".into(), abs.clone().into()),
                ("Neg".into(), neg.clone().into()),
                ("Fix".into(), fix.clone().into()),
                ("Cons".into(), LabelFunc::from(cons.clone()).into()),
                (
                    "Nil".into(),
                    Labeled::new_no_args(nil.clone()).into(),
                ),
                ("abs".into(), abs.clone().into()),
                ("neg".into(), neg.clone().into()),
                ("fix".into(), fix.clone().into()),
                ("not".into(), not.clone().into()),
            ]
            .into(),
        }
    }

    pub fn nil(&self) -> Value {
        Labeled::new_no_args(self.list.nil.clone()).into()
    }

    pub fn to_list<I>(&self, x: I) -> Value
    where
        I: IntoIterator<Item = Value>,
        I::IntoIter: DoubleEndedIterator,
    {
        x.into_iter().rev().fold(self.nil(), |acc, item| {
            Labeled::new(
                self.list.cons.clone(),
                vec![item, acc],
            )
            .into()
        })
    }
}

impl Default for Stdlib {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn add_stdlib(&mut self) -> Stdlib {
        let stdlib = Stdlib::new();
        self.modules.insert("stdlib".to_string(), stdlib.module());
        stdlib
    }
}
