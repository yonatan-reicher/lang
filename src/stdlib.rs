use std::cell::RefCell;
use std::rc::Rc;

use functionality::prelude::*;

use crate::context::{Context, Module};
use crate::value::{BuiltinDefinition, Func, Label, LabelFunc, Labeled, PLabel, Value};

pub struct Stdlib {
    pub io_commands: IoCommands,
    pub list: List,
    pub other: Other,
}

#[derive(Clone, Debug)]
pub struct IoCommands {
    pub input: PLabel,
    pub ls: PLabel,
    pub none: PLabel,
    pub print: PLabel,
    pub read: PLabel,
    pub write: PLabel,
    pub is_dir: PLabel,
}

#[derive(Clone, Debug)]
pub struct List {
    pub cons: PLabel,
    pub nil: PLabel,
}

#[derive(Clone, Debug)]
pub struct Other {
    pub abs: Rc<Func>,
    pub neg: Rc<Func>,
    pub fix: Rc<Func>,
}

impl Module {
    pub fn insert(&mut self, builtin: BuiltinDefinition) {
        self.values.insert(builtin.name.clone(), builtin.into());
    }
}

impl Stdlib {
    pub const N_IO_COMMANDS: usize = 7;
    pub fn io_commands_all(&self) -> [PLabel; Self::N_IO_COMMANDS] {
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
        }.pipe(Func::from).pipe(Rc::new);
        let neg = BuiltinDefinition {
            name: "neg".into(),
            arity: 1,
            func: Rc::new(|values| {
                let [Value::Int(i)] = values else {
                    panic!("AARG");
                };
                Ok((-i).into())
            }),
        }.pipe(Func::from).pipe(Rc::new);
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
        }.pipe(Func::from).pipe(Rc::new);
        *fix_cell.borrow_mut() = Some(fix.clone());
        let other = Other { abs, neg, fix };

        let io_commands = IoCommands {
            input: PLabel::from(Label {
                name: "Input".into(),
                parameters: vec!["f".to_string()],
            }),
            ls: PLabel::from(Label {
                name: "Ls".into(),
                parameters: vec!["path".into(), "f".into()],
            }),
            none: PLabel::from(Label {
                name: "None".into(),
                parameters: vec![],
            }),
            print: PLabel::from(Label {
                name: "Print".into(),
                parameters: vec!["value".to_string(), "next".to_string()],
            }),
            read: PLabel::from(Label {
                name: "Read".into(),
                parameters: vec!["path".to_string(), "f".into()],
            }),
            write: PLabel::from(Label {
                name: "Write".into(),
                parameters: vec!["path".to_string(), "text".into(), "next".into()],
            }),
            is_dir: PLabel::from(Label {
                name: "IsDir".into(),
                parameters: vec!["path".to_string(), "f".into()],
            }),
        };

        let list = List {
            cons: PLabel::from(Label {
                name: "Cons".into(),
                parameters: vec!["head".into(), "tail".into()],
            }),
            nil: PLabel::from(Label {
                name: "Nil".into(),
                parameters: vec![],
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
            other: Other { abs, neg, fix },
        } = self;
        Module {
            values: [
                ("Input".into(), LabelFunc::from(input.clone()).into()),
                ("Ls".into(), LabelFunc::from(ls.clone()).into()),
                (
                    "None".into(),
                    Labeled {
                        label: none.clone(),
                        args: vec![],
                    }.into(),
                ),
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
                    Labeled {
                        label: nil.clone(),
                        args: vec![],
                    }.into(),
                ),
                ("abs".into(), abs.clone().into()),
                ("neg".into(), neg.clone().into()),
                ("fix".into(), fix.clone().into()),
            ]
            .into(),
        }
    }

    pub fn nil(&self) -> Value {
        Labeled {
            label: self.list.nil.clone(),
            args: vec![],
        }.into()
    }

    pub fn to_list<I>(&self, x: I) -> Value
    where
        I: IntoIterator<Item = Value>,
        I::IntoIter: DoubleEndedIterator,
    {
        x.into_iter()
            .rev()
            .fold(self.nil(), |acc, item| Labeled {
                label: self.list.cons.clone(),
                args: vec![item, acc],
            }.into())
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
