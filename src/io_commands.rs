use std::path::Path;
use std::rc::Rc;

use crate::stdlib::Stdlib;
use crate::value::{Func, Value};

use derive_more::{Display, From};

// First, let's define some types.

/// This is a parsed representation of IO commands.
/// IO commands are values of the labels in `Stdlib.io_commands`.
#[derive(Clone, Debug, Display)]
pub enum IoCommand {
    #[display("(Input {_0})")]
    Input(Rc<Func>),
    #[display("(Ls {0} {_1})", _0.display())]
    Ls(Rc<Path>, Rc<Func>),
    #[display("None")]
    None,
    #[display("(Ls {value} {next})")]
    Print { value: Value, next: Rc<IoCommand> },
    #[display("(Read {0} {_1})", _0.display())]
    Read(Rc<Path>, Rc<Func>),
    #[display("(Write {} {text} {next})", path.display())]
    Write {
        path: Rc<Path>,
        text: Rc<str>,
        next: Rc<IoCommand>,
    },
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum IoCommandError {
    #[error("field '{field}' of '{value}' is not a function (had value '{had_value}')")]
    NotAFunction {
        value: Value,
        field: &'static str,
        had_value: Value,
    },
    #[error("the value '{value}' is not an io command")]
    NotAnIoCommand { value: Value },
    #[error(
        "the io command value '{bad_value}' is an invalid io command (hint: look at the fields, they might be wrong)"
    )]
    General { bad_value: Value },
    // #[error("from command '{0}': {1}")]
    // FromCommand(IoCommand, Box<IoCommandError>),
}

/// This is just the monolithic error type for this module.
#[derive(Debug, From, thiserror::Error)]
pub enum Error {
    #[error("io command error: {0}")]
    IoCommand(IoCommandError),
    #[error("eval error (from io command '{from_command}'): {eval_error}")]
    Eval {
        from_command: IoCommand,
        eval_error: crate::eval::Error,
    },
    #[error("io error: {0}")]
    Io(std::io::Error),
}

// Some helpers

macro_rules! args {
    ($value:expr, [ $($arg:pat),* ] = $args:expr) => {
        let [$($arg),*] = $args else {
            return Err(IoCommandError::General {
                bad_value: $value.clone(),
            });
        };
    };
}

macro_rules! func {
    ($value:expr, $field:literal, Func $f:ident = $arg:expr) => {
        let Value::Func($f) = $arg else {
            return Err(IoCommandError::NotAFunction {
                value: $value.clone(),
                field: $field,
                had_value: $arg.clone(),
            });
        };
    };
}

// Now let's implement some behavior.

impl IoCommand {
    pub fn of_value(x: &Value, stdlib: &Stdlib) -> Result<Self, IoCommandError> {
        let Some((label, args)) = x.unlabel() else {
            return Err(IoCommandError::NotAnIoCommand { value: x.clone() });
        };

        // Using deconstruction to ensure we use all the fields.
        let crate::stdlib::IoCommands {
            input,
            ls,
            none,
            print,
            read,
            write,
        } = &stdlib.io_commands;

        // Categorize by label.
        if label == input {
            args!(x, [f] = args);
            func!(x, "f", Func f = f);
            Ok(IoCommand::Input(f.clone()))
        } else if label == ls {
            args!(x, [Value::Str(path), f] = args);
            func!(x, "f", Func f = f);
            let path = Path::new(path.as_ref());
            Ok(IoCommand::Ls(path.into(), f.clone()))
        } else if label == none {
            args!(x, [] = args);
            Ok(IoCommand::None)
        } else if label == print {
            args!(x, [value, next] = args);
            let next = IoCommand::of_value(next, stdlib)?;
            Ok(IoCommand::Print {
                value: value.clone(),
                next: Rc::new(next),
            })
        } else if label == read {
            args!(x, [Value::Str(path), f] = args);
            func!(x, "f", Func f = f);
            let path = Path::new(path.as_ref());
            Ok(IoCommand::Read(path.into(), f.clone()))
        } else if label == write {
            args!(x, [Value::Str(path), Value::Str(text), next] = args);
            let path = Path::new(path.as_ref());
            Ok(IoCommand::Write {
                path: path.into(),
                text: text.clone(),
                next: Rc::new(IoCommand::of_value(next, stdlib)?),
            })
        } else {
            Err(IoCommandError::NotAnIoCommand { value: x.clone() })
        }
    }

    /// Run this IO command.
    pub fn execute(&self, stdlib: &Stdlib, input: &mut impl std::io::BufRead) -> Result<(), Error> {
        let eval_error = |e| Error::Eval {
            from_command: self.clone(),
            eval_error: e,
        };
        match self {
            IoCommand::Input(f) => {
                let mut line = String::new();
                input.read_line(&mut line)?;
                line = line.trim_end_matches("\r\n").trim_end_matches("\n").into();
                let next = f.apply(line.into()).map_err(eval_error)?;
                let next = Self::of_value(&next, stdlib)?;
                next.execute(stdlib, input)
            }
            IoCommand::Ls(path, f) => {
                let files = path
                    .read_dir()?
                    .map(|entry| {
                        entry.map(|entry| {
                            entry
                                .file_name()
                                .to_str()
                                .expect("unhandled case: file-name with invalid utf-8")
                                .into()
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let files = stdlib.to_list(files);
                let next = f.apply(files).map_err(eval_error)?;
                let next = Self::of_value(&next, stdlib)?;
                next.execute(stdlib, input)
            }
            IoCommand::None => Ok(()),
            IoCommand::Print { value, next } => {
                println!("{value}");
                next.execute(stdlib, input)
            }
            IoCommand::Read(path, f) => {
                let text = std::fs::read_to_string(path)?;
                let next = f.apply(text.into()).map_err(eval_error)?;
                let next = Self::of_value(&next, stdlib)?;
                next.execute(stdlib, input)
            }
            IoCommand::Write { path, text, next } => {
                let text: &str = text;
                std::fs::write(path, text)?;
                next.execute(stdlib, input)
            }
        }
    }
}

impl Stdlib {
    pub fn execute(&self, x: &Value, input: &mut impl std::io::BufRead) -> Result<(), Error> {
        let command = IoCommand::of_value(x, self)?;
        command.execute(self, input)
    }
}
