use crate::context::Context;
use crate::value::{Func, LabelFunc, PLabel, Value};

fn lbl(value: &Value) -> Option<PLabel> {
    if let Value::Func(Func::Label(LabelFunc { label, .. })) = value {
        return Some(label.clone());
    } else if let Value::Labeled { label, .. } = value {
        return Some(label.clone());
    }
    None
}
fn lbl_stdlib(name: &'static str, context: &Context) -> Option<PLabel> {
    lbl(context.modules["stdlib"].values.get(name)?)
}

#[derive(Clone, Copy, Debug, thiserror::Error)]
pub enum IoValueError {
    #[error("print io value should have exactly 2 parameters")]
    PrintDidNotHave2Parameters,
    #[error("input io value should have exactly 1 parameter, and it must be a function")]
    InputDidNotHaveASingleFunctionParamter,
    #[error("none io value should have exactly 0 parameters")]
    NoneShouldHaveZeroParameters,
}

#[derive(Debug, derive_more::From, thiserror::Error)]
pub enum Error {
    #[error("io command error: {0}, on command '{1}'")]
    IoValue(IoValueError, Value),
    #[error("eval error (from io command): {0}")]
    Eval(crate::eval::Error),
    #[error("io error: {0}")]
    Io(std::io::Error),
}

/// Takes a labeled value, or a label function or closure, and returns it's
/// label and the arguments that have been applied to it.
fn unlabel(x: &Value) -> Option<(&PLabel, &Vec<Value>)> {
    match x {
        Value::Labeled { label, arguments } => Some((label, arguments)),
        Value::Func(Func::Label(LabelFunc {
            label,
            applied_already,
        })) => Some((label, applied_already)),
        _ => None,
    }
}

pub fn execute(
    x: &Value,
    context: &Context,
    input: &mut impl std::io::BufRead,
) -> Result<(), Error> {
    let print_label = lbl_stdlib("Print", context).unwrap();
    let input_label = lbl_stdlib("Input", context).unwrap();
    let none_label = lbl_stdlib("None", context).unwrap();

    fn other_value<E>(x: &Value) -> Result<(), E> {
        println!("{x}");
        Ok(())
    }

    if let Some((label, arguments)) = unlabel(x) {
        // Print
        if *label == print_label {
            let [value, next] = arguments.as_slice() else {
                return Err(Error::IoValue(
                    IoValueError::PrintDidNotHave2Parameters,
                    x.clone(),
                ));
            };
            println!("{value}");
            execute(next, context, input)
        // Input
        } else if *label == input_label {
            let [Value::Func(f)] = arguments.as_slice() else {
                return Err(Error::IoValue(
                    IoValueError::InputDidNotHaveASingleFunctionParamter,
                    x.clone(),
                ));
            };
            let mut line = String::new();
            input.read_line(&mut line)?;
            line = line.trim_end_matches("\r\n").trim_end_matches("\n").into();
            let next = f.apply(line.into())?;
            execute(&next, context, input)
        // None
        } else if *label == none_label {
            if !arguments.is_empty() {
                return Err(Error::IoValue(
                    IoValueError::NoneShouldHaveZeroParameters,
                    x.clone(),
                ));
            }
            Ok(())
        } else {
            other_value(x)
        }
    } else {
        other_value(x)
    }
}
