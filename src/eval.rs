// std imports
use std::rc::Rc;
// our imports
use crate::ast::{BinOp, Expr};
use crate::context::Context;
use crate::value::{BuiltinFunc, Func, LabelFunc, LambdaFunc, Value};
// other imports
use functionality::{Mutate, Pipe};
use thiserror::Error;

/** An error in applying multiple arguments to a function. */
#[derive(Debug, Clone, Error)]
#[error(
    "Function application error: {func:?} was applied with arguments {args:?}, but was not able on the {on_arg_1}th argument, which was {the_arg:?}, the function returned {returns:?}. This cannot be applied, but there were still more arguments to apply.",
    on_arg_1 = on_arg + 1,
    the_arg = &args[*on_arg],
)]
pub struct ApplyAllError {
    func: Func,
    args: Vec<Value>,
    returns: Value,
    on_arg: usize,
}

impl LambdaFunc {
    pub fn apply(&self, arg: Value) -> Result<Value> {
        let mut context = self.closure.as_ref().clone();
        context.vars.insert(self.param_name.clone(), arg);
        self.body.eval(&context)
    }
}

/** Common functionality for functions that take many parameters. */
trait LotsOfParametersFunc: Into<Value> {
    fn apply_finalize(&self, args: Vec<Value>) -> Result<Value>;

    fn arity(&self) -> u8;

    fn applied_already(&self) -> &Vec<Value>;
    fn with_applied_already(&self, applied_already: Vec<Value>) -> Self;

    fn apply(&self, arg: Value) -> Result<Value> {
        assert!(0 < self.arity(), "Arity must be positive");
        assert!(
            self.applied_already().len() < self.arity() as _,
            "This function has too many already-applied arguments!"
        );

        // Let's apply the argument
        let args = self.applied_already().clone().mutate(move |v| v.push(arg));

        // Then we branch on whether we finished or not
        if self.arity() == args.len() as _ {
            self.apply_finalize(args)
        } else {
            Ok(self.with_applied_already(args).into())
        }
    }
}

impl LotsOfParametersFunc for BuiltinFunc {
    fn apply_finalize(&self, args: Vec<Value>) -> Result<Value> {
        (self.definition.func)(&args)
    }

    fn arity(&self) -> u8 {
        self.definition.arity
    }

    fn applied_already(&self) -> &Vec<Value> {
        &self.applied_already
    }

    fn with_applied_already(&self, applied_already: Vec<Value>) -> Self {
        Self {
            applied_already,
            definition: Rc::clone(&self.definition),
        }
    }
}

impl LotsOfParametersFunc for LabelFunc {
    fn apply_finalize(&self, arguments: Vec<Value>) -> Result<Value> {
        assert_eq!(self.arity() as usize, arguments.len());
        Value::Labeled {
            label: self.label.clone(),
            arguments,
        }.pipe(Ok)
    }

    fn arity(&self) -> u8 {
        self.label.parameters.len() as _
    }

    fn applied_already(&self) -> &Vec<Value> {
        &self.applied_already
    }

    fn with_applied_already(&self, applied_already: Vec<Value>) -> Self {
        Self {
            applied_already,
            label: self.label.clone(),
        }
    }
}

impl Func {
    pub fn apply(&self, arg: Value) -> Result<Value> {
        match self {
            Func::Lambda(f) => f.apply(arg),
            Func::Builtin(f) => f.apply(arg),
            Func::Label(f) => f.apply(arg),
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn apply_all(&self, args: &[Value]) -> Result<Value> {
        // We are going to continue applying arguments on this variable.
        let mut ret: Value = self.clone().into();
        // We want the index for the error message.
        for (i, arg) in args.iter().enumerate() {
            let func = match ret {
                Value::Func(func) => func,
                _ => {
                    return Err(ApplyAllError {
                        func: self.clone(),
                        args: args.to_vec(),
                        returns: ret,
                        on_arg: i,
                    }.into());
                }
            };
            let applied = func.apply(arg.clone())?;
            ret = applied;
        }
        Ok(ret)
    }
}

#[derive(Clone, Debug, Error)]
pub enum BinOpError {
    #[error("operator '&&' only supports Booleans on the left-hand side, but was given '{lhs}'")]
    And { lhs: Value },
    #[error("operator '||' only supports Booleans on the left-hand side, but was given '{lhs}'")]
    Or { lhs: Value },
    #[error("operator '{op}' only supports {supports}, but was given '{lhs}' and '{rhs}'")]
    Other {
        op: BinOp,
        supports: &'static str,
        lhs: Value,
        rhs: Value,
    },
}

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error("the variable '{name}' does not exist")]
    VariableNotFound { name: String },
    #[error("'{func}' is not a function, but is being used as one")]
    NotAFunction { func: Value },
    #[error("{0}")]
    ApplicationError(#[from] ApplyAllError),
    #[error("{0}")]
    BinOpError(#[from] BinOpError),
    #[error("division by zero - tried dividing '{lhs}' by '0'")]
    DivisionByZero { lhs: Value },
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

impl Expr {
    pub fn eval(&self, context: &Context) -> Result<Value> {
        match self {
            Expr::Int(i) => Ok(Value::Int(*i)),
            Expr::Str(s) => Ok(Value::Str(s.clone())),
            Expr::Var(name) => context
                .vars
                .get(name)
                .cloned()
                .ok_or_else(|| Error::VariableNotFound { name: name.clone() }),
            Expr::BinOp(left, op, right) => {
                let lhs = left.eval(context)?;
                let rhs_lazy = || right.eval(context);
                Self::bin_op(lhs, *op, rhs_lazy)
            }
            Expr::Func(name, body) => Ok(LambdaFunc {
                param_name: name.clone(),
                closure: context.clone().into(),
                body: body.clone().into(),
            }
            .into()),
            Expr::App(exprs) => {
                assert!(
                    exprs.len() >= 2,
                    "`Expr::App` should have at least two elements",
                );
                let values = exprs
                    .iter()
                    .map(|e| e.eval(context))
                    .collect::<Result<Vec<_>>>()?;
                let func = &values[0];
                let args = &values[1..];
                let Value::Func(func) = func else {
                    return Err(Error::NotAFunction { func: func.clone() });
                };
                func.apply_all(args)
            }
        }
    }

    fn bin_op(lhs: Value, op: BinOp, rhs: impl Fn() -> Result<Value>) -> Result<Value> {
        use BinOp::*;
        let err = {
            let lhs = lhs.clone();
            |s, r| {
                Err(BinOpError::Other {
                    op,
                    supports: s,
                    lhs,
                    rhs: r,
                }
                .into())
            }
        };
        match op {
            // Numeric operators
            Add => match (lhs, rhs()?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Str(l + &r)),
                (_, r) => err("Ints and Strings", r),
            },
            Sub => match (lhs, rhs()?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                (_, r) => err("Ints", r),
            },
            Mul => match (lhs, rhs()?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                (_, r) => err("Ints", r),
            },
            Div => match (lhs, rhs()?) {
                (Value::Int(l), Value::Int(r)) => {
                    if r == 0 {
                        Err(Error::DivisionByZero { lhs: Value::Int(l) })
                    } else {
                        Ok(Value::Int(l / r))
                    }
                }
                (_, r) => err("Ints", r),
            },
            // Comparison
            Eq => Ok(Value::Bool(lhs == rhs()?)),
            // Logical
            And => match lhs {
                Value::Bool(true) => rhs(),
                Value::Bool(false) => Ok(Value::Bool(false)),
                _ => Err(BinOpError::And { lhs: lhs.clone() }.into()),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::PrintOutput;

    #[test]
    fn getting_a_variable() {
        let context = Context {
            vars: [("x".to_string(), Value::Int(42))].into_iter().collect(),
            out: PrintOutput::Ignore,
            modules: Default::default(),
        };
        let expr = Expr::Var("x".to_string());
        assert_eq!(expr.eval(&context).unwrap(), Value::Int(42));
    }
}
