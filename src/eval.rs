use std::rc::Rc;

use crate::ast::{BinOp, Expr};
use crate::context::Context;
use crate::value::{BuiltinFunc, ConstructorFunc, Func, LambdaFunc, Value};
use functionality::Mutate;
use thiserror::Error;

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
    pub fn apply(&self, arg: Value) -> Value {
        let mut context = self.closure.as_ref().clone();
        context.vars.insert(self.param_name.clone(), arg);
        self.body.eval(&context)
    }
}

trait LotsOfParametersFunc: Into<Value> {
    fn apply_finalize(&self, args: Vec<Value>) -> Value;

    fn arity(&self) -> u8;

    fn applied_already(&self) -> &Vec<Value>;
    fn with_applied_already(&self, applied_already: Vec<Value>) -> Self;

    fn apply(&self, arg: Value) -> Value {
        assert!(0 < self.arity(), "Arity must be positive");
        assert!(self.applied_already().len() < self.arity() as _, "This function has too many already-applied arguments!");

        // Let's apply the argument
        let args = 
            self.applied_already().clone()
            .mutate(move |v| v.push(arg));

        // Then we branch on whether we finished or not
        if self.arity() == 1 {
            self.apply_finalize(args)
        } else {
            self.with_applied_already(args).into()
        }
    }
}

impl LotsOfParametersFunc for BuiltinFunc {
    fn apply_finalize(&self, args: Vec<Value>) -> Value {
        (self.definition.func)(&args)
    }

    fn arity(&self) -> u8 { self.definition.arity }

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

impl LotsOfParametersFunc for ConstructorFunc {
    fn apply_finalize(&self, args: Vec<Value>) -> Value {
        assert_eq!(self.arity() as usize, args.len());
        Value::Constructed(self.constructor.clone(), args)
    }

    fn arity(&self) -> u8 {
        self.constructor.parameters.len() as _
    }

    fn applied_already(&self) -> &Vec<Value> {
        &self.applied_already
    }

    fn with_applied_already(&self, applied_already: Vec<Value>) -> Self {
        Self {
            applied_already,
            constructor: self.constructor.clone(),
        }
    }
}

impl Func {
    pub fn apply(&self, arg: Value) -> Value {
        match self {
            Func::Lambda(f) => f.apply(arg),
            Func::Builtin(f) => f.apply(arg),
            Func::Constructor(f) => f.apply(arg),
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn apply_all(&self, args: &[Value]) -> Result<Value, ApplyAllError> {
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
                    });
                }
            };
            let applied = func.apply(arg.clone());
            ret = applied;
        }
        Ok(ret)
    }
}

impl Expr {
    pub fn eval(&self, context: &Context) -> Value {
        match self {
            Expr::Int(i) => Value::Int(*i),
            Expr::Str(s) => Value::Str(s.clone()),
            Expr::Var(name) => context
                .vars
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("Variable '{name}' not found")),
            Expr::BinOp(left, op, right) => {
                let lhs = left.eval(context);
                let rhs_lazy = || right.eval(context);
                Self::bin_op(lhs, *op, rhs_lazy)
            }
            Expr::Func(name, body) => LambdaFunc {
                param_name: name.clone(),
                closure: context.clone().into(),
                body: body.clone().into(),
            }.into(),
            Expr::App(exprs) => {
                assert!(
                    exprs.len() >= 2,
                    "`Expr::App` should have at least two elements",
                );
                let values = exprs.iter().map(|e| e.eval(context)).collect::<Vec<_>>();
                let func = &values[0];
                let args = &values[1..];
                let Value::Func(func) = func else {
                    panic!("First element of application must be a function, found: {func:?}");
                };
                func.apply_all(args)
                    .unwrap_or_else(|err| panic!("Function application error: {err}"))
            }
        }
    }

    fn bin_op(lhs: Value, op: BinOp, rhs: impl Fn() -> Value) -> Value {
        use BinOp::*;
        match op {
            Add => match (lhs, rhs()) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
                (Value::Str(l), Value::Str(r)) => Value::Str(l + &r),
                _ => panic!("Invalid types for addition"),
            },
            Sub => match (lhs, rhs()) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
                _ => panic!("Invalid types for subtraction"),
            },
            Mul => match (lhs, rhs()) {
                (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
                _ => panic!("Invalid types for multiplication"),
            },
            Div => match (lhs, rhs()) {
                (Value::Int(l), Value::Int(r)) => {
                    if r == 0 {
                        panic!("Division by zero");
                    }
                    Value::Int(l / r)
                }
                _ => panic!("Invalid types for division"),
            },
            Eq => Value::Bool(lhs == rhs()),
            And => match lhs {
                Value::Bool(true) => rhs(),
                Value::Bool(false) => Value::Bool(false),
                _ => panic!("Invalid type for logical AND"),
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
        assert_eq!(expr.eval(&context), Value::Int(42));
    }
}
