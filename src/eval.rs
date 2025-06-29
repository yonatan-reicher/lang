use crate::ast::{BinOp, Expr};
use crate::context::Context;
use crate::value::{Func, Value};
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

impl Func {
    pub fn apply(&self, arg: Value) -> Value {
        let mut context = self.closure.as_ref().clone();
        context.vars.insert(self.name.clone(), arg);
        self.body.eval(&context)
    }

    pub fn apply_all(&self, args: &[Value]) -> Result<Value, ApplyAllError> {
        let mut ret = Value::Func(self.clone());
        for (i, arg) in args.iter().enumerate() {
            let func = match ret {
                Value::Func(func) => func,
                _ => return Err(ApplyAllError {
                    func: self.clone(),
                    args: args.to_vec(),
                    returns: ret,
                    on_arg: i,
                }),
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
            Expr::Func(name, body) => Value::Func(Func {
                name: name.clone(),
                closure: context.clone().into(),
                body: body.clone().into(),
            }),
            Expr::App(exprs) => {
                assert!(
                    exprs.len() >= 2,
                    "`Expr::App` should have at least two elements",
                );
                let values = exprs.iter()
                    .map(|e| e.eval(context)).collect::<Vec<_>>();
                let func = &values[0];
                let args = &values[1..];
                let Value::Func(func) = func else {
                    panic!("First element of application must be a function, found: {:?}", func);
                };
                func.apply_all(args).unwrap_or_else(|err| {
                    panic!(
                        "Function application error: {}",
                        err
                    )
                })
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
        };
        let expr = Expr::Var("x".to_string());
        assert_eq!(expr.eval(&context), Value::Int(42));
    }
}
