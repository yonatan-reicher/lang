use crate::ast::{Expr, BinOp};
use crate::value::Value;
use crate::context::Context;

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
            }
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
