#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    name: String,
    statement: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assignment(String, Expr),
    Print(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(String),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    And,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Str(String),
}

#[derive(Debug, Default, Clone)]
pub struct Context {
    vars: std::collections::HashMap<String, Value>,
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
                .unwrap_or_else(|| panic!("Variable '{}' not found", name)),
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

impl Statement {
    pub fn execute(&self, context: &mut Context) {
        match self {
            Statement::Assignment(name, expr) => {
                let value = expr.eval(context);
                context.vars.insert(name.clone(), value);
            }
            Statement::Print(expr) => {
                let value = expr.eval(context);
                println!("{:?}", value);
            }
        }
    }
}

impl Program {
    pub fn execute(&self, context: &mut Context) {
        for statement in &self.statement {
            statement.execute(context);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn getting_a_variable() {
        let context = Context {
            vars: [("x".to_string(), Value::Int(42))].into_iter().collect(),
        };
        let expr = Expr::Var("x".to_string());
        assert_eq!(expr.eval(&context), Value::Int(42));
    }

    #[test]
    fn statments() {
        let s1 = Statement::Assignment("x".to_string(), Expr::Int(100));
        let s2 = Statement::Print(Expr::Var("x".to_string()));
        let mut context = Context {
            vars: [("x".to_string(), Value::Int(42))].into_iter().collect(),
        };
        s1.execute(&mut context);
        s2.execute(&mut context);
        assert_eq!(context.vars.get("x"), Some(&Value::Int(100)));
    }

    #[test]
    fn test_and() {
        use super::BinOp::*;
        use Expr::*;
        use Statement::*;
        let program = Program {
            name: "Foo".to_string(),
            statement: vec![
                Assignment("x".to_string(), Int(10)),
                Assignment("y".to_string(), Str("10".into())),
                Assignment("x".to_string(), BinOp(
                    BinOp(Var("x".into()).into(), Eq, Var("y".into()).into()).into(),
                    And,
                    Var("x".to_string()).into(),
                )),
            ],
        };
        let mut context = Context::default();
        program.execute(&mut context);
        assert_eq!(context.vars.get("x"), Some(&Value::Bool(false)));
    }
}
