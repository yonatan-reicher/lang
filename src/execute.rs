use crate::ast::{Statement, Program};
use crate::eval::Context;

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
    use crate::value::Value;
    use crate::ast::Expr;

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
        use crate::ast::BinOp::*;
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
