use crate::ast::{Program, Statement};
use crate::context::Context;

impl Statement {
    pub fn execute(&self, context: &mut Context) {
        match self {
            Statement::Assignment(name, expr) => {
                let value = expr.eval(context);
                context.vars.insert(name.clone(), value);
            }
            Statement::Print(expr) => {
                let value = expr.eval(context);
                context.out.print(&value);
            }
            Statement::Import {
                module_name,
                imports,
            } => {
                let Some(module) = context.modules.get(module_name) else {
                    panic!("No module '{module_name}'");
                };
                for i in imports {
                    let Some(value) = module.values.get(i) else {
                        panic!("Module '{module_name}' does not contain a value named '{i}'");
                    };
                    context.vars.insert(i.clone(), value.clone());
                }
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
    use crate::ast::Expr;
    use crate::context::PrintOutput;
    use crate::value::Value;
    use functionality::Mutate;

    #[test]
    fn statments() {
        let s1 = Statement::Assignment("x".to_string(), Expr::Int(100));
        let s2 = Statement::Print(Expr::Var("x".to_string()));

        let out = Default::default();
        let mut context = Context {
            vars: [("x".to_string(), Value::Int(42))].into_iter().collect(),
            out: PrintOutput::Vec(std::rc::Rc::clone(&out)),
            modules: [].into(),
        }
        .mutate(Context::add_stdlib);

        s1.execute(&mut context); // x = 100;
        s2.execute(&mut context); // print x;
        assert_eq!(context.vars.get("x"), Some(&Value::Int(100)));
        assert_eq!(out.borrow().as_ref(), [Value::Int(100)]);
    }

    #[test]
    fn test_and() {
        use crate::ast::BinOp::*;
        use Expr::*;
        use Statement::*;
        let program = Program {
            module_decl: None,
            statement: vec![
                Assignment("x".to_string(), Int(10)),
                Assignment("y".to_string(), Str("10".into())),
                Assignment(
                    "x".to_string(),
                    BinOp(
                        BinOp(Var("x".into()).into(), Eq, Var("y".into()).into()).into(),
                        And,
                        Var("x".to_string()).into(),
                    ),
                ),
            ],
        };
        let mut context = Context::default();
        program.execute(&mut context);
        assert_eq!(context.vars.get("x"), Some(&Value::Bool(false)));
    }
}
