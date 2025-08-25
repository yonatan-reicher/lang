use crate::ast::{Program, Statement};
use crate::context::Context;
use crate::value::{Label, LabelFunc, PLabel, Value};
use crate::eval;

impl Statement {
    pub fn execute(&self, context: &mut Context) -> eval::Result<()> {
        match self {
            Statement::Assignment(name, expr) => {
                let value = expr.eval(context)?;
                context.vars.insert(name.clone(), value);
            }
            Statement::Print(expr) => {
                let value = expr.eval(context)?;
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
            Statement::Label { name, parameters } => {
                let label = PLabel::from(Label {
                    name: name.clone(),
                    parameters: parameters.clone(),
                });
                // The value we are introducing to the context.
                let value = if parameters.is_empty() {
                    Value::Labeled {
                        label,
                        arguments: vec![],
                    }
                } else {
                    LabelFunc::from(label).into()
                };
                // The constructors are added under a new module with a name matching the name of
                // the type being defined.
                context.vars.insert(name.clone(), value);
            }
        }
        Ok(())
    }
}

impl Program {
    pub fn execute(&self, context: &mut Context) -> eval::Result<Option<Value>> {
        for statement in &self.statements {
            statement.execute(context)?;
        }
        self.return_expr.clone().map(|e| e.eval(context)).transpose()
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

        s1.execute(&mut context).unwrap(); // x = 100;
        s2.execute(&mut context).unwrap(); // print x;
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
            statements: vec![
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
            return_expr: None,
        };
        let mut context = Context::default();
        program.execute(&mut context).unwrap();
        assert_eq!(context.vars.get("x"), Some(&Value::Bool(false)));
    }
}
