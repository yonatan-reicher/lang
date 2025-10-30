use std::{collections::HashMap, rc::Rc};

use crate::ast;
use derive_more::Display;
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error("unknown variable")]
    UnknownVariable(Rc<str>),
}

type Result<T = Rc<Type>, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug, Display, PartialEq)]
pub enum Type {
    #[display("Bool")]
    Bool,
    #[display("Int")]
    Int,
    #[display("Str")]
    Str,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Context {
    vars: HashMap<Rc<str>, Rc<Type>>,
}

impl Context {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_var(&mut self, name: Rc<str>, t: Rc<Type>) -> Option<Rc<Type>> {
        self.vars.insert(name, t)
    }
}

// impl From<crate::context::Context> for Context {
//     fn from(c: crate::context::Context) -> Context {
//         vars: HashMap<String, Value>,
//         modules: HashMap<String, Module>,
//         out: PrintOutput,
//         c.vars.insert(name, 
//         todo!()
//     }
// }

impl ast::Expr {
    pub fn infer(&self, c: &mut Context) -> Result<Rc<Type>> {
        match self {
            // TODO: Make these singletons?
            ast::Expr::Bool(_) => Ok(Rc::new(Type::Bool)),
            ast::Expr::Int(_) => Ok(Rc::new(Type::Int)),
            ast::Expr::Str(_) => Ok(Rc::new(Type::Str)),
            ast::Expr::Var(s) => c
                .vars
                .get(s.as_str())
                .cloned()
                .ok_or_else(|| Error::UnknownVariable(Rc::from(s.as_str()))),
            ast::Expr::BinOp(lhs, bin_op, rhs) => {
                let lhs = lhs.infer(c)?;
                let rhs = rhs.infer(c)?;
                use ast::BinOp::*;
                match (bin_op, lhs.as_ref(), rhs.as_ref()) {
                    // int
                    (Add | Sub | Mul | Div | Eq, Type::Int, Type::Int) => Ok(lhs.clone()),
                    // str
                    (Add, Type::Str, Type::Str) => Ok(lhs.clone()),
                    // eq
                    (Eq, a, b) if a == b => Ok(lhs.clone()),
                    // bool
                    (And, Type::Bool, Type::Bool) => Ok(Rc::new(Type::Bool)),
                    // error
                    _ => todo!("bad op typing"),
                }
            }
            ast::Expr::Func(_, expr) => todo!(),
            ast::Expr::App(exprs) => todo!(),
            ast::Expr::Match(expr, match_arms) => todo!(),
            ast::Expr::Statements(statements, expr) => todo!(),
            ast::Expr::If(_) => todo!(),
        }
    }
}

impl ast::Statement {
    pub fn infer(&self, c: &mut Context) -> Result<()> {
        match self {
            ast::Statement::Assignment(name, expr) => {
                let expr_type = expr.infer(c)?;
                c.add_var(name.as_str().into(), expr_type);
            }
            ast::Statement::Print(expr) => (),
            ast::Statement::Import { module_name, imports } => {
                todo!()
            }
            ast::Statement::Label { name, parameters } => todo!(),
        }
        Ok(())
    }
}

impl ast::Program {
    pub fn infer(&self, c: &mut Context) -> Result<Option<Rc<Type>>> {
        // TODO: Maybe we need to use the `module_decl` field in some way?
        for s in &self.statements {
            s.infer(c)?;
        }
        let t = self.return_expr.as_ref().map(|x| x.infer(c)).transpose()?;
        Ok(t)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use functionality::Mutate;

    #[test]
    fn basic() {
        use ast::Expr::*;
        let e = BinOp(
            Box::new(Var("x".into())),
            ast::BinOp::Sub,
            Box::new(Int(64)),
        );
        let mut c = Context::new().mutate(|c| {
            c.add_var("x".into(), Rc::new(Type::Int));
        });
        let t = e.infer(&mut c).expect("should not err");
        assert_eq!(*t.as_ref(), Type::Int);
    }
}
