use std::{collections::HashMap, rc::Rc};

use crate::ast;
use derive_more::Display;
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error("unknown variable")]
    UnknownVariable(Rc<str>),
    #[error("no module '{name}'")]
    NoModule { name: Rc<str> },
    #[error("module '{module}' does not contain an item '{item}'")]
    NoModuleItem { module: Rc<str>, item: Rc<str> },
    #[error("operator '{op}' cannot be used on '{lhs}' and '{rhs}'")]
    BadOperator {
        op: ast::BinOp,
        lhs: Type,
        rhs: Type,
    },
}

type Result<T = Type, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug, Display, PartialEq, Eq)]
pub enum Type {
    #[display("Bool")]
    Bool,
    #[display("Int")]
    Int,
    #[display("Str")]
    Str,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Module {
    items: HashMap<Rc<str>, Type>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Context {
    modules: HashMap<Rc<str>, Module>,
    vars: HashMap<Rc<str>, Type>,
}

impl Context {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_var(&mut self, name: Rc<str>, t: Type) -> Option<Type> {
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
    pub fn infer(&self, c: &mut Context) -> Result {
        match self {
            ast::Expr::Bool(_) => Ok(Type::Bool),
            ast::Expr::Int(_) => Ok(Type::Int),
            ast::Expr::Str(_) => Ok(Type::Str),
            ast::Expr::Var(s) => c
                .vars
                .get(s.as_str())
                .cloned()
                .ok_or_else(|| Error::UnknownVariable(Rc::from(s.as_str()))),
            ast::Expr::BinOp(lhs, bin_op, rhs) => {
                let lhs = lhs.infer(c)?;
                let rhs = rhs.infer(c)?;
                use ast::BinOp::*;
                match (bin_op, lhs, rhs) {
                    // int
                    (Add | Sub | Mul | Div, Type::Int, Type::Int) => Ok(Type::Int),
                    // str
                    (Add, Type::Str, Type::Str) => Ok(Type::Str),
                    // eq
                    (Eq | NEq, a, b) if a == b => Ok(Type::Bool),
                    // bool
                    (And | Or, Type::Bool, Type::Bool) => Ok(Type::Bool),
                    // error
                    (bin_op, lhs, rhs) => Err(Error::BadOperator {
                        op: *bin_op,
                        lhs,
                        rhs,
                    }),
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
            ast::Statement::Print(_) => (),
            ast::Statement::Import { module_name, imports } => {
                let module_name: Rc<str> = module_name.as_str().into();
                let Some(module) = c.modules.get(&module_name) else {
                    return Err(Error::NoModule { name: module_name });
                };
                for item_name in imports {
                    let item_name: Rc<str> = item_name.as_str().into();
                    let Some(t) = module.items.get(&item_name) else {
                        return Err(Error::NoModuleItem { module: module_name, item: item_name });
                    };
                    c.vars.insert(item_name.clone(), t.clone());
                }
            }
            ast::Statement::Label { name, parameters } => todo!(),
        }
        Ok(())
    }
}

impl ast::Program {
    pub fn infer(&self, c: &mut Context) -> Result<Option<Type>> {
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
            c.add_var("x".into(), Type::Int);
        });
        let t = e.infer(&mut c).expect("should not err");
        assert_eq!(t, Type::Int);
    }
}
