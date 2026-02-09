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
    #[error("a value of type '{0}' was applied like a function, but it is not one")]
    NotAFunction(Type),
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
    #[display("({} -> {})", _0.0, _0.1)]
    Func(Rc<(Type, Type)>),
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
                .get(s)
                .cloned()
                .ok_or_else(|| Error::UnknownVariable(s.clone())),
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
            ast::Expr::Func(param, expr) => {
                let t = Type::Int; // TODO
                let old = c.vars.insert(param.clone(), t.clone());
                let rhs = expr.infer(c)?;
                match old {
                    Some(old) => {
                        c.vars.insert(param.clone(), old);
                    }
                    None => {
                        c.vars.remove(param);
                    }
                }
                Ok(Type::Func(Rc::new((t, rhs))))
            }
            ast::Expr::App(exprs) => {
                assert!(exprs.len() >= 2);
                let types = exprs
                    .iter()
                    .map(|a| a.infer(c))
                    .collect::<Result<Vec<_>>>()?;
                let mut lhs_type = &types[0];
                let arg_types = &types[1..];
                for arg_type in arg_types {
                    let Type::Func(f) = lhs_type else {
                        return Err(Error::NotAFunction(lhs_type.clone()));
                    };
                    let (param_type, rhs_type) = f.as_ref();
                    if param_type != arg_type {
                        todo!()
                    }
                    lhs_type = rhs_type
                }
                Ok(lhs_type.clone())
            }
            ast::Expr::Match(input, arms) => {
                let input_type = input.infer(c)?;
                let mut ret = None;
                for ast::MatchArm(p, e) in arms {
                    let vars = p.infer(&input_type)?;
                    let c = &mut c.clone();
                    for (v, t) in vars {
                        c.vars.insert(v, t);
                    }
                    let t = e.infer(c)?;
                    match &ret {
                        None => ret = Some(t),
                        Some(ret) if t == *ret => (),
                        Some(_) => todo!(),
                    }
                }
                Ok(ret.unwrap())
            }
            ast::Expr::Statements(statements, expr) => {
                let c = &mut c.clone();
                for s in statements {
                    s.infer(c)?
                }
                expr.infer(c)
            }
            ast::Expr::If(e) => {
                let (cond, x, y) = e.as_ref();
                let cond_type = cond.infer(c)?;
                if cond_type != Type::Bool {
                    todo!()
                }
                let x_type = x.infer(c)?;
                let y_type = y.infer(c)?;
                if x_type != y_type {
                    todo!()
                }
                Ok(x_type)
            }
        }
    }
}

impl ast::Statement {
    pub fn infer(&self, c: &mut Context) -> Result<()> {
        match self {
            ast::Statement::Assignment(name, expr) => {
                let expr_type = expr.infer(c)?;
                c.add_var(name.clone(), expr_type);
            }
            ast::Statement::Print(_) => (),
            ast::Statement::Import {
                module_name,
                exposing: imports,
            } => {
                let Some(module) = c.modules.get(module_name) else {
                    return Err(Error::NoModule {
                        name: module_name.clone(),
                    });
                };
                for item_name in imports {
                    let Some(t) = module.items.get(item_name) else {
                        return Err(Error::NoModuleItem {
                            module: module_name.clone(),
                            item: item_name.clone(),
                        });
                    };
                    c.vars.insert(item_name.clone(), t.clone());
                }
            }
            ast::Statement::Label { .. } => todo!(),
        }
        Ok(())
    }
}

impl ast::Pattern {
    pub fn infer(&self, input: &Type) -> Result<Vec<(Rc<str>, Type)>> {
        Ok(match self {
            ast::Pattern::Var(id) => vec![(id.clone(), input.clone())],
            ast::Pattern::Wildcard => vec![],
            ast::Pattern::Label { .. } => todo!(),
        })
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

    #[test]
    fn func_to_string() {
        let t = Type::Func(Rc::new((Type::Str, Type::Int)));
        assert_eq!(t.to_string(), "(Str -> Int)");
    }
}
