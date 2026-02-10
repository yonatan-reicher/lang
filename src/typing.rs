use std::{collections::HashMap, rc::Rc};

use crate::{ast, labeled::{Label, LabelInfo}};
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
    #[error("argument of type '{arg}' cannot be applied to function of type '{func}'")]
    ArgumentMismatch { func: Type, arg: Type },
    #[error("incompatible match arms {0:?}")]
    IncompatibleMatchArms(Vec<Type>),
    #[error("empty match")]
    EmptyMatch,
    #[error("if condition must be Bool, but was '{0}'")]
    IfConditionWasNotBool(Type),
    #[error("if arms had incompatible types '{0}' and '{1}'")]
    IncompatibleIfArms(Type, Type),
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
    #[display("{_0}")]
    Labeled(Rc<Label>),
}

// #[derive(Clone, Debug, Display, derive_more::From, PartialEq, Eq)]
// pub enum Item {
//     Type(Type),
//     Label(Label),
// }
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Module {
    vars: HashMap<Rc<str>, Type>,
    labels: HashMap<Rc<str>, Label>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Context {
    modules: HashMap<Rc<str>, Module>,
    vars: HashMap<Rc<str>, Type>,
    labels: HashMap<Rc<str>, Label>,
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
                        return Err(Error::ArgumentMismatch {
                            func: lhs_type.clone(),
                            arg: arg_type.clone(),
                        });
                    }
                    lhs_type = rhs_type
                }
                Ok(lhs_type.clone())
            }
            ast::Expr::Match(input, arms) => {
                let input_type = input.infer(c)?;
                let mut ret_types = vec![];
                for ast::MatchArm(p, e) in arms {
                    let vars = p.infer(&input_type)?;
                    let c = &mut c.clone();
                    c.vars.extend(vars);
                    ret_types.push(e.infer(c)?);
                }
                match ret_types.into() {
                    ZeroOneMore::Zero => Err(Error::EmptyMatch),
                    ZeroOneMore::One(t) => Ok(t),
                    ZeroOneMore::More(t) => Err(Error::IncompatibleMatchArms(t)),
                }
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
                    return Err(Error::IfConditionWasNotBool(cond_type));
                }
                let x_type = x.infer(c)?;
                let y_type = y.infer(c)?;
                if x_type != y_type {
                    return Err(Error::IncompatibleIfArms(x_type, y_type));
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
                    let mut found = false;
                    if let Some(t) = module.vars.get(item_name) {
                        c.vars.insert(item_name.clone(), t.clone());
                        found = true;
                    }
                    if let Some(l) = module.labels.get(item_name) {
                        c.labels.insert(item_name.clone(), l.clone());
                        found = true;
                    }
                    if !found {
                        return Err(Error::NoModuleItem {
                            module: module_name.clone(),
                            item: item_name.clone(),
                        });
                    };
                }
            }
            ast::Statement::Label { name, parameters  } => {
                let l = Label::new(LabelInfo {
                    name: name.clone(),
                    params: parameters.clone(),
                });
                c.labels.insert(name.clone(), l.clone());
                let param_types = parameters.iter()
                    .map(|_| Type::Int) // TODO
                    .collect::<Vec<_>>();
                // Make the function that should be added to the context
                let return_type = Type::Labeled(l);
                c.vars.insert(name.clone(), .clone());
            }
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

enum ZeroOneMore<T> {
    Zero,
    One(T),
    More(Vec<T>),
}

impl<T> From<Vec<T>> for ZeroOneMore<T> {
    fn from(mut v: Vec<T>) -> Self {
        match v.len() {
            0 => Self::Zero,
            1 => Self::One(v.pop().unwrap()),
            2.. => Self::More(v),
        }
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
