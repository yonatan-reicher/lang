use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Str(String),
    Func(Func),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Func {
    Lambda(LambdaFunc),
    Builtin(BuiltinFunc),
}

impl Func {
    pub const fn name(&self) -> &String {
        match self {
            Func::Lambda(l) => &l.name,
            Func::Builtin(b) => &b.name,
        }
    }

    pub const fn name_mut(&mut self) -> &mut String {
        match self {
            Func::Lambda(l) => &mut l.name,
            Func::Builtin(b) => &mut b.name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaFunc {
    pub name: String,
    pub closure: Rc<crate::context::Context>,
    pub body: Rc<crate::ast::Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltinFunc {
    pub name: String,
    pub func: fn(&[Value]) -> Value,
}

macro_rules! impl_from {
    ($constructor: ident, $from: ty) => {
        impl From<$from> for Value {
            fn from(x: $from) -> Self {
                Self::$constructor(x)
            }
        }
    };
}

// Implement From conversions
impl_from!(Bool, bool);
impl_from!(Int, i64);
impl_from!(Str, String);
impl_from!(Func, Func);
