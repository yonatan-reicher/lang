use derive_more::{Debug, Deref, Display, From};
use std::rc::Rc;

fn display_applied(name: impl std::fmt::Display, args: &[impl std::fmt::Display]) -> String {
    if args.is_empty() { return format!("{name}"); }
    let arg_strings: Vec<String> = args.iter().map(|x| x.to_string()).collect();
    format!("({name} {})", arg_strings.join(" "))
}

#[derive(Clone, Debug, Display, From, PartialEq)]
pub enum Value {
    /// ()
    #[from]
    Unit,
    /// true, false
    #[from]
    Bool(bool),
    /// 123
    #[from]
    Int(i64),
    /// "Hello handsome"
    #[from(Rc<str>, &str, String)]
    Str(Rc<str>),
    /// print, (x => x + 1)
    #[from(Rc<Func> /* `Func` handled by a different impl */)]
    Func(Rc<Func>),
    /// Hello x y     (here `Hello` is the label)
    #[from(Rc<Labeled>, Labeled)]
    Labeled(Rc<Labeled>),
}

#[derive(Debug, Display, PartialEq)]
#[display("{}", display_applied(label, args))]
pub struct Labeled {
    pub label: PLabel,
    // I wanted this to be `args: [Value]`, but currently it's too hard to
    // construct unsized structs (yes, even in an `Rc`).
    pub args: Vec<Value>,
}

#[derive(Clone, Debug, Display, From, PartialEq)]
pub enum Func {
    #[from]
    Lambda(LambdaFunc),
    #[from(BuiltinFunc, BuiltinDefinition)]
    Builtin(BuiltinFunc),
    #[from(LabelFunc, Label)]
    Label(LabelFunc),
}

impl<T> From<T> for Value
where T: Into<Func> {
    fn from(value: T) -> Self {
        Value::Func(Rc::new(value.into()))
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
#[display("({param_name} => ...)")]
pub struct LambdaFunc {
    pub param_name: String,
    pub closure: crate::context::Context,
    pub body: crate::ast::Expr,
}

#[derive(Clone, Debug, Display)]
#[display("{}", display_applied(definition, applied_already))]
pub struct BuiltinFunc {
    pub applied_already: Vec<Value>,
    pub definition: Rc<BuiltinDefinition>,
}
impl<T: Into<Rc<BuiltinDefinition>>> From<T> for BuiltinFunc {
    fn from(value: T) -> Self {
        BuiltinFunc {
            applied_already: vec![],
            definition: value.into(),
        }
    }
}
impl PartialEq for BuiltinFunc {
    fn eq(&self, other: &Self) -> bool {
        self.applied_already == other.applied_already
            && std::ptr::eq(&self.definition, &other.definition)
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
#[display("{}", display_applied(label, applied_already))]
pub struct LabelFunc {
    pub label: PLabel,
    pub applied_already: Vec<Value>,
}
impl<L: Into<PLabel>> From<L> for LabelFunc {
    fn from(other: L) -> Self {
        LabelFunc { label: other.into(), applied_already: vec![] }
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display("{name}")]
pub struct Label {
    pub name: String,
    pub parameters: Vec<String>,
}

#[derive(Clone, Debug, Deref, Display, From)]
#[display("{inner}")]
#[from(Rc<Label>, Label)]
pub struct PLabel {
    inner: Rc<Label>,
}

impl PartialEq for PLabel {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.inner.as_ref(), other.inner.as_ref())
    }
}

#[derive(Clone, Debug, Display)]
#[display("{name}")]
pub struct BuiltinDefinition {
    pub name: String,
    pub arity: u8,
    #[debug("{func:p}")]
    #[allow(clippy::type_complexity)]
    pub func: Rc<dyn Fn(&[Value]) -> crate::eval::Result<Value>>,
}


// Some helpers


impl Value {
    pub const fn is_unit(&self) -> bool {
        matches!(self, Value::Unit)
    }

    /// Takes a labeled value, or a label function or closure, and returns it's
    /// label and the arguments that have been applied to it.
    pub fn unlabel(&self) -> Option<(&PLabel, &[Value])> {
        use crate::value_ref::ValueRef;
        match self.as_ref() {
            ValueRef::Labeled(Labeled { label, args }) => Some((label, args)),
            ValueRef::Func(Func::Label(LabelFunc {
                label,
                applied_already,
            })) => Some((label, applied_already)),
            _ => None,
        }
    }
}
