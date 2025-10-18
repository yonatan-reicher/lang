use crate::labeled::{Label, Labeled};
use derive_more::{Debug, Display, From};
use std::rc::Rc;

fn display_applied(name: impl std::fmt::Display, args: &[impl std::fmt::Display]) -> String {
    if args.is_empty() {
        return format!("{name}");
    }
    let arg_strings: Vec<String> = args.iter().map(|x| x.to_string()).collect();
    format!("({name} {})", arg_strings.join(" "))
}

#[derive(Clone, Debug, Display, From, PartialEq)]
pub enum Value {
    /// ()
    #[from]
    #[display("()")]
    Unit,
    /// true, false
    #[from]
    #[display("{_0}", )]
    Bool(bool),
    /// 123
    #[from]
    Int(i64),
    /// "Hello handsome"
    #[from(Rc<str>, &str, String)]
    #[display("\"{_0}\"")]
    Str(Rc<str>),
    /// print, (x => x + 1)
    #[from(Rc<Func> /* `Func` handled by a different impl */)]
    Func(Rc<Func>),
    /// Hello x y     (here `Hello` is the label)
    #[from]
    Labeled(Labeled),
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
where
    T: Into<Func>,
{
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
    pub label: Label,
    pub applied_already: Vec<Value>,
}
impl<L: Into<Label>> From<L> for LabelFunc {
    fn from(other: L) -> Self {
        LabelFunc {
            label: other.into(),
            applied_already: vec![],
        }
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
    pub fn unlabel(&self) -> Option<(&Label, &[Value])> {
        use crate::value_ref::ValueRef;
        match self.as_ref() {
            ValueRef::Labeled(x) => Some((x.label(), x.args())),
            ValueRef::Func(Func::Label(LabelFunc {
                label,
                applied_already,
            })) => Some((label, applied_already)),
            _ => None,
        }
    }
}
