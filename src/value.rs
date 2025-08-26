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
    #[from]
    Str(String),
    /// print, (x => x + 1)
    #[from(Func, LambdaFunc, BuiltinFunc, LabelFunc, BuiltinDefinition)]
    Func(Func),
    /// Hello x y     (here `Hello` is the label)
    #[display("{}", display_applied(label, arguments))]
    Labeled { label: PLabel, arguments: Vec<Value> }
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

#[derive(Clone, Debug, Display, PartialEq)]
#[display("({param_name} => ...)")]
pub struct LambdaFunc {
    pub param_name: String,
    pub closure: Rc<crate::context::Context>,
    pub body: Rc<crate::ast::Expr>,
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

/*
impl PartialEq for BuiltinDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity && self.func == other.func
    }
}
*/

/*
macro_rules! impl_from {
    (
        // The type to convert from.
        $from: ident
        for
        // convert to.
        $to: ident
        ::
        // Name of a constructor.
        $constructor: ident
    ) => {
        impl From<$from> for $to {
            fn from(x: $from) -> Self {
                Self::$constructor(x.into())
            }
        }
    };
    (
        // The type to convert from.
        $from: ident
        for
        // convert to.
        $to: ident
        using
        $middle: ty
    ) => {
        impl From<$from> for $to {
            fn from(x: $from) -> Self {
                let y: $middle = x.into();
                y.into()
            }
        }
    };
}

// Implement From conversions
impl_from!(bool for Value::Bool);
impl_from!(i64 for Value::Int);
impl_from!(String for Value::Str);
impl_from!(Func for Value::Func);
impl_from!(LambdaFunc for Func::Lambda);
impl_from!(LambdaFunc for Value::Func);
impl_from!(BuiltinFunc for Func::Builtin);
impl_from!(BuiltinFunc for Value::Func);
impl From<Rc<BuiltinDefinition>> for BuiltinFunc {
    fn from(value: Rc<BuiltinDefinition>) -> Self {
        BuiltinFunc {
            applied_already: vec![],
            definition: value,
        }
    }
}
impl_from!(BuiltinDefinition for BuiltinFunc using Rc<BuiltinDefinition>);
impl_from!(BuiltinDefinition for Func::Builtin);
impl_from!(BuiltinDefinition for Value::Func);
impl_from!(ConstructorFunc for Func::Constructor);
impl_from!(ConstructorFunc for Value::Func);

#[derive(Debug, thiserror::Error)]
pub enum PConstructorError {
    #[error(
        "Constructor should take index of an existing constructor, but got '{index}' in type '{type}', which has only {c} constructors",
        c = r#type.constructors.len(),
    )]
    IndexIsNotValid { index: usize, r#type: Rc<Type> },
}

impl PConstructor {
    pub fn new(r#type: Rc<Type>, index: usize) -> Result<Self, PConstructorError> {
        let Some(constructor) = r#type.constructors.get(index) else {
            return Err(PConstructorError::IndexIsNotValid { index, r#type });
        };
        let ptr = constructor as _;
        Ok(Self { r#type, ptr })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ConstructorFuncError {
    #[error(
        "constructor given to constructor function object should have parameters, but had none (constructor {constructor})"
    )]
    ConstructorIsParameterless { constructor: PConstructor },
    #[error("{0}")]
    PConstructorError(#[from] PConstructorError),
}

impl ConstructorFunc {
    pub fn new(r#type: Rc<Type>, constructor_index: usize) -> Result<Self, ConstructorFuncError> {
        let this = Self {
            constructor: PConstructor::new(r#type, constructor_index),
            applied_already: vec![],
        };
        assert!(
            !this.constructor.parameters.is_empty(),
            "constructor functions should be only hold constructors that have parameters",
        );
        this
    }
}

impl std::ops::Deref for PConstructor {
    type Target = Constructor;

    fn deref(&self) -> &Self::Target {
        // Safety:
        // This dereference is fine as long as this object was constructed with
        // the appropriate `new` function
        unsafe { &*self.ptr }
    }
}

*/
