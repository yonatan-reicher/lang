use derive_more::{Display, Debug, From};
use std::rc::Rc;
use thiserror::Error;

fn display_applied(name: impl std::fmt::Display, args: &[impl std::fmt::Display]) -> String {
    let arg_strings: Vec<String> = args.iter().map(|x| x.to_string()).collect();
    format!("({name} {})", arg_strings.join(" "))
}

#[derive(Clone, Debug, Display, From, PartialEq)]
pub enum Value {
    #[from]
    Unit,
    #[from]
    Bool(bool),
    #[from]
    Int(i64),
    #[from]
    Str(String),
    #[from(Func, LambdaFunc, BuiltinFunc, ConstructorFunc, BuiltinDefinition)]
    Func(Func),
    #[display("{}", display_applied(_0, _1))]
    Constructed(PConstructor, Vec<Value>),
}

#[derive(Clone, Debug, Display, From, PartialEq)]
pub enum Func {
    #[from]
    Lambda(LambdaFunc),
    #[from(BuiltinFunc, BuiltinDefinition)]
    Builtin(BuiltinFunc),
    #[from]
    Constructor(ConstructorFunc),
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
        self.applied_already == other.applied_already && std::ptr::eq(&self.definition, &other.definition)
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
#[display("{}", display_applied(constructor, applied_already))]
pub struct ConstructorFunc {
    pub applied_already: Vec<Value>,
    pub constructor: PConstructor,
}

#[derive(Clone, Debug, Display, PartialEq)]
#[display("{name}")]
pub struct Type {
    pub name: String,
    pub constructors: Vec<Constructor>,
}

#[derive(Clone, Debug, Display, PartialEq)]
#[display("{name}")]
pub struct Constructor {
    pub name: String,
    pub parameters: Vec<String>,
}

#[derive(Clone, Debug, Display)]
#[display("{}", self.deref())]
pub struct PConstructor {
    pub r#type: Rc<Type>,
    ptr: *const Constructor,
}

impl PConstructor {
    fn deref(&self) -> &Constructor {
        // Safety:
        // This dereference is fine as long as this object was constructed with
        // the appropriate `new` function
        unsafe { &*self.ptr }
    }
}

impl PartialEq for PConstructor {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl std::ops::Deref for PConstructor {
    type Target = Constructor;
    fn deref(&self) -> &Self::Target {
        self.deref()
    }
}

#[derive(Clone, Debug, Display)]
#[display("{name}")]
pub struct BuiltinDefinition {
    pub name: String,
    pub arity: u8,
    #[debug("{func:p}")]
    pub func: Rc<dyn Fn(&[Value]) -> Value>,
}

/*
impl PartialEq for BuiltinDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity && self.func == other.func
    }
}
*/

#[derive(Debug, Error)]
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

#[derive(Debug, Error)]
pub enum ConstructorFuncError {
    #[error(
        "constructor given to constructor function object should have parameters, but had none (constructor {constructor})"
    )]
    ConstructorIsParameterless { constructor: PConstructor },
}

impl ConstructorFunc {
    pub fn new(constructor: PConstructor) -> Result<Self, ConstructorFuncError> {
        if constructor.parameters.is_empty() {
            return Err(ConstructorFuncError::ConstructorIsParameterless { constructor });
        }
        Ok(Self {
            constructor,
            applied_already: vec![],
        })
    }
}

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
