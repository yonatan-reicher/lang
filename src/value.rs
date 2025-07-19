use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Str(String),
    Func(Func),
    Constructed(PConstructor, Vec<Value>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Func {
    Lambda(LambdaFunc),
    Builtin(BuiltinFunc),
    Constructor(ConstructorFunc),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaFunc {
    pub param_name: String,
    pub closure: Rc<crate::context::Context>,
    pub body: Rc<crate::ast::Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltinFunc {
    pub applied_already: Vec<Value>,
    pub definition: Rc<BuiltinDefinition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorFunc {
    pub applied_already: Vec<Value>,
    pub constructor: PConstructor,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub name: String,
    pub constructors: Vec<Constructor>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub name: String,
    pub parameters: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PConstructor {
    pub r#type: Rc<Type>,
    ptr: *const Constructor,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BuiltinDefinition {
    pub name: String,
    pub arity: u8,
    pub func: fn(&[Value]) -> Value,
}

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

impl ConstructorFunc {
    pub fn new(r#type: Rc<Type>, constructor_index: usize) -> Self {
        Self {
            constructor: PConstructor::new(r#type, constructor_index),
            applied_already: vec![],
        }
    }
}

impl PConstructor {
    pub fn new(r#type: Rc<Type>, index: usize) -> Self {
        let ptr = &r#type.constructors[index] as _;
        Self { r#type, ptr }
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
