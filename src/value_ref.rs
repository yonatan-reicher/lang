use crate::labeled::Labeled;
use crate::value::{Func, Value};


/// The result of taking a value's fields as references. This type should be
/// used just for pattern matching.
pub enum ValueRef<'a> {
    Unit,
    Bool(bool),
    Int(i64),
    Str(&'a str),
    Func(&'a Func),
    Labeled(&'a Labeled),
}


impl Value {
    pub fn as_ref<'a>(&'a self) -> ValueRef<'a> {
        use Value as V;
        use ValueRef as R;
        match self {
            V::Unit => R::Unit,
            V::Bool(a) => R::Bool(*a),
            V::Int(a) => R::Int(*a),
            V::Str(a) => R::Str(a),
            V::Func(a) => R::Func(a),
            V::Labeled(a) => R::Labeled(a),
        }
    }
}
