
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Str(String),
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

impl_from!(Bool, bool);
impl_from!(Int, i64);
impl_from!(Str, String);
