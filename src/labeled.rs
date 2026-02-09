//! Labeled values are values that have an assigned 'label' which is a branding
//! constructor. The constructors have names, but they are not equivalent just
//! because they might have the same name. They are equivalent only by memory
//! address, which makes them more useful in a language that supports shadowing.
//!
//! A labeled value may have arguments applied to it, specified by it's label.

use crate::value::Value;
use derive_more::{Display, From};
use std::rc::Rc;

#[derive(Clone, Debug, Display, From, PartialEq)]
#[display("{}", self.display())]
pub struct Labeled(Rc<(Label, Vec<Value>)>);

impl Labeled {
    pub fn new(label: Label, args: Vec<Value>) -> Self {
        assert_eq!(label.params.len(), args.len());
        Self(Rc::new((label, args)))
    }

    pub fn new_no_args(label: Label) -> Self {
        Self::new(label, vec![])
    }

    pub fn label(&self) -> &Label {
        &self.0.0
    }

    pub fn name(&self) -> &str {
        &self.0.0.name
    }

    pub fn args(&self) -> &[Value] {
        &self.0.1
    }

    pub fn display(&self) -> String {
        match self.args() {
            [] => self.name().to_owned(),
            _ => {
                let name = self.name();
                let arg_strings = self
                    .args()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>();
                let args = arg_strings.join(" ");
                format!("({name} {args})")
            }
        }
    }
}

#[derive(Clone, Debug, Display, Eq)]
#[display("{}", self.name)]
pub struct Label {
    inner: Rc<LabelInfo>,
}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        // Equality for Self is by address of the field!
        std::ptr::eq(self.inner.as_ref(), other.inner.as_ref())
    }
}

impl std::ops::Deref for Label {
    type Target = LabelInfo;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl Label {
    pub fn new(i: LabelInfo) -> Self {
        Self { inner: i.into() }
    }
}

#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display("{name} of {params:?}")]
pub struct LabelInfo {
    pub name: Rc<str>,
    pub params: Vec<Rc<str>>,
}
