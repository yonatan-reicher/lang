use std::rc::Rc;

use derive_more::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub module_decl: Option<ModuleDecl>,
    pub statements: Vec<Statement>,
    pub return_expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDecl {
    pub name: String,
    // TODO: Rename to exporting?
    pub exports: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// `x = 42;`
    Assignment(String, Expr),
    /// `import module_name;` or `import module_name exposing (foo, bar);`
    Import {
        module_name: String,
        imports: Vec<String>,
    },
    /// `print x;`
    Print(Expr),
    /// ```lang
    /// label Cons head tail;
    /// ```
    Label {
        name: String,
        parameters: Vec<String>,
    },
}

// TODO: Use reference counting for better cloning?
#[derive(Clone, Debug, derive_more::From, PartialEq)]
pub enum Expr {
    #[from]
    Bool(bool),
    #[from]
    Int(i64),
    #[from(Rc<str>, String, &str)]
    Str(Rc<str>),
    Var(String),
    #[from]
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    /// An expression that introduces a function: `x => x + 1`
    Func(String, Box<Expr>),
    /// A function application: `f(42)`
    /// Note: The vector (should) always contain at least two elements.
    /// The first element is the function being applied.
    #[from]
    App(Vec<Expr>),
    Match(Box<Expr>, Vec<MatchArm>),
    /// An expression preceded by zero or more statements. The names introduced
    /// by the statements are not available outside the expression.
    Statements(Vec<Statement>, Box<Expr>),
    /// if cond then x else y
    If(Box<(Expr, Expr, Expr)>),
}

#[derive(Clone, Debug, derive_more::From, PartialEq)]
pub struct MatchArm(pub Pattern, pub Expr);

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Label {
        name: String,
        parameter_patterns: Vec<Pattern>,
    },
    Var(String),
    Wildcard,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
pub enum BinOp {
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("*")]
    Mul,
    #[display("/")]
    Div,
    #[display("=")]
    Eq,
    #[display("&&")]
    And,
}
