#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub name: String,
    pub statement: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assignment(String, Expr),
    Print(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(String),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    /// An expression that introduces a function: `x => x + 1`
    Func(String, Box<Expr>),
    /// A function application: `f(42)`
    /// Note: The vector (should) always contain at least two elements.
    /// The first element is the function being applied.
    App(Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    And,
}
