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
