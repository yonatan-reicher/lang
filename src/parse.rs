//! This module is responsible for parsing from source code to AST.

// use from this crate
use crate::ast::{BinOp, Expr, ModuleDecl, Program, Statement};
use crate::lex::{token, token_eq, token_ident, EofFail, Error as LexError, Token};
// use libraries
use derive_more::From;
use nessie_parse::{ParseResult, one_of};
use thiserror::Error;

// This parser is built on top of Nessie Parse. Parsers can either return, fail,
// or error. The difference between failure and error is that failures are
// supposed to be recovered from.
//
// In this module, we use a single, massive, error type, but use smaller failure
// types. The failure types act as documentation as to when a different
// syntactic element should be parsed. For example, a number parser might fail
// with a failure like NotANumber, and a literal parser might fail with an enum
// that would have NotANumber as one of the cases.

/// A monolith error type for this parser.
#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("Unrecognized token: {0}")]
    UnregocnizedToken(LexError),
    // atom: ( expr )
    #[error("Expected an expression after '('. No expression because: {0}")]
    NoExprAfterLParen(ExprFailure),
    #[error("Expected a ')' here, because the expression ends here, but the ')' was not found")]
    MissingRParen,
    // binop expr
    #[error("Missing expression after binary operator")]
    MissingExprAfterBinOp(ExprFailure),
    #[error("Print statement expects a single argument (print x)")]
    PrintStatementExpectsSingleArgument,
    #[error("Assignment statement expected an expression")]
    AssignmentStatementExpectsExpression,
    #[error("Did not find function body")]
    NoFunctionBody,
    #[error("Expected to a see a name after `module` keyword")]
    ExpectedModuleName,
    #[error("Expected to a see a name after `import` keyword")]
    ExpectedModuleNameImport,
    #[error(
        "Expects a list of names in parentheses after the `exporting` keyword, like `exporting (name1 name2)`. Missing left parenthesis '('"
    )]
    ExportingMissingLParen,
    #[error(
        "Expects a list of names in parentheses after the `exporting` keyword, like `exporting (name1 name2)`. Missing right parenthesis ')'"
    )]
    ExportingMissingRParen,
    #[error("Missing semicolon ';' at the end of the statement")]
    MissingSemicolon,
    #[error("Expected a statement or an expression after a module declartion")]
    ExpectedAStatementOrExprAfterModuleDecl,
    #[error("there should be a statement, expression, or module declaration here")]
    ExpectedAStatementExprOrModuleDecl,
}

impl From<LexError> for Error {
    fn from(e: LexError) -> Self {
        Error::UnregocnizedToken(e)
    }
}

/// The parser type we use.
type Parser<'a, T, F = ()> = nessie_parse::Parser<'a, T, Error, F>;

macro_rules! derive_all {
    ( - Default /* Should be an item */ $($token:tt)* ) => {
        // TODO: Do we want to take `Default` out?
        #[derive(Clone, Debug, Error, From, PartialEq, Eq)]
        $($token)*
    };
    ( /* Should be an item */ $($token:tt)* ) => {
        // TODO: Do we want to take `Default` out?
        #[derive(Clone, Debug, Default, Error, From, PartialEq, Eq)]
        $($token)*;
    };
}

fn default<T: Default>() -> T { T::default() }

derive_all![
    #[error("does not begin with '('")]
    struct NoLParen
];
derive_all![
    #[error("does not begin with a number")]
    struct NoNumber
];
derive_all![
    #[error("does not begin with an identifier")]
    struct NoIdent
];
derive_all![
    #[error("{0}, {1}, {2}")]
    struct NoAtom(NoLParen, NoNumber, NoIdent)
];

fn atom<'a>() -> Parser<'a, Expr, NoAtom> {
    token().or_fail(default()).map_err(Error::from).and_then(|token| match token {
        Token::LParen => expr()
            .and_then_fail(|f| Parser::err(Error::NoExprAfterLParen(f)))
            .and_then(|expr| {
                token_eq::<()>(Token::RParen)
                    .map_err(Error::from)
                    .map(move |_| expr.clone())
                    .or_err(Error::MissingRParen)
            }),
        Token::Number(n) => Parser::ret(Expr::Int(n)),
        Token::Ident(ident) => Parser::ret(Expr::Var(ident)),
        _ => Parser::fail(NoAtom::default()),
    })
}

derive_all![
    #[error("does not begin with `<ident> =>`")]
    struct NoFunction
];

fn function_expr<'a>() -> Parser<'a, Expr, NoFunction> {
    token_ident().map_err(Error::from).and_then(|param_name| {
        token_eq(Token::FatArrow)
            .map_err(Error::from)
            .and_then(move |()| {
                let param_name = param_name.clone();
                expr().or_err(Error::NoFunctionBody).map(move |body| {
                    let name = param_name.clone();
                    let body = body.clone();
                    Expr::Func(name, body.into())
                })
            })
    })
}

fn application_expr_or_atom<'a>() -> Parser<'a, Expr, NoAtom> {
    atom().repeat_1().map(|atoms| {
        debug_assert!(!atoms.is_empty(), "repeat_1 always returns non-empty");
        if let [lonely_atom] = atoms.as_slice() {
            lonely_atom.clone()
        } else {
            Expr::App(atoms.clone())
        }
    })
}

static BINARY_OP_MAP: &[(Token, BinOp)] = &[
    (Token::Plus, BinOp::Add),
    (Token::Minus, BinOp::Sub),
    (Token::Star, BinOp::Mul),
    (Token::Slash, BinOp::Div),
    (Token::Eq, BinOp::Eq),
];

fn binops() -> String {
    BINARY_OP_MAP
        .iter()
        .map(|(_, r)| r.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

derive_all![ - Default
    #[error("not a binary operator (options are {})", binops())]
    enum NotABinOp {
        Token(Token),
        Eof(EofFail),
    }
];

fn binop<'a>() -> Parser<'a, BinOp, NotABinOp> {
    token().map_fail(From::from).map_err(Error::from).and_then(|token| {
        for (t, op) in BINARY_OP_MAP {
            if t == &token {
                return Parser::ret(*op);
            }
        }
        Parser::fail(token.into())
    })
}

derive_all![ - Default
    enum NoBinOpExpr {
        #[error("lhs is not an atom: {0}")]
        NoLhsAtom(NoAtom),
        #[error("{0}")]
        NoBinOp(NotABinOp),
    }
];

fn binop_expr<'a>() -> Parser<'a, Expr, NoBinOpExpr> {
    atom().map_fail(Into::into).and_then(|left| {
        binop().map_fail(Into::into).and_then(move |op| {
            let left = left.clone();
            expr()
                .and_then_fail(|f| Parser::err(Error::MissingExprAfterBinOp(f)))
                .map(move |right| Expr::BinOp(Box::new(left.clone()), op, Box::new(right)))
        })
    })
}

// TODO
derive_all![ - Default
    pub enum ExprFailure {
        #[error("{0}")]
        NoFunction(NoFunction),
        #[error("{0}")]
        NoAtom(NoAtom),
        #[error("{0}")]
        NoBinOpExpr(NoBinOpExpr),
        #[error("todo")] // TODO:
        Many(Vec<ExprFailure>),
    }
];

impl nessie_parse::CombineManyFail<'_, ExprFailure> for ExprFailure {
    fn combine_many_fail(fails: Vec<(Self, nessie_parse::State<'_>)>) -> Self {
        Self::Many(ExprFailure::combine_many_fail(fails))
    }
}

fn expr<'a>() -> Parser<'a, Expr, ExprFailure> {
    one_of![
        function_expr().map_fail(ExprFailure::from),
        binop_expr().map_fail(ExprFailure::from),
        application_expr_or_atom().map_fail(ExprFailure::from),
    ]
}

fn print_statement<'a>() -> Parser<'a, Statement> {
    token_eq(Token::Print).map_err(Error::from).and_then(|()| {
        atom()
            .map(Statement::Print)
            .or_err(Error::PrintStatementExpectsSingleArgument)
    })
}

fn assignment_statement<'a>() -> Parser<'a, Statement> {
    token_ident().map_err(Error::from).and_then(|ident| {
        token_eq(Token::Eq).map_err(Error::from).and_then(move |_| {
            let ident = ident.clone();
            expr()
                .or_err(Error::AssignmentStatementExpectsExpression)
                .map(move |ex| Statement::Assignment(ident.clone(), ex))
        })
    })
}

/// Parses a list of names in parenthesis like `(hello cruel world)`.
/// In case of wrong syntax, doesn't fail - errors instead.
fn parenthesis_name_list<'a>(
    on_missing_left_paren: fn() -> Error,
    on_missing_right_paren: fn() -> Error,
) -> Parser<'a, Vec<String>> {
    // (
    token_eq::<()>(Token::LParen)
        .map_err(Error::from)
        .or_err(on_missing_left_paren())
        .and_then(move |()| {
            // exporting ( f g h
            token_ident::<()>()
                .map_err(Error::from)
                .repeat_0()
                .and_then(move |names| {
                    // exporting ( f g h )
                    token_eq::<()>(Token::RParen)
                        .map_err(Error::from)
                        .or_err(on_missing_right_paren())
                        .map(move |()| names.clone())
                })
        })
}

fn import_exposing<'a>() -> Parser<'a, Vec<String>> {
    token_eq(Token::Exposing)
        .map_err(Error::from)
        .and_then(|()| {
            parenthesis_name_list(
                || Error::ExportingMissingLParen,
                || Error::ExportingMissingRParen,
            )
        })
}

fn import_statement<'a>() -> Parser<'a, Statement> {
    token_eq(Token::Import).map_err(Error::from).and_then(|()| {
        token_ident::<()>()
            .map_err(Error::from)
            .or_err(Error::ExpectedModuleNameImport)
            .and_then(|module_name| {
                import_exposing().maybe().map(move |exposing| {
                    let exposing = exposing.unwrap_or_default();
                    Statement::Import {
                        module_name: module_name.clone(),
                        imports: exposing,
                    }
                })
            })
    })
}

fn label_statement<'a>() -> Parser<'a, Statement> {
    // label
    token_eq(Token::Label).map_err(Error::from).and_then(|()| {
        // label Cons
        // token_ident().or_err(todo!()).and_then(|name| {
        token_ident().map_err(Error::from).and_then(|name| {
            let name = name.clone();
            // label Cons head tail
            token_ident::<()>()
                .map_err(Error::from)
                .repeat_0()
                .map(move |parameters| Statement::Label {
                    name: name.clone(),
                    parameters: parameters.clone(),
                })
        })
    })
}

/// This parser never fails, always returns or errs.
struct NotAStatement;
fn statement<'a>() -> Parser<'a, Statement, NotAStatement> {
    one_of![
        print_statement(),
        import_statement(),
        label_statement(),
        assignment_statement(),
    ]
    .map_fail(|()| NotAStatement)
    .and_then(|s| {
        // TODO: Make token_eq and all token* functions return a more generic type using Into, so
        // we can get rid of all these .map_err
        token_eq::<()>(Token::Semicolon)
            .map_err(Error::from)
            .or_err(Error::MissingSemicolon)
            .map(move |_| s.clone())
    })
}

#[derive(Clone, Debug, Default, From, PartialEq)]
struct Block {
    statements: Vec<Statement>,
    return_expr: Option<Expr>,
}

/// Parse a block - a series of statements, maybe followed by a returned
/// expression. Does not parse any braces or delimiters around.
fn block<'a, F: 'a>() -> Parser<'a, Block, F> {
    one_of![
        statement().map_fail(|_| ()).and_then(|statement| {
            let statement_clone = statement.clone();
            block::<()>()
                .map(move |mut block| {
                    block.statements.insert(0, statement.clone());
                    block
                })
                .or_ret(Block {
                    statements: vec![statement_clone.clone()],
                    return_expr: None,
                })
        }),
        expr()
            .map(|e| Block {
                statements: vec![],
                return_expr: Some(e),
            })
            .or_fail(()),
    ]
    .map_fail(|()| ()) // Annotate the failure type
    .or_ret(Block::default())
}

fn module_exporting<'a>() -> Parser<'a, Vec<String>> {
    token_eq(Token::Exporting)
        .map_err(Error::from)
        .and_then(|()| {
            parenthesis_name_list(
                || Error::ExportingMissingLParen,
                || Error::ExportingMissingRParen,
            )
        })
}

fn module_declaration<'a>() -> Parser<'a, ModuleDecl> {
    token_eq(Token::Module).map_err(Error::from).and_then(|()| {
        token_ident::<()>()
            .map_err(Error::from)
            .or_err(Error::ExpectedModuleName)
            .and_then(|name| {
                module_exporting().maybe().map(move |exports| ModuleDecl {
                    name: name.clone(),
                    exports: exports.unwrap_or_default(),
                })
            })
    })
}

fn program<'a>() -> Parser<'a, Program> {
    module_declaration().maybe().and_then(|module_decl| {
        let err = if module_decl.is_some() {
            Error::ExpectedAStatementOrExprAfterModuleDecl
        } else {
            Error::ExpectedAStatementExprOrModuleDecl
        };
        block::<()>()
            .map(move |block| Program {
                module_decl: module_decl.clone(),
                statements: block.statements,
                return_expr: block.return_expr,
            })
            .or_err(err)
    })
}

pub fn parse(text: &str) -> Result<Program, Error> {
    let state = text.into();
    match program().parse(state) {
        ParseResult::Ok(prog, _) => Ok(prog),
        ParseResult::Fail((), _) => {
            unreachable!(
                "The main program parser should always either succeed or err, never soft fail."
            );
        }
        ParseResult::Err(err, _) => Err(err),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use nessie_parse::{ParseResult, Pos};

    #[test]
    fn number_atom() {
        let res = atom().parse(
            indoc! {r"
                3
            "}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Expr::Int(3),
                Pos {
                    offset: 1,
                    row: 1,
                    col: 2,
                },
            ),
        );
    }

    #[test]
    fn test_expr() {
        let res = expr().parse(
            indoc! {r"
                (1 + 2) + 3
            "}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Expr::BinOp(
                    Box::new(Expr::BinOp(
                        Box::new(Expr::Int(1)),
                        BinOp::Add,
                        Box::new(Expr::Int(2)),
                    )),
                    BinOp::Add,
                    Box::new(Expr::Int(3)),
                ),
                Pos {
                    row: 1,
                    col: 12,
                    offset: 11,
                },
            ),
        );
    }

    #[test]
    fn empty_module() {
        assert_eq!(
            parse("module foo"),
            Ok(Program {
                module_decl: Some(ModuleDecl {
                    name: "foo".to_string(),
                    exports: vec![],
                }),
                statements: vec![],
                return_expr: None,
            })
        );
    }

    #[test]
    fn empty_module_empty_exporting() {
        assert_eq!(
            parse(indoc! {r"
                module foo exporting ()
            "}),
            Ok(Program {
                module_decl: Some(ModuleDecl {
                    name: "foo".to_string(),
                    exports: vec![],
                }),
                statements: vec![],
                return_expr: None,
            }),
        );
    }

    #[test]
    fn module_with_exporting() {
        assert_eq!(
            parse(indoc! {r"
                module modmod exporting (
                    hello friend
                )
                hello = 2;
                print hello;
                world = 1 + 2;
                friend = (1 + 1) + 1;
            "}),
            Ok(Program {
                module_decl: Some(ModuleDecl {
                    name: "modmod".to_string(),
                    exports: vec!["hello".into(), "friend".into()],
                }),
                statements: vec![
                    Statement::Assignment("hello".to_string(), Expr::Int(2)),
                    Statement::Print(Expr::Var("hello".to_string())),
                    Statement::Assignment(
                        "world".to_string(),
                        Expr::BinOp(Box::new(Expr::Int(1)), BinOp::Add, Box::new(Expr::Int(2)),),
                    ),
                    Statement::Assignment(
                        "friend".to_string(),
                        Expr::BinOp(
                            Box::new(Expr::BinOp(
                                Box::new(Expr::Int(1)),
                                BinOp::Add,
                                Box::new(Expr::Int(1)),
                            )),
                            BinOp::Add,
                            Box::new(Expr::Int(1)),
                        ),
                    ),
                ],
                return_expr: None,
            }),
        );
    }

    #[test]
    fn parse_return_expr() {
        assert_eq!(
            parse(indoc! {r"
                hello = 2;
                hello
            "}),
            Ok(Program {
                module_decl: None,
                statements: vec![Statement::Assignment("hello".to_string(), Expr::Int(2))],
                return_expr: Some(Expr::Var("hello".to_string())),
            }),
        );
    }
}
