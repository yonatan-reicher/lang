//! This module is responsible for parsing from source code to AST.

// TODO: Report token positions and error positions

use std::rc::Rc;

// use from this crate
use crate::ast::{BinOp, Expr, MatchArm, ModuleDecl, Pattern, Program, Statement};
use crate::lex::{EofFail, Error as LexError, Token, token, token_ident};
use crate::position::Position;
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
    #[error("{0}")]
    UnregocnizedToken(LexError),
    // atom: ( expr )
    #[error("Expected an expression or a statement after '('. No expression because: {0}")]
    NoExprNorStatementAfterLParen(ExprFailure),
    #[error(
        "Expected an expression at the end of this block (block started at {start}, expected expression at {expected_at})"
    )]
    ExpectedAnExpressionAtTheEndOfThisBlock {
        start: Position,
        expected_at: Position,
        no_expression_because: ExprFailure,
    },
    #[error(
        "The '(' at {l_paren_pos} was not closed, expected it to be closed at {expected_r_pos_at}"
    )]
    MissingRParen {
        l_paren_pos: Position,
        expected_r_pos_at: Position,
    },
    // binop expr
    #[error("Missing expression after binary operator")]
    MissingExprAfterBinOp(Position, ExprFailure),
    #[error("Print statement expects a single argument (print x)")]
    PrintStatementExpectsSingleArgument(Position),
    #[error("Assignment statement expected an expression")]
    AssignmentStatementExpectsExpression,
    #[error("Did not find function body at {0}")]
    NoFunctionBody(Position),
    #[error("Expected to a see a name after `module` keyword at {0}")]
    ExpectedModuleName(Position),
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
    #[error("there should be an expression after the match keyword")]
    NoExprAfterMatchKeyword(ExprFailure),
    #[error("should have a pattern after the '|' symbol")]
    MatchArmExpectedPatternAfterPipe,
    #[error("should have a '=>' symbol after the pattern in a match arm")]
    MatchArmExpectedArrowAfterPattern,
    #[error("should have an expression after the '=>' symbol")]
    MatchArmExpectedExprAfterArrow(ExprFailure),
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
        #[derive(Clone, Debug, Error, From, PartialEq, Eq)]
        $($token)*
    };
    ( /* Should be an item */ $($token:tt)* ) => {
        #[derive(Clone, Debug, Default, Error, From, PartialEq, Eq)]
        $($token)*
    };
}

fn default<T: Default>() -> T {
    T::default()
}

derive_all![
    #[error("does not begin with '('")]
    struct NoLParen;
];
derive_all![
    #[error("does not begin with a number")]
    struct NoNumber;
];
derive_all![
    #[error("does not begin with an identifier")]
    struct NoIdent;
];
derive_all![
    #[error("{0}, {1}, {2}")]
    struct NoAtom(NoLParen, NoNumber, NoIdent);
];

fn position<'a, E: 'a>() -> Parser<'a, Position, E> {
    Parser::state().map(|s| Position::new(s.text, s.pos.offset))
}

fn token_eq<'a, F: 'a + Clone + Default>(t: Token) -> Parser<'a, (), F> {
    crate::lex::token_eq(t).map_err(Error::from)
}

fn atom<'a>() -> Parser<'a, Expr, NoAtom> {
    position().and_then(|start| {
        token()
            .or_fail(default())
            .map_err(Error::from)
            .and_then(move |token| match token {
                Token::LParen => block()
                    .and_then_fail(|f| Parser::err(Error::NoExprNorStatementAfterLParen(f)))
                    .and_then(move |block| {
                        token_eq::<()>(Token::RParen)
                            .map_err(Error::from)
                            .map(move |_| block.clone())
                            .and_then_fail(move |()| {
                                position().and_then(move |pos| {
                                    Parser::err(Error::MissingRParen {
                                        l_paren_pos: start,
                                        expected_r_pos_at: pos,
                                    })
                                })
                            })
                    })
                    .and_then(move |block| {
                        let block = block.clone();
                        position().and_then(move |end| {
                            let statements = block.statements.clone();
                            let expr = match block.return_expr.clone() {
                                Ok(e) => e,
                                Err(no_expression_because) => {
                                    return Parser::err(
                                        Error::ExpectedAnExpressionAtTheEndOfThisBlock {
                                            start,
                                            expected_at: end,
                                            no_expression_because,
                                        },
                                    );
                                }
                            };
                            Parser::ret(if !statements.is_empty() {
                                Expr::Statements(statements, expr.into())
                            } else {
                                expr
                            })
                        })
                    }),
                Token::Number(n) => Parser::ret(Expr::Int(n)),
                Token::Ident(ident) => Parser::ret(Expr::Var(ident.as_ref().clone())),
                Token::String(s) => Parser::ret(Expr::Str(s.clone())),
                _ => Parser::fail(NoAtom::default()),
            })
    })
}

derive_all![
    #[error("does not begin with `<ident> =>`")]
    struct NoFunction;
];

fn function_expr<'a>() -> Parser<'a, Expr, NoFunction> {
    token_ident().map_err(Error::from).and_then(|param_name| {
        token_eq(Token::FatArrow)
            .map_err(Error::from)
            .and_then(move |()| {
                let param_name = param_name.clone();
                expr()
                    .and_then_fail(|_| {
                        position().and_then(|pos| Parser::err(Error::NoFunctionBody(pos)))
                    })
                    .map(move |body| {
                        let name = param_name.clone();
                        let body = body.clone();
                        Expr::Func(name.as_ref().into(), body.into())
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

fn binary_op_map() -> &'static [(Token, BinOp)] {
    &[
        (Token::Plus, BinOp::Add),
        (Token::Minus, BinOp::Sub),
        (Token::Star, BinOp::Mul),
        (Token::Slash, BinOp::Div),
        (Token::Eq, BinOp::Eq),
    ]
}

fn binops() -> String {
    binary_op_map()
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
    token()
        .map_fail(From::from)
        .map_err(Error::from)
        .and_then(|token| {
            for (t, op) in binary_op_map() {
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
                .and_then_fail(|f| {
                    position()
                        .and_then(move |p| Parser::err(Error::MissingExprAfterBinOp(p, f.clone())))
                })
                .map(move |right| Expr::BinOp(Box::new(left.clone()), op, Box::new(right)))
        })
    })
}

derive_all![
    #[error("does not begin with the '|' symbol")]
    pub struct NoPipe;
];

derive_all![
    #[error("does not begin with the 'match' keyword")]
    pub struct NoMatchKeyword;
];

fn match_arm<'a>() -> Parser<'a, MatchArm, NoPipe> {
    // |
    token_eq(Token::Pipe).map_err(From::from).and_then(|()| {
        // | Cons head tail
        pattern()
            .and_then_fail(|_| Parser::err(Error::MatchArmExpectedPatternAfterPipe))
            .and_then(|p| {
                // | Cons head tail =>
                token_eq::<()>(Token::FatArrow)
                    .map_err(Error::from)
                    .or_err(Error::MatchArmExpectedArrowAfterPattern)
                    .and_then(move |()| {
                        let p = p.clone();
                        // | Cons head tail => head + sum tail
                        expr()
                            .and_then_fail(|f| {
                                Parser::err(Error::MatchArmExpectedExprAfterArrow(f))
                            })
                            .map(move |e| MatchArm(p.clone(), e))
                    })
            })
    })
}

fn match_expr<'a>() -> Parser<'a, Expr, NoMatchKeyword> {
    token_eq(Token::Match).map_err(From::from).and_then(|()| {
        expr()
            .and_then_fail(|f| Parser::err(Error::NoExprAfterMatchKeyword(f)))
            .and_then(|e| {
                match_arm()
                    .repeat_0()
                    .map(move |arms| Expr::Match(Box::new(e.clone()), arms))
            })
    })
}

derive_all![
    #[error("should be an `if` keyword at the start of an If expression.")]
    struct NoIfKeyword;
];

fn if_expr<'a>() -> Parser<'a, Expr, NoIfKeyword> {
    token_eq(Token::If).and_then(move |()| {
        expr().and_then_fail(|_| todo!()).and_then(move |cond| {
            let cond = Rc::new(cond);
            token_eq(Token::Then)
                .and_then_fail(|()| todo!())
                .and_then(move |()| {
                let cond = cond.clone();
                expr().and_then_fail(|_| todo!()).and_then(move |x| {
                    let cond = cond.clone();
                    let x = Rc::new(x);
                    token_eq(Token::Else)
                        .and_then_fail(|()| todo!())
                        .and_then(move |()| {
                        let cond = cond.clone();
                        let x = x.clone();
                        expr().and_then_fail(|_| todo!()).map(move |y| {
                            Expr::If(Box::new((cond.as_ref().clone(), x.as_ref().clone(), y)))
                        })
                    })
                })
            })
        })
    })
}

// TODO
derive_all![ - Default
    #[allow(private_interfaces)]
    pub enum ExprFailure {
        #[error("{0}")]
        NoFunction(NoFunction),
        #[error("{0}")]
        NoAtom(NoAtom),
        #[error("{0}")]
        NoBinOpExpr(NoBinOpExpr),
        #[error("{0}")]
        NoMatchKeyword(NoMatchKeyword),
        #[error("{0}")]
        NoIfKeyword(NoIfKeyword),
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
        if_expr().map_fail(ExprFailure::from),
        match_expr().map_fail(ExprFailure::from),
        function_expr().map_fail(ExprFailure::from),
        binop_expr().map_fail(ExprFailure::from),
        application_expr_or_atom().map_fail(ExprFailure::from),
    ]
}

/// A pattern atom is either a '_', a name, or a parenthesised pattern.
fn pattern_atom<'a>() -> Parser<'a, Pattern, EofFail> {
    one_of![
        token_eq::<()>(Token::Underscore)
            .map_err(From::from)
            .map(|()| Pattern::Wildcard),
        token_ident().map_err(From::from).map(|name| {
            let is_label = name.chars().next().is_some_and(char::is_uppercase);
            let name = name.as_ref().clone();
            if is_label {
                Pattern::Label {
                    name,
                    parameter_patterns: vec![],
                }
            } else {
                Pattern::Var(name)
            }
        }),
        token_eq(Token::LParen).map_err(From::from).and_then(|()| {
            pattern().and_then_fail(|_f| todo!()).and_then(|p| {
                token_eq(Token::RParen)
                    .map_err(From::from)
                    .and_then_fail(|()| todo!())
                    .map(move |()| p.clone())
            })
        })
    ]
    .map_fail(|()| EofFail)
}

fn pattern<'a>() -> Parser<'a, Pattern, EofFail> {
    one_of![
        token_ident().map_err(Error::from).and_then(|name| {
            pattern_atom()
                .repeat_1()
                .map(move |parameter_patterns| Pattern::Label {
                    name: name.as_ref().clone(),
                    parameter_patterns,
                })
        }),
        pattern_atom(),
    ]
    .map_fail(|()| EofFail)
}

fn print_statement<'a>() -> Parser<'a, Statement> {
    token_eq(Token::Print).map_err(Error::from).and_then(|()| {
        atom().map(Statement::Print).and_then_fail(|_| {
            position().and_then(|p| Parser::err(Error::PrintStatementExpectsSingleArgument(p)))
        })
    })
}

fn assignment_statement<'a>() -> Parser<'a, Statement> {
    token_ident().map_err(Error::from).and_then(|ident| {
        token_eq(Token::Eq).map_err(Error::from).and_then(move |_| {
            let ident = ident.clone();
            expr()
                .or_err(Error::AssignmentStatementExpectsExpression)
                .map(move |ex| Statement::Assignment(ident.as_ref().clone(), ex))
        })
    })
}

/// Parses a list of names in parenthesis like `(hello cruel world)`.
/// In case of wrong syntax, doesn't fail - errors instead.
fn parenthesis_name_list<'a>(
    on_missing_left_paren: fn() -> Error,
    on_missing_right_paren: fn() -> Error,
) -> Parser<'a, Vec<Rc<String>>> {
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

fn import_exposing<'a>() -> Parser<'a, Vec<Rc<String>>> {
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
                        module_name: module_name.as_ref().clone(),
                        imports: exposing.into_iter().map(|x| x.to_string()).collect(),
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
                    name: name.as_ref().clone(),
                    parameters: parameters
                        .into_iter()
                        .map(|x| x.as_ref().clone())
                        .clone()
                        .collect(),
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

#[derive(Clone, Debug, From, PartialEq)]
struct Block {
    statements: Vec<Statement>,
    return_expr: Result<Expr, ExprFailure>,
}

/// Parse a block - a series of statements, maybe followed by a returned
/// expression. Does not parse any braces or delimiters around.
fn block<'a, F: 'a>() -> Parser<'a, Block, F> {
    statement()
        // A block starting with a statement expects the rest of the block - to
        // be just another block! We can just recurse.
        .and_then(|statement| {
            block().map(move |mut block| {
                block.statements.insert(0, statement.clone());
                block
            })
        })
        // No statement - may or may not have an expression.
        .and_then_fail(|_| {
            expr()
                .map(Ok)
                .and_then_fail(|f| Parser::ret(Err(f)))
                .map(|return_expr| Block {
                    statements: vec![],
                    return_expr,
                })
        })
}

fn module_exporting<'a>() -> Parser<'a, Vec<Rc<String>>> {
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
            .and_then_fail(|()| {
                position().and_then(|pos| Parser::err(Error::ExpectedModuleName(pos)))
            })
            .and_then(|name| {
                module_exporting().maybe().map(move |exports| ModuleDecl {
                    name: name.as_ref().clone(),
                    exports: exports
                        .unwrap_or_default()
                        .into_iter()
                        .map(|x| x.as_ref().clone())
                        .collect(),
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
                return_expr: block.return_expr.ok(),
            })
            .or_err(err)
    })
}

pub fn parse<'a>(text: &'a str) -> Result<Program, Error> {
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

    #[test]
    fn parse_binary_operator_expr() {
        // As you can see, binary operators are (for now) parsed precedence-less
        assert_eq!(
            parse(indoc! {r"
                1 + 2 * 3 + 1
            "}),
            Ok(Program {
                module_decl: None,
                statements: vec![],
                return_expr: Some(Expr::BinOp(
                    Box::new(1.into()),
                    BinOp::Add,
                    Box::new(Expr::BinOp(
                        Box::new(2.into()),
                        BinOp::Mul,
                        Box::new(Expr::BinOp(
                            Box::new(3.into()),
                            BinOp::Add,
                            Box::new(1.into()),
                        )),
                    )),
                ),),
            }),
        );
    }
}
