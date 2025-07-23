//! This module is responsible for parsing from source code to AST.

use crate::ast::{BinOp, Expr, ModuleDecl, Program, Statement};
use nessie_parse::{ParseResult, one_of};
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    // TODO: Make this not hard-coded, or at least made in some reasonable way.
    #[error(
        "Unrecognized token: valid tokens are numbers, identifiers, and symbols ';', ':', '=', '=>', '+', '(' and  ')'"
    )]
    UnregocnizedToken,
    #[error("Expected a ')' here, because the expression ends here, but the ')' was not found")]
    MissingParen,
    #[error("Print statement expects a single argument (print x)")]
    PrintStatementExpectsSingleArgument,
    #[error("Assignment statement expected an expression")]
    AssignmentStatementExpectsExpression,
    #[error("Bad statement")]
    BadStatement,
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
}

type Parser<'a, T, F = ()> = nessie_parse::Parser<'a, T, Error, F>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Token {
    Ident(String),
    Number(i64),
    Colon,
    Comma,
    Eq,
    Exporting,
    Exposing,
    FatArrow,
    Import,
    /// `{`
    LCurly,
    /// `(`
    LParen,
    Module,
    Plus,
    Print,
    /// `}`
    RCurly,
    /// `)`
    RParen,
    Semicolon,
    Label,
}

/// Parse a token from the text!
fn token<'a>() -> Parser<'a, Token> {
    fn number<'a>() -> Parser<'a, i64> {
        fn vec_to_string(vec: Vec<char>) -> String {
            vec.iter().cloned().collect()
        }

        Parser::digit()
            .map_fail(|_| ())
            .repeat_1()
            .map(vec_to_string)
            .map(|s| s.parse::<i64>().unwrap())
    }

    fn identifier_or_keyword<'a>() -> Parser<'a, Token> {
        fn start_char<'a>() -> Parser<'a, char> {
            Parser::char()
                .map_fail(|_| ())
                .filter(|&c| c.is_alphabetic() || c == '_', ())
        }

        fn end_char<'a>() -> Parser<'a, char> {
            Parser::char()
                .map_fail(|_| ())
                .filter(|&c| c.is_alphanumeric() || c == '_', ())
        }

        start_char()
            .and_then(|start_char| {
                end_char().repeat_0().map(move |end_chars| {
                    let mut identifier = String::new();
                    identifier.push(start_char);
                    identifier.extend(end_chars);
                    identifier
                })
            })
            // Keywords should be added here!
            .map(|ident| match ident.as_str() {
                "exporting" => Token::Exporting,
                "exposing" => Token::Exposing,
                "import" => Token::Import,
                "module" => Token::Module,
                "print" => Token::Print,
                "label" => Token::Label,
                _ => Token::Ident(ident),
            })
    }

    fn symbol<'a>(s: &'static str, ret: Token) -> Parser<'a, Token> {
        Parser::expect_string(s)
            .map(move |()| ret.clone())
            .map_fail(|_| ())
    }

    Parser::skip_whitespace()
        .and_then(|()| Parser::eof().not().map_fail(|()| ()))
        .and_then(|()| {
            one_of![
                symbol(";", Token::Semicolon),
                symbol(":", Token::Colon),
                symbol("=>", Token::FatArrow),
                symbol("=", Token::Eq),
                symbol(",", Token::Comma),
                symbol("+", Token::Plus),
                symbol("(", Token::LParen),
                symbol(")", Token::RParen),
                symbol("{", Token::LCurly),
                symbol("}", Token::RCurly),
                identifier_or_keyword(),
                number().map(Token::Number),
                Parser::err(Error::UnregocnizedToken),
            ]
            .map_fail(|_| unreachable!())
        })
}

fn token_eq<'a>(t: Token) -> Parser<'a, ()> {
    token().filter(move |t1| t1 == &t, ()).map(|_| ())
}

fn token_ident<'a>() -> Parser<'a, String> {
    token().and_then(|token| match token {
        Token::Ident(ident) => Parser::ret(ident),
        _ => Parser::fail(()),
    })
}

fn atom<'a>() -> Parser<'a, Expr> {
    token().and_then(|token| match token {
        Token::LParen => expr().and_then(|expr| {
            token_eq(Token::RParen)
                .map(move |_| expr.clone())
                .or_err(Error::MissingParen)
        }),
        Token::Number(n) => Parser::ret(Expr::Int(n)),
        Token::Ident(ident) => Parser::ret(Expr::Var(ident)),
        _ => Parser::fail(()),
    })
}

fn function_expr<'a>() -> Parser<'a, Expr> {
    token_ident().and_then(|param_name| {
        token_eq(Token::FatArrow).and_then(move |()| {
            let param_name = param_name.clone();
            expr().or_err(Error::NoFunctionBody).map(move |body| {
                let name = param_name.clone();
                let body = body.clone();
                Expr::Func(name, body.into())
            })
        })
    })
}

fn application_expr_or_atom<'a>() -> Parser<'a, Expr> {
    atom().repeat_1().map(|atoms| {
        debug_assert!(!atoms.is_empty(), "repeat_1 always returns non-empty");
        if let [lonely_atom] = atoms.as_slice() {
            lonely_atom.clone()
        } else {
            Expr::App(atoms.clone())
        }
    })
}

fn expr<'a>() -> Parser<'a, Expr> {
    one_of![
        function_expr(),
        atom().and_then(|left| {
            token_eq(Token::Plus)
                .and_then(move |_| expr())
                .map(move |right| Expr::BinOp(Box::new(left.clone()), BinOp::Add, Box::new(right)))
        }),
        application_expr_or_atom(),
        atom(),
    ]
    .map_fail(|_| ())
}

fn print_statement<'a>() -> Parser<'a, Statement> {
    token_eq(Token::Print).and_then(|()| {
        atom()
            .map(Statement::Print)
            .or_err(Error::PrintStatementExpectsSingleArgument)
    })
}

fn assignment_statement<'a>() -> Parser<'a, Statement> {
    token_ident().and_then(|ident| {
        token_eq(Token::Eq).and_then(move |_| {
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
    token_eq(Token::LParen)
        .or_err(on_missing_left_paren())
        .and_then(move |()| {
            // exporting ( f g h
            token_ident().repeat_0().and_then(move |names| {
                // exporting ( f g h )
                token_eq(Token::RParen)
                    .or_err(on_missing_right_paren())
                    .map(move |()| names.clone())
            })
        })
}

fn import_exposing<'a>() -> Parser<'a, Vec<String>> {
    token_eq(Token::Exposing).and_then(|()| {
        parenthesis_name_list(
            || Error::ExportingMissingLParen,
            || Error::ExportingMissingRParen,
        )
    })
}

fn import_statement<'a>() -> Parser<'a, Statement> {
    token_eq(Token::Import).and_then(|()| {
        token_ident()
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
    token_eq(Token::Label).and_then(|()| {
        // label Cons
        // token_ident().or_err(todo!()).and_then(|name| {
        token_ident().and_then(|name| {
            let name = name.clone();
            // label Cons head tail
            token_ident()
                .repeat_0()
                .map(move |parameters| Statement::Label {
                    name: name.clone(),
                    parameters: parameters.clone(),
                })
        })
    })
}

/// This parser never fails, always returns or errs.
fn statement<'a>() -> Parser<'a, Statement> {
    one_of![
        print_statement(),
        import_statement(),
        label_statement(),
        assignment_statement(),
        Parser::err(Error::BadStatement),
    ]
    .map_fail(|_| unreachable!())
    .and_then(|s| {
        token_eq(Token::Semicolon)
            .or_err(Error::MissingSemicolon)
            .map(move |_| s.clone())
    })
}

fn statement_but_fails_on_eof<'a>() -> Parser<'a, Statement> {
    (Parser::skip_whitespace().and_then(|()| Parser::eof().not())).and_then(|()| statement())
}

fn module_exporting<'a>() -> Parser<'a, Vec<String>> {
    token_eq(Token::Exporting).and_then(|()| {
        parenthesis_name_list(
            || Error::ExportingMissingLParen,
            || Error::ExportingMissingRParen,
        )
    })
}

fn module_declaration<'a>() -> Parser<'a, ModuleDecl> {
    token_eq(Token::Module).and_then(|()| {
        token_ident()
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
        statement_but_fails_on_eof()
            .repeat_0()
            .map(move |statements| Program {
                module_decl: module_decl.clone(),
                statement: statements,
            })
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
    fn test_number_token() {
        let res = token().parse(
            indoc! {r"
                1234
            "}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Token::Number(1234),
                Pos {
                    row: 1,
                    col: 5,
                    offset: 4
                },
            ),
        );
    }

    #[test]
    fn token_left_paren() {
        let res = token().parse(
            indoc! {r"
                (
            "}
            .into(),
        );
        assert_eq!(
            res,
            ParseResult::Ok(
                Token::LParen,
                Pos {
                    offset: 1,
                    row: 1,
                    col: 2,
                },
            ),
        );
    }

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
                statement: vec![],
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
                statement: vec![],
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
                statement: vec![
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
            }),
        );
    }
}
