//! This module is responsible for parsing tokens into an Abstract Syntax Tree (AST)!

use crate::ast::*;
use crate::lex::Lexer;
use crate::token::*;
use std::io::Result;
use std::rc::Rc;

pub fn parse<'a, T>(source: T) -> Result<Program>
where
    Parser<'a>: From<T>,
{
    let mut p = Parser::from(source);
    p.program()
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a, L: Into<Lexer<'a>>> From<L> for Parser<'a> {
    fn from(lexer: L) -> Self {
        Self {
            lexer: lexer.into(),
        }
    }
}

// --- Some helpers ---

impl<'a> Parser<'a> {
    fn peek(&mut self) -> Result<Token> {
        self.lexer.peek()
    }

    fn pop(&mut self) -> Result<Token> {
        self.lexer.pop()
    }

    fn pop_eq<T>(&mut self, other: T) -> Result<bool>
    where
        Token: PartialEq<T>,
    {
        Ok(if self.peek()? == other {
            self.pop()?;
            true
        } else {
            false
        })
    }

    fn pop_id(&mut self) -> Result<Option<Rc<str>>> {
        Ok(if let TokenKind::Id(id) = self.peek()?.kind {
            self.pop()?;
            Some(id)
        } else {
            None
        })
    }

    /// Pop an identifier, only if it is followed by some specific token.
    fn pop_id_followed_by<T>(&mut self, after: T) -> Result<Option<Rc<str>>>
    where
        Token: PartialEq<T>,
    {
        // Works by using the push-back mechanism which let's us restore a single token backwards.
        let id_token = self.peek()?;
        let Some(id) = self.pop_id()? else {
            return Ok(None);
        };
        if !self.pop_eq(after)? {
            self.lexer.push_back(id_token);
            return Ok(None);
        }
        Ok(Some(id))
    }

    /// Pop an identifier, only if it is followed by some specific token.
    fn pop_id_then<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<Option<T>>,
    ) -> Result<Option<(Rc<str>, T)>> {
        // Works by using the push-back mechanism which let's us restore a single token backwards.
        let id_token = self.peek()?;
        let Some(id) = self.pop_id()? else {
            return Ok(None);
        };
        let Some(ret) = f(self)? else {
            self.lexer.push_back(id_token);
            return Ok(None);
        };
        Ok(Some((id, ret)))
    }
}

macro_rules! only_if {
    ($self:ident.pop_eq($token:expr)) => {
        if !$self.pop_eq($token)? {
            return Ok(None);
        }
    };
    ($p:pat = $e:expr) => {
        let Some($p) = $e? else {
            return Ok(None);
        };
    };
}

/// `one_of![a, b, c]` returns the first expression which evaluates to `Ok(Some(..))`, or that
/// returns some `Err(..)`.
macro_rules! one_of {
    ($e:expr $(,)?) => {
        $e?
    };
    ($e1:expr, $($e:expr),* $(,)? ) => {
        if let Some(v) = $e1? {
            Some(v)
        } else {
            one_of!($($e),*)
        }
    };
}

// --- Actual implementation ---

#[derive(Clone, Debug, derive_more::From, PartialEq)]
struct Block {
    statements: Vec<Statement>,
    return_expr: Option<Expr>,
}

impl<'a> Parser<'a> {
    pub fn program(&mut self) -> Result<Program> {
        let module_decl = self.module_decl()?;
        let block = self.block()?;
        if !self.pop_eq(TokenKind::Eof)? {
            todo!()
        }
        Ok(Program {
            module_decl,
            statements: block.statements,
            return_expr: block.return_expr,
        })
    }

    // --- Module declaration ---

    fn module_decl(&mut self) -> Result<Option<ModuleDecl>> {
        only_if!(self.pop_eq(Keyword::Module));
        let Some(name) = self.pop_id()? else {
            todo!();
        };
        let exporting = self.exporting()?;
        Ok(Some(ModuleDecl {
            name: name.as_ref().into(),
            exports: exporting
                .unwrap_or_default()
                .iter()
                .map(|id| id.as_ref().into())
                .collect(),
        }))
    }

    fn exporting(&mut self) -> Result<Option<Vec<Rc<str>>>> {
        only_if!(self.pop_eq(Keyword::Exporting));
        if !self.pop_eq(Symbol::LParen)? {
            todo!()
        }
        let items = self.repeat(Self::pop_id)?;
        if !self.pop_eq(Symbol::RParen)? {
            todo!()
        }
        Ok(Some(items))
    }

    // --- Blocks and statements ---

    /// Parse a block - a series of statements, maybe followed by a returned expression. Does
    /// not parse any braces or delimiters around.
    fn block(&mut self) -> Result<Block> {
        let mut statements = vec![];
        while let Some(s) = self.statement()? {
            statements.push(s);
        }
        // May or may not have an expression at the end.
        let return_expr = self.maybe_expr()?;
        Ok(Block {
            statements,
            return_expr,
        })
    }

    fn statement(&mut self) -> Result<Option<Statement>> {
        let Some(s) = one_of![
            self.print_statement(),
            self.import_statement(),
            self.label_statement(),
            self.assignment_statement(),
        ] else {
            return Ok(None);
        };
        if !self.pop_eq(Symbol::Semicolon)? {
            todo!()
        }
        Ok(Some(s))
    }

    fn print_statement(&mut self) -> Result<Option<Statement>> {
        only_if!(self.pop_eq(Keyword::Print));
        let expr = self.expr()?;
        Ok(Some(Statement::Print(expr)))
    }

    fn import_statement(&mut self) -> Result<Option<Statement>> {
        only_if!(self.pop_eq(Keyword::Import));
        let Some(module_name) = self.pop_id()? else {
            todo!()
        };
        let exposing = self.import_exposing()?.unwrap_or_default();
        Ok(Some(Statement::Import {
            module_name: module_name.to_string(),
            imports: exposing.into_iter().map(|id| id.to_string()).collect(),
        }))
    }

    fn import_exposing(&mut self) -> Result<Option<Vec<Rc<str>>>> {
        only_if!(self.pop_eq(Keyword::Exposing));
        if !self.pop_eq(Symbol::LParen)? {
            todo!()
        }
        let items = self.repeat(Self::pop_id)?;
        if !self.pop_eq(Symbol::RParen)? {
            todo!()
        }
        Ok(Some(items))
    }

    fn label_statement(&mut self) -> Result<Option<Statement>> {
        only_if!(self.pop_eq(Keyword::Label));
        let Some(name) = self.pop_id()? else { todo!() };
        let parameters = self.repeat(Self::pop_id)?;
        Ok(Some(Statement::Label {
            name: name.as_ref().into(),
            parameters: parameters
                .into_iter()
                .map(|id| id.as_ref().into())
                .collect(),
        }))
    }

    fn assignment_statement(&mut self) -> Result<Option<Statement>> {
        only_if!(id = self.pop_id_followed_by(Symbol::Equal));
        let rhs = self.expr()?;
        Ok(Some(Statement::Assignment(id.to_string(), rhs)))
    }

    // --- Expressions ---

    fn expr(&mut self) -> Result<Expr> {
        if let Some(e) = self.maybe_expr()? {
            Ok(e)
        } else {
            todo!()
        }
    }

    fn maybe_expr(&mut self) -> Result<Option<Expr>> {
        Ok(one_of![
            self.if_expr(),
            self.match_expr(),
            self.function_expr(),
            self.binop_expr(),
        ])
    }

    fn if_expr(&mut self) -> Result<Option<Expr>> {
        only_if!(self.pop_eq(Keyword::If));
        let cond = self.expr()?;
        if !self.pop_eq(Keyword::Then)? {
            todo!()
        }
        let x = self.expr()?;
        if !self.pop_eq(Keyword::Else)? {
            todo!()
        }
        let y = self.expr()?;
        Ok(Some(Expr::If(Box::new((cond, x, y)))))
    }

    fn match_expr(&mut self) -> Result<Option<Expr>> {
        only_if!(self.pop_eq(Keyword::Match));
        let input = self.expr()?;
        let arms = self.repeat(Self::match_arm)?;
        Ok(Some(Expr::Match(Box::new(input), arms)))
    }

    fn match_arm(&mut self) -> Result<Option<MatchArm>> {
        only_if!(self.pop_eq(Symbol::Pipe));
        let p = self.pattern()?;
        if !self.pop_eq(Symbol::FatArrow)? {
            todo!()
        }
        let e = self.expr()?;
        Ok(Some(MatchArm(p, e)))
    }

    fn function_expr(&mut self) -> Result<Option<Expr>> {
        only_if!(param_id = self.pop_id_followed_by(Symbol::FatArrow));
        let rhs = self.expr()?;
        Ok(Some(Expr::Func(param_id.to_string(), Box::new(rhs))))
    }

    fn binop_expr(&mut self) -> Result<Option<Expr>> {
        only_if!(lhs = self.application_expr());
        Ok(Some(if let Some(op) = self.binop()? {
            let Some(rhs) = self.binop_expr()? else {
                todo!()
            };
            Expr::BinOp(Box::new(lhs), op, Box::new(rhs))
        } else {
            lhs
        }))
    }

    fn binop(&mut self) -> Result<Option<BinOp>> {
        let t = self.peek()?.kind;
        let ret = Self::BINOPS
            .iter()
            .find(|(t2, _)| *t2 == t)
            .map(|(_, op)| *op);
        if ret.is_some() {
            self.pop()?;
        }
        Ok(ret)
    }

    const BINOPS: &'static [(TokenKind, BinOp)] = &[
        (TokenKind::Sym(Symbol::Plus), BinOp::Add),
        (TokenKind::Sym(Symbol::Minus), BinOp::Sub),
        (TokenKind::Sym(Symbol::Star), BinOp::Mul),
        (TokenKind::Sym(Symbol::Slash), BinOp::Div),
        (TokenKind::Sym(Symbol::Equal), BinOp::Eq),
        (TokenKind::Sym(Symbol::BangEqual), BinOp::NEq),
        (TokenKind::Kw(Keyword::And), BinOp::And),
        (TokenKind::Kw(Keyword::Or), BinOp::Or),
    ];

    fn application_expr(&mut self) -> Result<Option<Expr>> {
        let mut atoms = self.repeat(Self::atom)?;
        Ok(match atoms.len() {
            0 => None,
            1 => Some(atoms.pop().unwrap()),
            _ => Some(Expr::App(atoms)),
        })
    }

    fn atom(&mut self) -> Result<Option<Expr>> {
        Ok(match self.peek()?.kind {
            TokenKind::Err => todo!(),
            TokenKind::Eof => None,
            TokenKind::Id(id) => {
                self.pop()?;
                Some(Expr::Var(id.as_ref().into()))
            }
            TokenKind::Num(num) => {
                self.pop()?;
                Some(Expr::Int(num))
            }
            TokenKind::Str(str) => {
                self.pop()?;
                Some(Expr::Str(str))
            }
            TokenKind::Kw(k @ Keyword::True | k @ Keyword::False) => {
                self.pop()?;
                Some(Expr::Bool(k == Keyword::True))
            }
            TokenKind::Sym(Symbol::LParen) => {
                self.pop()?;
                let inside = self.block()?;
                if !self.pop_eq(Symbol::RParen)? {
                    todo!()
                }
                Some(match (inside.statements.is_empty(), inside.return_expr) {
                    (true, Some(e)) => e,
                    (false, Some(e)) => Expr::Statements(inside.statements, Box::new(e)),
                    (_, None) => todo!(),
                })
            }
            TokenKind::Kw(_) => None,
            TokenKind::Sym(_) => None,
        })
    }

    // --- Pattern ---

    fn pattern(&mut self) -> Result<Pattern> {
        Ok(if let Some(p) = self.maybe_pattern()? {
            p
        } else {
            todo!()
        })
    }

    fn maybe_pattern(&mut self) -> Result<Option<Pattern>> {
        Ok(one_of![self.applied_label_pattern(), self.pattern_atom()])
    }

    fn applied_label_pattern(&mut self) -> Result<Option<Pattern>> {
        only_if!((label_id, param_1) = self.pop_id_then(Self::pattern_atom));
        let mut rest = self.repeat(Self::pattern_atom)?;
        rest.insert(0, param_1);
        Ok(Some(Pattern::Label {
            name: label_id.to_string(),
            parameter_patterns: rest,
        }))
    }

    fn pattern_atom(&mut self) -> Result<Option<Pattern>> {
        Ok(one_of![
            self.wildcard_pattern(),
            self.parenthisized_pattern(),
            self.id_pattern(),
        ])
    }

    fn wildcard_pattern(&mut self) -> Result<Option<Pattern>> {
        only_if!(self.pop_eq(Symbol::Underscore));
        Ok(Some(Pattern::Wildcard))
    }

    fn parenthisized_pattern(&mut self) -> Result<Option<Pattern>> {
        only_if!(self.pop_eq(Symbol::LParen));
        let inside = self.pattern()?;
        if !self.pop_eq(Symbol::RParen)? {
            todo!()
        }
        Ok(Some(inside))
    }

    fn id_pattern(&mut self) -> Result<Option<Pattern>> {
        only_if!(id = self.pop_id());
        let is_label = id.chars().next().is_some_and(char::is_uppercase);
        Ok(Some(if is_label {
            Pattern::Label {
                name: id.to_string(),
                parameter_patterns: vec![],
            }
        } else {
            Pattern::Var(id.to_string())
        }))
    }

    // --- Other ---

    fn repeat<T>(&mut self, item: impl Fn(&mut Self) -> Result<Option<T>>) -> Result<Vec<T>> {
        let mut ret = vec![];
        while let Some(item) = item(self)? {
            ret.push(item);
        }
        Ok(ret)
    }

    // fn separated_list<T>(
    //     &mut self,
    //     item: impl Fn(&mut Self) -> Result<Option<T>>,
    //     sep: impl Fn(&mut Self) -> Result<bool>,
    // ) -> Result<Vec<T>> {
    //     let mut ret = vec![];
    //     while let Some(item) = item(self)? {
    //         ret.push(item);
    //         if !sep(self)? {
    //             break;
    //         }
    //     }
    //     Ok(ret)
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! parser_test {
        (
            name = $name:ident,
            text = $text:literal,
            $function:ident() = $expected:expr,
        ) => {
            #[test]
            fn $name() {
                let source: &'static str = indoc! { $text };
                let mut parser = Parser::from(source.as_bytes());
                assert_eq!(parser.$function().unwrap(), $expected,);
            }
        };
    }

    parser_test! {
        name = empty_program,
        text = "",
        program() = Program {
            module_decl: None,
            statements: vec![],
            return_expr: None,
        },
    }

    parser_test! {
        name = number_atom,
        text = "3",
        atom() = Some(Expr::from(3)),
    }

    parser_test! {
        name = addition_and_parenthesis,
        text = "
            (1 + 2) + 3
        ",
        expr() = Expr::BinOp(
            Box::new(Expr::BinOp(
                Box::new(Expr::Int(1)),
                BinOp::Add,
                Box::new(Expr::Int(2)),
            )),
            BinOp::Add,
            Box::new(Expr::Int(3)),
        ),
    }

    parser_test! {
        name = empty_module,
        text = "module foo",
        program() = Program {
            module_decl: Some(ModuleDecl {
                name: "foo".to_string(),
                exports: vec![],
            }),
            statements: vec![],
            return_expr: None,
        },
    }

    parser_test! {
        name = empty_module_empty_exporting,
        text = "
            module foo exporting ()
        ",
        program() = Program {
            module_decl: Some(ModuleDecl {
                name: "foo".to_string(),
                exports: vec![],
            }),
            statements: vec![],
            return_expr: None,
        },
    }

    parser_test! {
        name = variable_atom,
        text = "hello",
        atom() = Some(Expr::Var("hello".to_string())),
    }

    parser_test! {
        name = module_with_exporting,
        text = "
            module modmod exporting (
                hello friend
            )
            hello = 2;
            print hello;
            world = 1 + 2;
            friend = (1 + 1) + 1;
        ",
        program() = Program {
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
        },
    }

    parser_test! {
        name = return_expr,
        text = "
            hello = 2;
            hello
        ",
        program() = Program {
            module_decl: None,
            statements: vec![Statement::Assignment("hello".to_string(), Expr::Int(2))],
            return_expr: Some(Expr::Var("hello".to_string())),
        },
    }

    parser_test! {
        // As you can see, binary operators are (for now) parsed precedence-less
        name = binary_operator,
        text = "
            1 + 2 * 3 + 1
        ",
        program() = Program {
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
            )),
        },
    }
}
