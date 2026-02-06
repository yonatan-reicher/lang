//! This module is responsible for parsing tokens into an Abstract Syntax Tree (AST)!

use crate::ast::*;
use crate::lex2::Lexer;
use crate::token::*;
use std::io::Result;
use std::rc::Rc;

#[derive(Debug)]
struct Parser<'a> {
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
}

macro_rules! only_if_pop_eq {
    ($self:ident, $token:expr) => {
        if !$self.pop_eq($token)? {
            return Ok(None);
        }
    };
}

macro_rules! any {
    ($e:expr $(,)?) => {
        $e?
    };
    ($e1:expr, $($e:expr),* $(,)? ) => {
        if let Some(v) = $e1? {
            Some(v)
        } else {
            any!($($e),*)
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
        Ok(Program {
            module_decl: self.module_decl()?,
            statements: vec![],
            return_expr: None,
        })
    }

    // --- Module declaration ---

    fn module_decl(&mut self) -> Result<Option<ModuleDecl>> {
        only_if_pop_eq!(self, Keyword::Module);
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
        only_if_pop_eq!(self, Keyword::Exporting);
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
        let return_expr = self.expr()?;
        Ok(Block {
            statements,
            return_expr: Some(return_expr),
        })
    }

    fn statement(&mut self) -> Result<Option<Statement>> {
        Ok(any![
            self.print_statement(),
            self.import_statement(),
            self.label_statement(),
            self.assignment_statement(),
        ])
    }

    fn print_statement(&mut self) -> Result<Option<Statement>> {
        only_if_pop_eq!(self, Keyword::Print);
        let expr = self.expr()?;
        Ok(Some(Statement::Print(expr)))
    }

    fn import_statement(&mut self) -> Result<Option<Statement>> {
        only_if_pop_eq!(self, Keyword::Import);
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
        only_if_pop_eq!(self, Keyword::Exposing);
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
        only_if_pop_eq!(self, Keyword::Label);
        let Some(name) = self.pop_id()? else {
            todo!()
        };
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
        todo!()
    }

    // --- Expressions ---

    fn expr(&mut self) -> Result<Expr> {
        todo!()
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
            TokenKind::Kw(_) => None,
            TokenKind::Sym(_) => None,
        })
    }

    fn repeat<T>(&mut self, item: impl Fn(&mut Self) -> Result<Option<T>>) -> Result<Vec<T>> {
        let mut ret = vec![];
        while let Some(item) = item(self)? {
            ret.push(item);
        }
        Ok(ret)
    }

    fn separated_list<T>(
        &mut self,
        item: impl Fn(&mut Self) -> Result<Option<T>>,
        sep: impl Fn(&mut Self) -> Result<bool>,
    ) -> Result<Vec<T>> {
        let mut ret = vec![];
        while let Some(item) = item(self)? {
            ret.push(item);
            if !sep(self)? {
                break;
            }
        }
        Ok(ret)
    }
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
