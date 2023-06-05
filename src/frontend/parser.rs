use std::iter::Peekable;

use crate::common::operator::Operator;
use crate::common::parsed_ast::{Block, Expression, Program, Statement, TopStatement};
use crate::common::position::Position;

use crate::frontend::lexer::Lexer;
use crate::frontend::error::{Error, LocatedError};
use crate::frontend::token::{LocatedToken, Token};

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    fallback_position: Position,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            fallback_position: lexer.position().clone(),
            lexer: lexer.peekable(),
        }
    }

    fn handle_unexpected(&mut self, lt: LocatedToken<'a>) -> LocatedError {
        match lt.token {
            Token::Unexpected(c) => LocatedError {
                position: lt.position,
                error: Error::UnexpectedChar(c),
            },
            _ => LocatedError {
                position: lt.position,
                error: Error::UnexpectedToken(lt.token.to_string()),
            },
        }
    }

    fn expect_next(&mut self, expected: Token) -> Result<LocatedToken<'a>, LocatedError> {
        match self.lexer.next() {
            Some(lt) if std::mem::discriminant(&lt.token) == std::mem::discriminant(&expected) => {
                Ok(lt)
            }
            Some(lt) => Err(self.handle_unexpected(lt)),
            None => Err(LocatedError {
                error: Error::UnexpectedEof,
                position: self.fallback_position.clone(),
            }),
        }
    }

    pub fn parse(&mut self) -> Result<Program, LocatedError> {
        let mut program = Program::new();

        while let Some(lt) = self.lexer.next() {
            match lt.token {
                Token::Let => program.statements.push(self.parse_top_let()?),
                Token::Fn => program.statements.push(self.parse_fn_declaration()?),
                _ => return Err(self.handle_unexpected(lt)),
            }
        }

        Ok(program)
    }

    fn parse_top_let(&mut self) -> Result<TopStatement, LocatedError> {
        let id = self.expect_next(Token::Id(""))?;
        self.expect_next(Token::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(TopStatement::Let {
            value,
            id: id.token.to_string(),
        })
    }

    fn parse_fn_declaration(&mut self) -> Result<TopStatement, LocatedError> {
        let id = self.expect_next(Token::Id(""))?;
        self.expect_next(Token::LParen)?;
        // TODO: Parse Paramters
        self.expect_next(Token::RParen)?;
        // TODO: Pare Return Type
        self.expect_next(Token::LBrace)?;
        let body = self.parse_block()?;
        self.expect_next(Token::RBrace)?;
        Ok(TopStatement::Fn {
            body,
            id: id.token.to_string(),
        })
    }

    fn parse_block(&mut self) -> Result<Block, LocatedError> {
        let mut statements = Vec::new();
        while let Some(lt) = self.lexer.peek() {
            match lt.token {
                Token::RBrace => break,
                Token::Let => statements.push(self.parse_let()?),
                Token::Var => statements.push(self.parse_var()?),
                Token::Return => statements.push(self.parse_return()?),
                _ => statements.push(self.parse_expression_statement()?),
            }
        }
        Ok(statements)
    }

    fn parse_let(&mut self) -> Result<Statement, LocatedError> {
        self.expect_next(Token::Let)?;
        let id = self.expect_next(Token::Id(""))?;
        self.expect_next(Token::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(Statement::Let {
            value,
            id: id.token.to_string(),
        })
    }

    fn parse_var(&mut self) -> Result<Statement, LocatedError> {
        self.expect_next(Token::Var)?;
        let id = self.expect_next(Token::Id(""))?;
        self.expect_next(Token::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(Statement::Var {
            value,
            id: id.token.to_string(),
        })
    }

    fn parse_return(&mut self) -> Result<Statement, LocatedError> {
        self.expect_next(Token::Return)?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(Statement::Return { value: expr })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, LocatedError> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, LocatedError> {
        self.handle_prefix()
    }

    fn handle_prefix(&mut self) -> Result<Expression, LocatedError> {
        let lt = self.lexer.next().ok_or(LocatedError {
            error: Error::UnexpectedEof,
            position: self.fallback_position.clone(),
        })?;
        match lt.token {
            Token::Id(id) => Ok(Expression::Lvalue {
                id: id.to_string(),
                position: lt.position.clone(),
            }),
            _ => Err(self.handle_unexpected(lt)),
        }
    }
}
