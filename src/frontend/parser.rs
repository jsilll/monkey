use std::iter::Peekable;

use crate::common::operator::{BinOp, UnOp};
use crate::common::parsed_ast::{Block, Expression, InnerStatement, Program, TopStatement};
use crate::common::position::Position;

use crate::frontend::error::{Error, LocatedError};
use crate::frontend::lexer::Lexer;
use crate::frontend::token::{LocatedToken, Token};

impl UnOp {
    fn from_token(token: &Token<'_>) -> Self {
        match token {
            Token::Bang => UnOp::Bang,
            Token::Minus => UnOp::Minus,
            _ => unreachable!(),
        }
    }
}

impl BinOp {
    fn from_token(token: &Token<'_>) -> Self {
        match token {
            Token::Plus => BinOp::Plus,
            Token::Minus => BinOp::Minus,
            Token::Star => BinOp::Star,
            Token::Slash => BinOp::Slash,
            Token::Eq => BinOp::Eq,
            Token::Neq => BinOp::Neq,
            Token::Lt => BinOp::Lt,
            Token::Gt => BinOp::Gt,
            _ => unreachable!(),
        }
    }
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn from_token(token: &Token<'_>) -> Self {
        match token {
            Token::Eq | Token::Neq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Star | Token::Slash => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
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
                Token::Fn => program.statements.push(self.parse_top_fn()?),
                Token::Let => program.statements.push(self.parse_top_let()?),
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

    fn parse_top_fn(&mut self) -> Result<TopStatement, LocatedError> {
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
                Token::Let => statements.push(self.parse_inner_let()?),
                Token::Var => statements.push(self.parse_inner_var()?),
                Token::Return => statements.push(self.parse_inner_return()?),
                _ => statements.push(self.parse_expression_statement()?),
            }
        }
        Ok(statements)
    }

    fn parse_inner_let(&mut self) -> Result<InnerStatement, LocatedError> {
        self.expect_next(Token::Let)?;
        let id = self.expect_next(Token::Id(""))?;
        self.expect_next(Token::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(InnerStatement::Let {
            value,
            id: id.token.to_string(),
        })
    }

    fn parse_inner_var(&mut self) -> Result<InnerStatement, LocatedError> {
        self.expect_next(Token::Var)?;
        let id = self.expect_next(Token::Id(""))?;
        self.expect_next(Token::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(InnerStatement::Var {
            value,
            id: id.token.to_string(),
        })
    }

    fn parse_inner_return(&mut self) -> Result<InnerStatement, LocatedError> {
        self.expect_next(Token::Return)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semi)?;
        Ok(InnerStatement::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Result<InnerStatement, LocatedError> {
        let value = self.parse_expression(Precedence::Lowest)?;
        let lt = self.lexer.peek().ok_or(LocatedError {
            error: Error::UnexpectedEof,
            position: self.fallback_position.clone(),
        })?;
        match lt.token {
            Token::Semi => {
                self.lexer.next();
                Ok(InnerStatement::Expression(value))
            }
            _ => Ok(InnerStatement::Return(value)),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, LocatedError> {
        let mut lhs = self.handle_prefix_token()?;

        while let Some(lt) = self.lexer.peek() {
            if lt.token == Token::Semi
                || lt.token == Token::RBrace
                || precedence < Precedence::from_token(&lt.token)
            {
                break;
            } else {
                self.lexer.next();
                lhs = self.handle_infix_token(lhs)?;
            }
        }

        Ok(lhs)
    }

    fn handle_prefix_token(&mut self) -> Result<Expression, LocatedError> {
        let lt = self.lexer.next().ok_or(LocatedError {
            error: Error::UnexpectedEof,
            position: self.fallback_position.clone(),
        })?;
        match lt.token {
            Token::Id(id) => Ok(Expression::Lvalue {
                id: id.to_string(),
                position: lt.position.clone(),
            }),
            Token::Int(i) => {
                let value = i.parse::<i64>().map_err(|_| LocatedError {
                    error: Error::InvalidInt,
                    position: lt.position.clone(),
                })?;
                Ok(Expression::IntegerLiteral {
                    value,
                    position: lt.position.clone(),
                })
            }
            Token::Minus | Token::Bang => {
                let rhs = Box::new(self.parse_expression(Precedence::Prefix)?);
                Ok(Expression::Unary {
                    rhs,
                    op: UnOp::from_token(&lt.token),
                })
            }
            _ => Err(self.handle_unexpected(lt)),
        }
    }

    fn handle_infix_token(&mut self, lhs: Expression) -> Result<Expression, LocatedError> {
        let lt = self.lexer.next().ok_or(LocatedError {
            error: Error::UnexpectedEof,
            position: self.fallback_position.clone(),
        })?;
        let precedence = Precedence::from_token(&lt.token);
        let rhs = self.parse_expression(precedence)?;
        Ok(Expression::Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: BinOp::from_token(&lt.token),
        })
    }
}
