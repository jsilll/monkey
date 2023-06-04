use std::fmt::{Display, Formatter};
use std::iter::Peekable;

use crate::common::operator::Operator;
use crate::common::parsed_ast::{Expression, Program, Statement, TopLvlStatement};
use crate::common::position::Position;

use crate::frontend::lexer::Lexer;
use crate::frontend::token::{LocatedToken, Token};

pub enum Error {
    InvalidInt,
    UnexpectedEof,
    UnexpectedChar(char),
    UnexpectedToken(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidInt => write!(f, "Invalid integer literal"),
            Error::UnexpectedEof => write!(f, "Unexpected end of file"),
            Error::UnexpectedChar(c) => write!(f, "Unexpected character '{}'", c),
            Error::UnexpectedToken(token) => write!(f, "Unexpected token {}", token),
        }
    }
}

pub struct LocatedError {
    pub error: Error,
    pub position: Position,
}

impl Display for LocatedError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.error, self.position)
    }
}

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

    fn expect_next(&mut self, expected: Token) -> Result<LocatedToken<'a>, LocatedError> {
        match self.lexer.next() {
            Some(lt) if std::mem::discriminant(&lt.token) == std::mem::discriminant(&expected) => {
                Ok(lt)
            }

            Some(token) => Err(LocatedError {
                position: token.position,
                error: Error::UnexpectedToken(token.token.to_string()),
            }),

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
                Token::Let => {
                    let id = self.expect_next(Token::Id(""))?;
                    self.expect_next(Token::Assign)?;
                    let expr = self.parse_expression(Precedence::Lowest)?;
                    self.expect_next(Token::Semi)?;
                    program.statements.push(TopLvlStatement::Let {
                        value: expr,
                        id: id.token.to_string(),
                    })
                }

                // TODO: Token::Fn

                // TODO: Move this
                // Token::Return => {
                //     let expr = self.parse_expression(Precedence::Lowest)?;
                //     self.expect_next(Token::Semi)?;
                //     program.statements.push(Statement::Return { value: expr })
                // }

                Token::Unexpected(c) => {
                    return Err(LocatedError {
                        position: lt.position,
                        error: Error::UnexpectedChar(c),
                    })
                }

                _ => {
                    return Err(LocatedError {
                        position: lt.position,
                        error: Error::UnexpectedToken(lt.token.to_string()),
                    })
                }
            }
        }

        Ok(program)
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
            _ => Err(LocatedError {
                position: lt.position.clone(),
                error: Error::UnexpectedToken(lt.token.to_string()),
            }),
        }
    }

    fn handle_postfix(&mut self, lt: LocatedToken) -> Result<Expression, LocatedError> {
        unimplemented!()
    }
}
