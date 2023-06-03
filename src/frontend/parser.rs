use std::fmt::{Display, Formatter};
use std::iter::Peekable;

use crate::common::operator::Operator;
use crate::common::parsed_ast::{Expression, Program, Statement};
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

    pub fn parse(&mut self) -> Result<Program, LocatedError> {
        let mut program = Program::new();

        while let Some(lt) = self.lexer.next() {
            match lt.token {
                Token::Let => {
                    let statement = self.parse_let_statement()?;
                    program.statements.push(statement)
                }

                Token::Return => {
                    let statement = self.parse_return_statement()?;
                    program.statements.push(statement)
                }

                Token::If => {
                    let statement = self.parse_if_statement()?;
                    program.statements.push(statement)
                }

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

    fn advance_or_err(&mut self, expected: Token) -> Result<LocatedToken<'a>, LocatedError> {
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

    fn parse_let_statement(&mut self) -> Result<Statement, LocatedError> {
        let id = self.advance_or_err(Token::Id(""))?;
        self.advance_or_err(Token::Assign)?;
        let expr = self.parse_expression()?;
        Ok(Statement::Let {
            value: expr,
            position: id.position,
            id: id.token.to_string(),
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, LocatedError> {
        unimplemented!()
    }

    fn parse_if_statement(&mut self) -> Result<Statement, LocatedError> {
        unimplemented!()
    }

    fn parse_expression(&mut self) -> Result<Expression, LocatedError> {
        if let Some(lt) = self.lexer.peek() {
            let expr = match lt.token {
                Token::Int(i) => {
                    self.lexer.next();
                    if let Some(lt) = self.lexer.peek() {
                        match lt.token {
                            Token::Plus => self.parse_operator_expression(i, lt.position.clone()),

                            Token::Semi => self.parse_integer_literal(i, lt.position.clone()),

                            _ => Err(LocatedError {
                                position: lt.position.clone(),
                                error: Error::UnexpectedToken(lt.token.to_string()),
                            }),
                        }
                    } else {
                        Err(LocatedError {
                            error: Error::UnexpectedEof,
                            position: self.fallback_position.clone(),
                        })
                    }
                }

                Token::LParen => self.parse_grouped_expression(),

                _ => Err(LocatedError {
                    position: lt.position.clone(),
                    error: Error::UnexpectedToken(lt.token.to_string()),
                }),
            };

            self.lexer.next();
            expr
        } else {
            Err(LocatedError {
                error: Error::UnexpectedEof,
                position: self.fallback_position.clone(),
            })
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, LocatedError> {
        unimplemented!()
    }

    fn parse_operator_expression(
        &mut self,
        lhs: &str,
        position: Position,
    ) -> Result<Expression, LocatedError> {
        let value = lhs.parse::<i64>().map_err(|_| LocatedError {
            position,
            error: Error::InvalidInt,
        })?;
        let lhs = Expression::Integer {
            value,
            position: position,
        };
        self.lexer.next();
        let rhs = self.parse_expression()?;
        Ok(Expression::Binary {
            position,
            op: Operator::Plus,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    fn parse_integer_literal(
        &mut self,
        i: &str,
        position: Position,
    ) -> Result<Expression, LocatedError> {
        let value = i.parse::<i64>().map_err(|_| LocatedError {
            position,
            error: Error::InvalidInt,
        })?;
        Ok(Expression::Integer { value, position })
    }
}
