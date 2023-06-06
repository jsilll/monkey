use std::fmt::{Display, Formatter};

use crate::common::Position;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct LocatedError {
    pub error: Error,
    pub position: Position,
}

impl Display for LocatedError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.error, self.position)
    }
}

pub mod token;

pub mod lexer;

pub mod parser;