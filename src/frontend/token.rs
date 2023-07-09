use std::fmt::{Display, Formatter};

use crate::common::Position;

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    // Literals
    True,
    False,
    Int(&'a str),
    // Keywords
    Fn,
    Let,
    Var,
    Return,
    If,
    Else,
    // Primitive Types
    Bool,
    Int32,
    // Identifiers
    Id(&'a str),
    // Assignment Operator
    Assign,
    // Arithmetic Operators
    Plus,
    Minus,
    Star,
    Slash,
    // Comparison Operators
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    // Logical Operators
    Bang,
    // Punctuation
    Arrow,
    Comma,
    Semi,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    // Unexpected
    Unexpected(char),
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::True => write!(f, "'true'"),
            Token::False => write!(f, "'false'"),
            Token::Int(i) => write!(f, "'{}'", i),
            Token::Fn => write!(f, "'fn'"),
            Token::Let => write!(f, "'let'"),
            Token::Var => write!(f, "'var'"),
            Token::Return => write!(f, "'return'"),
            Token::If => write!(f, "'if'"),
            Token::Else => write!(f, "'else'"),
            Token::Bool => write!(f, "'bool'"),
            Token::Int32 => write!(f, "'i32'"),
            Token::Id(id) => write!(f, "'{}'", id),
            Token::Assign => write!(f, "'='"),
            Token::Plus => write!(f, "'+'"),
            Token::Minus => write!(f, "'-'"),
            Token::Star => write!(f, "'*'"),
            Token::Slash => write!(f, "'/'"),
            Token::Eq => write!(f, "'=='"),
            Token::Neq => write!(f, "'!='"),
            Token::Lt => write!(f, "'<'"),
            Token::Gt => write!(f, "'>'"),
            Token::Lte => write!(f, "'<='"),
            Token::Gte => write!(f, "'>='"),
            Token::Bang => write!(f, "'!'"),
            Token::Arrow => write!(f, "'->'"),
            Token::Comma => write!(f, "','"),
            Token::Semi => write!(f, "';'"),
            Token::Colon => write!(f, "':'"),
            Token::LParen => write!(f, "'('"),
            Token::RParen => write!(f, "')'"),
            Token::LBrace => write!(f, "'{{'"),
            Token::RBrace => write!(f, "'}}'"),
            Token::Unexpected(c) => write!(f, "Unexpected '{}'", c),
        }
    }
}

pub struct LocatedToken<'a> {
    pub token: Token<'a>,
    pub position: Position,
}

impl Display for LocatedToken<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.token, self.position)
    }
}
