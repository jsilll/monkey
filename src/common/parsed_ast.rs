use crate::common::operator::Operator;
use crate::common::position::Position;

type Identifier = String;

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral {
        value: i64,
        position: Position,
    },
    Lvalue {
        id: Identifier,
        position: Position,
    },
    Binary {
        op: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Return { value: Expression },
    Let { id: Identifier, value: Expression },
}

#[derive(Debug)]
pub enum TopLvlStatement {
    Let { id: Identifier, value: Expression },
    Fn { id: Identifier, body: Vec<Statement> },
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<TopLvlStatement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}