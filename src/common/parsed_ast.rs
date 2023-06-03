use crate::common::position::Position;
use crate::common::operator::Operator;

type Identifier = String;

#[derive(Debug)]
pub enum Expression {
    Integer{ value: i64, position: Position },
    Lvalue { id: Identifier, position: Position },
    Binary { op: Operator, lhs: Box<Expression>, rhs: Box<Expression>, position: Position },
}

#[derive(Debug)]
pub enum Statement {
    Let {
        id: Identifier,
        value: Expression,
        position: Position,
    },
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program { statements: Vec::new() }
    }
}