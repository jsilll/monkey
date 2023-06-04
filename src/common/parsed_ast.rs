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
    Var { id: Identifier, value: Expression },
    Let { id: Identifier, value: Expression },
}

pub type Block = Vec<Statement>;

#[derive(Debug)]
pub enum TopStatement {
    Let { id: Identifier, value: Expression },
    Fn { id: Identifier, body: Block },
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<TopStatement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}
