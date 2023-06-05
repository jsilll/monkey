use crate::common::operator::{BinOp, UnOp};
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
    Unary {
        op: UnOp,
        rhs: Box<Expression>,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum InnerStatement {
    Return(Expression),
    Expression(Expression),
    Var { id: Identifier, value: Expression },
    Let { id: Identifier, value: Expression },
}

pub type Block = Vec<InnerStatement>;

#[derive(Debug)]
pub enum TopStatement {
    Fn { id: Identifier, body: Block },
    Let { id: Identifier, value: Expression },
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
