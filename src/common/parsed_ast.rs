use std::fmt::{Display, Formatter};

use crate::common::operators::{BinOp, UnOp};
use crate::common::types::Type;
use crate::common::Position;

pub type Identifier = String;

#[derive(Debug)]
pub enum Expression {
    BooleanLiteral {
        value: bool,
        position: Position,
    },
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
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Block,
        otherwise: Block,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Lvalue { id, .. } => write!(f, "{}", id),
            Expression::IntegerLiteral { value, .. } => write!(f, "{}", value),
            Expression::BooleanLiteral { value, .. } => write!(f, "{}", value),
            Expression::Unary { op, rhs } => write!(f, "({} {})", op, rhs),
            Expression::Binary { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expression::Call {
                function : _,
                arguments: _,
            } => unimplemented!(),
            Expression::If {
                condition : _,
                consequence : _,
                otherwise : _,
            } => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub enum InnerStatement {
    Return(Expression),
    Expression(Expression),
    Var { id: Identifier, value: Expression },
    Let { id: Identifier, value: Expression },
}

impl Display for InnerStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InnerStatement::Expression(expr) => write!(f, "{};", expr),
            InnerStatement::Return(expr) => write!(f, "return {};", expr),
            InnerStatement::Var { id, value } => write!(f, "var {} = {};", id, value),
            InnerStatement::Let { id, value } => write!(f, "let {} = {};", id, value),
        }
    }
}

pub type Block = Vec<InnerStatement>;

pub type Param = (Identifier, Type);

#[derive(Debug)]
pub enum TopStatement {
    Let {
        id: Identifier,
        value: Expression,
    },
    Fn {
        id: Identifier,
        rtype: Type,
        params: Vec<Param>,
        body: Block,
    },
}

#[derive(Debug)]
pub struct File {
    pub statements: Vec<TopStatement>,
}

impl File {
    pub fn new() -> Self {
        File {
            statements: Vec::new(),
        }
    }
}
