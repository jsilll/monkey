use std::fmt::{Display, Formatter};

use crate::common::operator::{BinOp, UnOp};
use crate::common::position::Position;

type Identifier = String;

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
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::BooleanLiteral { value, .. } => write!(f, "{}", value),
            Expression::IntegerLiteral { value, .. } => write!(f, "{}", value),
            Expression::Lvalue { id, .. } => write!(f, "{}", id),
            Expression::Unary { op, rhs } => write!(f, "({} {})", op, rhs),
            Expression::Binary { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
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
            InnerStatement::Return(expr) => write!(f, "return {};", expr),
            InnerStatement::Expression(expr) => write!(f, "{};", expr),
            InnerStatement::Var { id, value } => write!(f, "var {} = {};", id, value),
            InnerStatement::Let { id, value } => write!(f, "let {} = {};", id, value),
        }
    }
}

pub type Block = Vec<InnerStatement>;

#[derive(Debug)]
pub enum TopStatement {
    Fn { id: Identifier, body: Block },
    Let { id: Identifier, value: Expression },
}

impl Display for TopStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopStatement::Fn { id, body } => {
                writeln!(f, "fn {}() {{", id)?;
                for statement in body {
                    writeln!(f, " {}", statement)?;
                }
                writeln!(f, "}}")
            }
            TopStatement::Let { id, value } => writeln!(f, "let {} = {};", id, value),
        }
    }
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

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}
