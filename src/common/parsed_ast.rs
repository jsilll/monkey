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
    If {
        condition: Box<Expression>,
        consequence: Block,
        otherwise: Option<Block>,
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
            Expression::If {
                condition,
                consequence,
                otherwise,
            } => {
                write!(f, "if {} {{", condition)?;
                for statement in consequence {
                    writeln!(f, " {}", statement)?;
                }
                write!(f, "}}")?;
                if let Some(otherwise) = otherwise {
                    write!(f, " else {{")?;
                    for statement in otherwise {
                        writeln!(f, " {}", statement)?;
                    }
                    write!(f, "}}")?;
                }
                Ok(())
            }
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
    Let {
        id: Identifier,
        value: Expression,
    },
    Fn {
        id: Identifier,
        rtype: Type,
        params: Vec<(Identifier, Type)>,
        body: Block,
    },
}

impl Display for TopStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopStatement::Let { id, value } => writeln!(f, "let {} = {};", id, value),
            TopStatement::Fn {
                rtype,
                body,
                id,
                params,
            } => {
                write!(
                    f,
                    "fn {}({}) -> {} {{",
                    id,
                    params
                        .iter()
                        .map(|(id, t)| format!("{}: {}", id, t))
                        .collect::<Vec<String>>()
                        .join(", "),
                    rtype
                )?;
                for statement in body {
                    writeln!(f, " {}", statement)?;
                }
                write!(f, "}}")
            }
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
