use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum UnOp {
    Bang,
    Minus,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Bang => write!(f, "!"),
            UnOp::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
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
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Plus => write!(f, "+"),
            BinOp::Minus => write!(f, "-"),
            BinOp::Star => write!(f, "*"),
            BinOp::Slash => write!(f, "/"),

            BinOp::Eq => write!(f, "=="),
            BinOp::Neq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Gt => write!(f, ">"),
            BinOp::Lte => write!(f, "<="),
            BinOp::Gte => write!(f, ">="),
        }
    }
}