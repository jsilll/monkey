use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Operator {
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
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),

            Operator::Eq => write!(f, "=="),
            Operator::Neq => write!(f, "!="),
            Operator::Lt => write!(f, "<"),
            Operator::Gt => write!(f, ">"),
            Operator::Lte => write!(f, "<="),
            Operator::Gte => write!(f, ">="),

            Operator::Bang => write!(f, "!"),
        }
    }
}