use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Type {
    Int,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "i64"),
        }
    }
}
