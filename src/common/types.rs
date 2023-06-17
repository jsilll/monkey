use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Type {
    Bool,
    Int64,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Int64 => write!(f, "i64"),
        }
    }
}
