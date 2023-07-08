use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Type {
    Bool,
    Int32,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Int32 => write!(f, "i32"),
        }
    }
}
