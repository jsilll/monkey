use std::fmt::{Display};

#[derive(Clone, Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub fname: String,
}

impl<'i> Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.fname, self.line, self.column)
    }
}

pub mod types;

pub mod operators;

pub mod typed_ast;

pub mod parsed_ast;