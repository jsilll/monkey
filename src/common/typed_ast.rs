#[derive(Debug)]
pub struct File {
    pub statements: Vec<bool>,
}

impl File {
    pub fn new() -> Self {
        File {
            statements: Vec::new(),
        }
    }
}