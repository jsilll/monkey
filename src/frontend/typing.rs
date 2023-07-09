use crate::common::parsed_ast::File;
use crate::common::typed_ast::File as TypedFile;

pub struct TypeChecker {
    file: File,
}

impl TypeChecker {
    pub fn new(file: File) -> Self {
        TypeChecker { file }
    }

    pub fn check(self) -> Result<TypedFile, String> {
        Ok(TypedFile::new())
    }
}
