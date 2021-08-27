use crate::compiler::Compiler;

#[derive(thiserror::Error)]
#[derive(Debug)]
#[error("require")]
pub struct Error {
}

pub fn load(compiler: &mut Compiler, group: &str, module: &str) -> Result<(), Error> {
    todo!()
}
