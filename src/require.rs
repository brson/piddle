use crate::compiler::Compiler;

use crate::parser;

use std::fs;
use std::path::PathBuf;

#[derive(thiserror::Error)]
#[derive(Debug)]
pub enum Error {
    #[error("module {0}/{1} not found")]
    NotFound(String, String),
    #[error("I/O error")]
    Io(#[from] std::io::Error),
}

pub fn load(compiler: &mut Compiler, group: &str, module: &str) -> Result<(), Error> {
    let path = format!("./lib/{}/{}.piddle", group, module);
    let path = PathBuf::from(path);
    
    if !fs::metadata(&path).is_ok() {
        return Err(Error::NotFound(group.to_string(), module.to_string()));
    }

    let text = fs::read_to_string(&path)?;

    todo!()
}
