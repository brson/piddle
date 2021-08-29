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
    #[error("parsing module")]
    Nom(#[from] nom::Err<nom::error::Error<String>>),
}

pub fn load(compiler: &mut Compiler, group: &str, module: &str) -> Result<(), Error> {
    if compiler.have_module(group, module) {
        return Ok(());
    }

    let ast = read_ast(group, module)?;
    println!("{:?}", ast);
    let decls = ast.decls.clone();

    compiler.add_module(group, module, ast);

    for decl in decls {
        // todo
    }

    Ok(())
}

fn read_ast(group: &str, module: &str) -> Result<parser::Module, Error> {
    let path = format!("./lib/{}/{}.piddle", group, module);
    let path = PathBuf::from(path);
    
    if !fs::metadata(&path).is_ok() {
        return Err(Error::NotFound(group.to_string(), module.to_string()));
    }

    let text = fs::read_to_string(&path)?;

    let (_, module) = parser::module(&text)
        .map_err(|e| e.map(|e| nom::error::Error::new(e.input.to_string(), e.code)))?;

    Ok(module)
}
