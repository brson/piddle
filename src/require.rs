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
    #[error("compiling")]
    Compile(#[from] Box<crate::compiler::CompileError>),
}

pub fn load(compiler: &mut Compiler, module: &parser::ModuleId) -> Result<(), Error> {
    if compiler.have_module(module) {
        return Ok(());
    }

    let ast = read_ast(module)?;
    let decls = ast.decls.clone();

    compiler.add_module(module, ast);

    for decl in decls {
        match decl {
            parser::Declaration::Require(parser::Require { module }) => {
                // fixme recursion
                load(compiler, &module)?;
            }
            parser::Declaration::Import(parser::Import { module: from_module, item }) => {
                crate::compiler::import(compiler, module, &from_module, &item).map_err(Box::new)?;
            }
            parser::Declaration::ImportAll(parser::ImportAll { module: from_module }) => {
                crate::compiler::import_all(compiler, module, &from_module).map_err(Box::new)?;
            }
            _ => { /* pass */ }
        }
    }

    Ok(())
}

fn read_ast(module: &parser::ModuleId) -> Result<parser::Module, Error> {
    let group = &module.group;
    let module = &module.module;
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
