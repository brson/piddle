use crate::parser;
use crate::parser::Expression;
use crate::require;

#[derive(Debug)]
pub enum Code {
    Nop,
    Clear,
    IntrinsicLiteralInt32(i32),
    Set(String, Box<Code>),
    Read(String),
}

#[derive(Debug, Clone)]
pub struct Require {
    pub group: String,
    pub module: String,
}

#[derive(thiserror::Error, Debug)]
pub enum CompileError {
    #[error("parsing integer")]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("loading module")]
    Require(#[from] require::Error),
}

pub fn compile_expression(compiler: &mut Compiler, expr: Expression) -> Result<Code, CompileError> {
    match expr {
        Expression::IntrinsicCall(parser::IntrinsicCall::Nop) => {
            Ok(Code::Nop)
        },
        Expression::IntrinsicCall(parser::IntrinsicCall::Clear) => {
            Ok(Code::Clear)
        }
        Expression::Require(parser::Require { group, module }) => {
            require::load(compiler, &group, &module)?;
            Ok(Code::Nop)
        },
        Expression::IntrinsicLiteral(parser::IntrinsicLiteral::Int32(v)) => {
            let i = v.parse()?;
            Ok(Code::IntrinsicLiteralInt32(i))
        },
        Expression::Struct(_) => {
            todo!()
        },
        Expression::Set(parser::Set { name, type_, expr }) => {
            let expr = compile_expression(compiler, *expr)?;
            Ok(Code::Set(name, Box::new(expr)))
        },
        Expression::Name(name) => {
            Ok(Code::Read(name))
        },
        Expression::Function(_) => {
            todo!()
        },
    }
}

pub struct Compiler {
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
        }
    }

    pub fn have_module(&self, group: &str, module: &str) -> bool {
        todo!()
    }

    pub fn add_module(&mut self, group: &str, module: &str, ast: parser::Module) {
        assert!(!self.have_module(group, module));
        todo!()
    }
}

