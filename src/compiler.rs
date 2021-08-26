use crate::parser;
use crate::parser::Expression;

#[derive(Debug)]
pub enum Code {
    Nop,
    Clear,
    Require(Require),
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
}

pub fn compile_expression(expr: Expression) -> Result<Code, CompileError> {
    match expr {
        Expression::IntrinsicCall(parser::IntrinsicCall::Nop) => {
            Ok(Code::Nop)
        },
        Expression::IntrinsicCall(parser::IntrinsicCall::Clear) => {
            Ok(Code::Clear)
        }
        Expression::Require(parser::Require { group, module }) => {
            Ok(Code::Require(Require {
                group, module,
            }))
        },
        Expression::IntrinsicLiteral(parser::IntrinsicLiteral::Int32(v)) => {
            let i = v.parse()?;
            Ok(Code::IntrinsicLiteralInt32(i))
        },
        Expression::Struct(_) => {
            todo!()
        },
        Expression::Set(parser::Set { name, type_, expr }) => {
            let expr = compile_expression(*expr)?;
            Ok(Code::Set(name, Box::new(expr)))
        },
        Expression::Name(name) => {
            Ok(Code::Read(name))
        },
    }
}
