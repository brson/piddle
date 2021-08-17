use crate::parser;
use crate::parser::Expression;

#[derive(Debug)]
pub enum Code {
    Nop,
    Clear,
    Require(Require),
    LiteralInteger(i32),
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
        Expression::Intrinsic(parser::Intrinsic::Nop) => {
            Ok(Code::Nop)
        },
        Expression::Intrinsic(parser::Intrinsic::Clear) => {
            Ok(Code::Clear)
        }
        Expression::Require(parser::Require { group, module }) => {
            Ok(Code::Require(Require {
                group, module,
            }))
        },
        Expression::Literal(parser::Literal::Integer(v)) => {
            let i = v.parse()?;
            Ok(Code::LiteralInteger(i))
        },
        Expression::Struct(_) => {
            todo!()
        },
    }
}
