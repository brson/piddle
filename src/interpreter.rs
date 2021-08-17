use crate::compiler::Code;

#[derive(thiserror::Error, Debug)]
#[error("running")]
pub enum RunError {
}

pub fn run_expression(expr: Code) -> Result<Evaluation, RunError> {
    match expr {
        Code::Nop => Ok(Evaluation::Nil),
        Code::Clear => Ok(Evaluation::Nil),
        Code::Require(_) => todo!(),
        Code::LiteralInt32(i) => Ok(Evaluation::Int32(i)),
    }
}

#[derive(Debug)]
pub enum Evaluation {
    Nil,
    Int32(i32),
}

impl std::fmt::Display for Evaluation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Evaluation::Nil => {
                write!(f, "Nil")
            },
            Evaluation::Int32(i) => {
                write!(f, "{}", i)
            },
        }
    }
}
