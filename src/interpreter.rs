use crate::compiler::Code;

use std::collections::HashMap;

#[derive(thiserror::Error, Debug)]
#[error("running")]
pub enum RunError {
}

#[derive(Default)]
pub struct Environment {
    values: HashMap<String, Evaluation>,
}

#[derive(Debug)]
pub enum Evaluation {
    Nil,
    IntrinsicInt32(i32),
}

pub fn run_expression(env: &mut Environment, expr: Code) -> Result<Evaluation, RunError> {
    match expr {
        Code::Nop => Ok(Evaluation::Nil),
        Code::Clear => Ok(Evaluation::Nil),
        Code::Require(_) => todo!(),
        Code::IntrinsicLiteralInt32(i) => Ok(Evaluation::IntrinsicInt32(i)),
        Code::Set(name, code) => {
            let eval = run_expression(env, *code)?;
            env.values.insert(name, eval);
            Ok(Evaluation::Nil)
        }
        Code::Read(name) => {
            let eval = env.values.get(&name);
            todo!()
        }
    }
}

impl std::fmt::Display for Evaluation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Evaluation::Nil => {
                write!(f, "Nil")
            },
            Evaluation::IntrinsicInt32(i) => {
                write!(f, "{}", i)
            },
        }
    }
}
