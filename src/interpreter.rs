use crate::compiler::Code;

use std::collections::HashMap;

#[derive(thiserror::Error, Debug)]
#[error("running")]
pub enum RunError {
}

#[derive(Default)]
pub struct Environment {
    arguments: Vec<Evaluation>,
    values: HashMap<String, Evaluation>,
}

#[derive(Debug, Clone)]
pub enum Evaluation {
    Nil,
    IntrinsicInt32(i32),
}

pub fn run_expression(env: &mut Environment, expr: Code) -> Result<Evaluation, RunError> {
    match expr {
        Code::Nop => Ok(Evaluation::Nil),
        Code::Clear => Ok(Evaluation::Nil),
        Code::Dump => {
            println!("# runtime");
            println!("## values");
            for (name, value) in &env.values {
                println!("{}: {}", name, value);
            }
            Ok(Evaluation::Nil)
        }
        Code::IntrinsicLiteralInt32(i) => Ok(Evaluation::IntrinsicInt32(i)),
        Code::Set(name, code) => {
            let eval = run_expression(env, *code)?;
            env.values.insert(name, eval);
            Ok(Evaluation::Nil)
        }
        Code::SetArg { name, arg_no } => {
            let eval = env.arguments.remove(arg_no);
            env.values.insert(name, eval);
            Ok(Evaluation::Nil)
        }
        Code::Read(name) => {
            let eval = if let Some(eval) = env.values.get(&name) {
                eval
            } else {
                panic!("reading empty name {}", name);
            };
            let eval = eval.clone();
            Ok(eval)
        }
        Code::Call { name, args } => {
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
