use crate::compiler::{self, Code};

use std::collections::HashMap;

#[derive(thiserror::Error, Debug)]
#[error("running")]
pub enum RunError {
}

#[derive(Default)]
pub struct Environment {
    args: Vec<Evaluation>,
    values: HashMap<String, Evaluation>,
}

pub struct Tables<'compiler> {
    pub fns: &'compiler HashMap<String, compiler::CompiledFunction>,
}

#[derive(Debug, Clone)]
pub enum Evaluation {
    Nil,
    IntrinsicInt32(i32),
}

pub fn run_expression(env: &mut Environment, tables: &Tables<'_>, expr: Code) -> Result<Evaluation, RunError> {
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
            let eval = run_expression(env, tables, *code)?;
            env.values.insert(name, eval);
            Ok(Evaluation::Nil)
        }
        Code::SetArg { name, arg_no } => {
            let eval = env.args.remove(arg_no);
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
            let fn_ = &tables.fns[&name];
            let mut eval_args = vec![];
            for arg in args {
                let eval = run_expression(env, tables, arg)?;
                eval_args.push(eval);
            }
            let mut env = Environment {
                args: eval_args,
                .. Environment::default()
            };
            let mut final_eval = Evaluation::Nil;
            for code in &fn_.codes {
                let code = code.clone();
                final_eval = run_expression(&mut env, tables, code)?;
            }
            Ok(final_eval)
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
