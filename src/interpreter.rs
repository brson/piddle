use crate::compiler::{Code, CompiledFunction};

use crate::ast;

use std::collections::HashMap;

#[derive(thiserror::Error, Debug)]
#[error("running")]
pub enum RunError {
}

#[derive(Default)]
pub struct Environment {
    args: Vec<Evaluation>,
    values: HashMap<ast::Name, Evaluation>,
}

pub struct Tables<'compiler, Context> {
    pub ctxt: Context,
    pub fns: &'compiler HashMap<ast::Name, CompiledFunction>,
    pub dump: &'compiler dyn Fn(),
    pub switch_tables: &'compiler dyn for <'a> Fn(&Tables<'a, Context>, &ast::Name) -> Tables<'a, Context>,
}

#[derive(Debug, Clone)]
pub enum Evaluation {
    Nil,
    IntrinsicInt32(i32),
    Composite {
        fields: Vec<(ast::Name, Evaluation)>,
    }
}

pub fn run_expression<Context>(env: &mut Environment, tables: &Tables<'_, Context>, expr: Code) -> Result<Evaluation, RunError> {
    match expr {
        Code::Nop => Ok(Evaluation::Nil),
        Code::Clear => Ok(Evaluation::Nil),
        Code::Dump => {
            (tables.dump)();
            println!("# runtime");
            println!("## values");
            for (name, value) in &env.values {
                todo!()
                //println!("{}: {}", name, value);
            }
            Ok(Evaluation::Nil)
        }
        Code::IntrinsicLiteralInt32(i) => Ok(Evaluation::IntrinsicInt32(i)),
        Code::IntrinsicCallInt32WrappingAdd(a, b) => {
            let eval_a = env.values.remove(&a).expect("eval");
            let eval_b = env.values.remove(&b).expect("eval");
            let val_a = match eval_a {
                Evaluation::IntrinsicInt32(a) => a,
                _ => panic!(),
            };
            let val_b = match eval_b {
                Evaluation::IntrinsicInt32(a) => a,
                _ => panic!(),
            };
            let res = val_a.wrapping_add(val_b);
            Ok(Evaluation::IntrinsicInt32(res))
        }
        Code::Set(name, code) => {
            let eval = run_expression(env, tables, *code)?;
            env.values.insert(name, eval);
            Ok(Evaluation::Nil)
        }
        Code::PopArg { name } => {
            let eval = env.args.pop().expect("pop arg");
            env.values.insert(name, eval);
            Ok(Evaluation::Nil)
        }
        Code::Read(name) => {
            let eval = if let Some(eval) = env.values.get(&name) {
                eval
            } else {
                todo!()
                //panic!("reading empty name {}", name);
            };
            let eval = eval.clone();
            Ok(eval)
        }
        Code::Call { name, args } => {
            let mut eval_args = vec![];
            for arg in args {
                let eval = run_expression(env, tables, arg)?;
                eval_args.push(eval);
            }
            let mut env = Environment {
                args: eval_args,
                .. Environment::default()
            };

            let fn_ = &tables.fns[&name];
            let tables = (tables.switch_tables)(tables, &name);
            let mut final_eval = Evaluation::Nil;
            for code in &fn_.codes {
                let code = code.clone();
                final_eval = run_expression(&mut env, &tables, code)?;
            }
            Ok(final_eval)
        }
        Code::Composite { fields } => {
            let mut evals = vec![];
            for field in fields {
                let eval = run_expression(env, &tables, *field.1)?;
                evals.push((field.0, eval));
            }
            Ok(Evaluation::Composite {
                fields: evals
            })
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
            Evaluation::Composite { fields } => {
                writeln!(f, "{{ ");
                for field in fields {
                    todo!()
                    //writeln!(f, "    {}: {}", field.0, field.1);
                }
                write!(f, "}}")
            }
        }
    }
}
