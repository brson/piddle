#![allow(unused)]

use lazy_static::lazy_static;
use std::error::Error;
use rustyline::Editor;
use rustyline::error::ReadlineError;

use std::panic::{self, AssertUnwindSafe};

mod parser;
mod compiler;
mod interpreter;

mod require;

use parser::Expression;
use compiler::{Compiler, Code};
use interpreter::{Environment, Evaluation};

static REPL_HISTORY_FILE: &str = "repl-history";

lazy_static! {
    static ref REPL_MODULE: parser::ModuleId = parser::ModuleId {
        group: "local".to_string(),
        module: "main".to_string(),
    };
}

fn main() -> anyhow::Result<()> {
    let mut compiler = Compiler::new();
    let mut env = Environment::default();
    let mut readline = Editor::<()>::new();

    let res = readline.load_history(REPL_HISTORY_FILE);
    match res {
        Ok(_) => {},
        Err(ReadlineError::Io(e)) if e.kind() == std::io::ErrorKind::NotFound => {
        },
        Err(e) => Err(e)?
    }

    loop {
        match read_eval(&mut compiler, &mut env, &mut readline) {
            Ok(expr) => {
                eprintln!("ok: {}", expr);
            },
            Err(ReadEvalError::Readline(err @ ReadlineError::Eof)) |
            Err(ReadEvalError::Readline(err @ ReadlineError::Interrupted)) => {
                eprintln!("{}", err);
                return Ok(());
            },
            Err(err) => {
                eprintln!("error: {}", err);

                let mut source = err.source();
                while let Some(cause) = source {
                    eprintln!("cause: {}", cause);
                    source = cause.source();
                }
            }
        }
    }
}

#[derive(thiserror::Error, Debug)]
enum ReadEvalError {
    #[error("reading line input")]
    Readline(#[from] rustyline::error::ReadlineError),
    #[error("parsing input")]
    Nom(#[from] nom::Err<nom::error::Error<String>>),
    #[error("compiling")]
    Compile(#[from] compiler::CompileError),
    #[error("running")]
    Run(#[from] interpreter::RunError),
    #[error("runtime panic")]
    RuntimePanic,
}

fn read_eval(compiler: &mut Compiler, env: &mut Environment, readline: &mut Editor::<()>) -> Result<Evaluation, ReadEvalError> {
    let res = panic::catch_unwind(AssertUnwindSafe(|| {
        let expr = read_expression(readline)?;
        let expr = compile_expression(compiler, expr)?;
        let expr = run_expression(compiler, env, expr)?;

        Ok(expr)
    }));

    match res {
        Ok(res) => res,
        Err(e) => {
            Err(ReadEvalError::RuntimePanic)
        }
    }
}

fn read_expression(readline: &mut Editor::<()>) -> Result<Expression, ReadEvalError> {
    let line = readline.readline("> ")?;

    readline.add_history_entry(line.clone());
    readline.append_history(REPL_HISTORY_FILE)?;

    let (_, expr) = parser::expr(&line)
        .map_err(|e| e.map(|e| nom::error::Error::new(e.input.to_string(), e.code)))?;

    Ok(expr)
}

fn compile_expression(compiler: &mut Compiler, expr: Expression) -> Result<Code, ReadEvalError> {
    Ok(compiler::compile_expression(compiler, expr)?)
}

fn run_expression(compiler: &mut Compiler, env: &mut Environment, expr: Code) -> Result<Evaluation, ReadEvalError> {
    let tables = interpreter::Tables {
        fns: &compiler.fns,
    };
    Ok(interpreter::run_expression(env, &tables, expr)?)
}
