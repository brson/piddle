use anyhow::Result;
use std::error::Error;
use rustyline::Editor;
use rustyline::error::ReadlineError;

mod parser;
mod compiler;
mod interpreter;

use parser::Expression;
use compiler::Code;
use interpreter::Evaluation;

fn main() {
    let mut readline = Editor::<()>::new();

    loop {
        match read_eval(&mut readline) {
            Ok(expr) => {
                eprintln!("ok: {}", expr);
            },
            Err(ReadEvalError::Readline(err @ ReadlineError::Eof)) |
            Err(ReadEvalError::Readline(err @ ReadlineError::Interrupted)) => {
                eprintln!("{}", err);
                return
            },
            Err(err) => {
                eprintln!("{:?}", err);
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
}

fn read_eval(readline: &mut Editor::<()>) -> Result<Evaluation, ReadEvalError> {
    let expr = read_expression(readline)?;
    let expr = compile_expression(expr)?;
    let expr = run_expression(expr)?;

    Ok(expr)
}

fn read_expression(readline: &mut Editor::<()>) -> Result<Expression, ReadEvalError> {
    let line = readline.readline("> ")?;

    readline.add_history_entry(line.clone());

    let (_, expr) = parser::expr(&line)
        .map_err(|e| e.map(|e| nom::error::Error::new(e.input.to_string(), e.code)))?;

    Ok(expr)
}

fn compile_expression(expr: Expression) -> Result<Code, ReadEvalError> {
    Ok(compiler::compile_expression(expr)?)
}

fn run_expression(expr: Code) -> Result<Evaluation, ReadEvalError> {
    Ok(interpreter::run_expression(expr)?)
}
