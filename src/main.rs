use anyhow::Result;
use std::error::Error;
use rustyline::Editor;
use rustyline::error::ReadlineError;

mod parser;
mod compiler;

use parser::Expression;
use compiler::Code;

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
    match expr {
        Code::Nop => Ok(Evaluation::Nil),
        Code::Clear => Ok(Evaluation::Nil),
        Code::Require(_) => todo!(),
        Code::LiteralInteger(i) => Ok(Evaluation::Integer(i)),
    }
}

#[derive(Debug)]
enum Evaluation {
    Nil,
    Integer(i32),
}

impl std::fmt::Display for Evaluation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Evaluation::Nil => {
                write!(f, "Nil")
            },
            Evaluation::Integer(i) => {
                write!(f, "{}", i)
            },
        }
    }
}
