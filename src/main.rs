use anyhow::Result;
use thiserror::Error;
use std::error::Error;
use rustyline::Editor;
use rustyline::error::ReadlineError;

mod parser;

use parser::Expression;

fn main() {
    let mut readline = Editor::<()>::new();

    loop {
        match read_eval(&mut readline) {
            Ok(expr) => {
                eprintln!("ok: {:?}", expr);
            },
            Err(ReadEvalError::Readline(err @ ReadlineError::Eof)) |
            Err(ReadEvalError::Readline(err @ ReadlineError::Interrupted)) => {
                eprintln!("{}", err);
                return
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

#[derive(Error, Debug)]
enum ReadEvalError {
    #[error("reading line input")]
    Readline(#[from] rustyline::error::ReadlineError),
    #[error("parsing input")]
    Nom(#[from] nom::Err<nom::error::Error<String>>),
}

fn read_eval(readline: &mut Editor::<()>) -> Result<Evaluation, ReadEvalError> {
    let expr = read_expression(readline)?;
    eprintln!("expr: {:?}", expr);
    let expr = compile_expression(expr)?;
    eprintln!("code: {:?}", expr);
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
    use parser::Intrinsic;

    match expr {
        Expression::Intrinsic(Intrinsic::Nop) => {
            Ok(Code::Nop)
        },
        Expression::Require(parser::Require { group, module }) => {
            Ok(Code::Require(Require {
                group, module,
            }))
        },
    }
}

fn run_expression(expr: Code) -> Result<Evaluation, ReadEvalError> {
    match expr {
        Code::Nop => Ok(Evaluation::None),
        Code::Require(_) => todo!(),
    }
}

#[derive(Debug)]
enum Code {
    Nop,
    Require(Require),
}

#[derive(Debug, Clone)]
pub struct Require {
    pub group: String,
    pub module: String,
}

#[derive(Debug)]
enum Evaluation {
    None,
}
