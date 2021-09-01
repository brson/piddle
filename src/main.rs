#![allow(unused)]

use lazy_static::lazy_static;
use std::error::Error;
use rustyline::Editor;
use rustyline::error::ReadlineError;

use std::panic::{self, AssertUnwindSafe};

mod ast;
mod parser;
mod compiler;
mod interpreter;

mod require;

use ast::Expression;
use compiler::{Compiler, Code};
use interpreter::{Environment, Evaluation};

static REPL_HISTORY_FILE: &str = "repl-history";

lazy_static! {
    static ref REPL_MODULE: ast::ModuleId = ast::ModuleId {
        group: ast::Name::from("local"),
        module: ast::Name::from("main"),
    };
}

fn main() -> anyhow::Result<()> {
    let mut compiler = Compiler::new();
    compiler.add_module(&REPL_MODULE, ast::Module {
        decls: vec![],
    });

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
    Ok(compiler::compile_expression(compiler, &REPL_MODULE, expr)?)
}

fn run_expression(compiler: &mut Compiler, env: &mut Environment, expr: Code) -> Result<Evaluation, ReadEvalError> {
    let module_ctxt = &compiler.modules[&REPL_MODULE];
    let switch_tables: &dyn for <'a> Fn(&interpreter::Tables<'a, _>, &ast::Name) -> interpreter::Tables<'a, _>
        = &|tables: &interpreter::Tables<'_, (&'_ compiler::ModuleContext, &'_ compiler::Compiler)>, name: &ast::Name| -> interpreter::Tables<'_, _>
    {
        let module = tables.ctxt.0.fn_imports.get(name);
        if let Some(module) = module {
            let module_ctxt = &tables.ctxt.1.modules[module];
            interpreter::Tables {
                ctxt: (module_ctxt, tables.ctxt.1),
                fns: &module_ctxt.fns,
                dump: tables.dump,
                switch_tables: tables.switch_tables,
            }
        } else {
            interpreter::Tables {
                ctxt: tables.ctxt,
                fns: tables.fns,
                dump: tables.dump,
                switch_tables: tables.switch_tables,
            }
        }
    };
    let tables = interpreter::Tables::<(&compiler::ModuleContext, &compiler::Compiler)> {
        ctxt: (module_ctxt, compiler),
        fns: &module_ctxt.fns,
        dump: &|| compiler::dump(compiler),
        switch_tables,
    };
    Ok(interpreter::run_expression(env, &tables, expr)?)
}
