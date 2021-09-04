#![allow(unused)]

use lazy_static::lazy_static;
use std::error::Error;
use rustyline::Editor;
use rustyline::error::ReadlineError;

use std::panic::{self, AssertUnwindSafe};
use std::fs;

mod ast;
mod parser;
mod compiler;
mod interpreter;

mod require;

use ast::ExpressionHandle;
use compiler::{Compiler, Code};
use interpreter::{Environment, Evaluation};

static REPL_HISTORY_FILE: &str = "repl-history";

fn main() -> anyhow::Result<()> {

    let script = std::env::args().skip(1).next();

    if let Some(script) = script {
        run_script(&script)
    } else {
        run_repl()
    }
}

fn run_script(script_file: &str) -> anyhow::Result<()> {
    let mut compiler = Compiler::new();
    let repl_module = repl_module(&mut compiler.strings);
    compiler.add_module(&repl_module, ast::Module {
        decls: vec![],
    });

    let mut env = Environment::default();

    let text = fs::read_to_string(script_file)?;
    let mut parser_ctxt = parser::Context {
        strings: &mut compiler.strings,
        world: &mut compiler.world,
    };
    let (_, script) = parser::script(&mut parser_ctxt, &text)
        .map_err(|e| e.map(|e| nom::error::Error::new(e.input.to_string(), e.code)))?;

    for expr in script.exprs {
        let code = compile_expression(&mut compiler, expr)?;
        let _ = run_expression(&mut compiler, &mut env, code)?;
    }

    Ok(())
}

fn run_repl() -> anyhow::Result<()> {
    let mut compiler = Compiler::new();
    let repl_module = repl_module(&mut compiler.strings);
    compiler.add_module(&repl_module, ast::Module {
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
        let expr = read_expression(compiler, readline)?;
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

fn read_expression(compiler: &mut Compiler, readline: &mut Editor::<()>) -> Result<ExpressionHandle, ReadEvalError> {
    let line = readline.readline("> ")?;

    readline.add_history_entry(line.clone());
    readline.append_history(REPL_HISTORY_FILE)?;

    let mut parser_ctxt = parser::Context {
        strings: &mut compiler.strings,
        world: &mut compiler.world,
    };
    let (_, expr) = parser::expr(&mut parser_ctxt, &line)
        .map_err(|e| e.map(|e| nom::error::Error::new(e.input.to_string(), e.code)))?;

    Ok(expr)
}

fn compile_expression(compiler: &mut Compiler, expr: ExpressionHandle) -> Result<Code, ReadEvalError> {
    let repl_module = repl_module(&mut compiler.strings);
    Ok(compiler::compile_expression(compiler, &repl_module, expr)?)
}

fn run_expression(compiler: &mut Compiler, env: &mut Environment, expr: Code) -> Result<Evaluation, ReadEvalError> {

    let repl_module = repl_module(&mut compiler.strings);
    let module_ctxt = &compiler.modules[&repl_module];
    let tables = interpreter::Tables::<Context> {
        ctxt: (compiler, module_ctxt),
        fns: &module_ctxt.fns,
        dump: &|| compiler::dump(compiler),
        switch_tables: &switch_tables,
        name: &|symbol| compiler.name(symbol),
    };

    type Context<'b> = (&'b compiler::Compiler, &'b compiler::ModuleContext);

    fn switch_tables<'compiler, 'b>(tables: &interpreter::Tables<'compiler, Context<'b>>, name: &ast::Name) -> interpreter::Tables<'compiler, Context<'b>> {
        let module = tables.ctxt.1.fn_imports.get(name);
        if let Some(module) = module {
            let module_ctxt = &tables.ctxt.0.modules[module];
            interpreter::Tables {
                ctxt: (tables.ctxt.0, module_ctxt),
                fns: &module_ctxt.fns,
                dump: tables.dump,
                switch_tables: tables.switch_tables,
                name: tables.name,
            }
        } else {
            interpreter::Tables {
                ctxt: tables.ctxt,
                fns: tables.fns,
                dump: tables.dump,
                switch_tables: tables.switch_tables,
                name: tables.name,
            }
        }
    }

    Ok(interpreter::run_expression(env, &tables, expr)?)
}

fn repl_module(strings: &mut string_interner::StringInterner) -> ast::ModuleId {
    ast::ModuleId {
        group: ast::Name::from(strings.get_or_intern("local")),
        module: ast::Name::from(strings.get_or_intern("main")),
    }
}

