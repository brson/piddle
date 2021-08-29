use crate::parser;
use crate::parser::Expression;
use crate::require;

use std::collections::HashMap;

#[derive(Debug)]
pub enum Code {
    Nop,
    Clear,
    IntrinsicLiteralInt32(i32),
    Set(String, Box<Code>),
    Read(String),
    Call {
        name: String,
        args: Vec<Code>,
    },
    Dump,
}

#[derive(Debug, Clone)]
pub struct Require {
    pub group: String,
    pub module: String,
}

#[derive(thiserror::Error, Debug)]
pub enum CompileError {
    #[error("parsing integer")]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("loading module")]
    Require(#[from] require::Error),
}

pub fn compile_expression(compiler: &mut Compiler, expr: Expression) -> Result<Code, CompileError> {
    match expr {
        Expression::IntrinsicCall(parser::IntrinsicCall::Nop) => {
            Ok(Code::Nop)
        },
        Expression::IntrinsicCall(parser::IntrinsicCall::Clear) => {
            Ok(Code::Clear)
        }
        Expression::IntrinsicCall(parser::IntrinsicCall::Dump) => {
            println!("# compiler");
            println!("## function asts");
            for fn_ast in compiler.fn_asts.values() {
                println!("{}", fn_ast.name);
            }
            Ok(Code::Dump)
        }
        Expression::Require(parser::Require { group, module }) => {
            require::load(compiler, &group, &module)?;
            Ok(Code::Nop)
        },
        Expression::IntrinsicLiteral(parser::IntrinsicLiteral::Int32(v)) => {
            let i = v.parse()?;
            Ok(Code::IntrinsicLiteralInt32(i))
        },
        Expression::Struct(_) => {
            todo!()
        },
        Expression::Set(parser::Set { name, type_, expr }) => {
            let expr = compile_expression(compiler, *expr)?;
            Ok(Code::Set(name, Box::new(expr)))
        },
        Expression::Name(name) => {
            Ok(Code::Read(name))
        },
        Expression::Function(function) => {
            let name = function.name.clone();
            assert!(!compiler.fn_asts.contains_key(&name));
            compiler.fn_asts.insert(name.clone(), function);
            Ok(Code::Nop)
        },
        Expression::Call(call) => {
            compile_function(compiler, &call.name)?;
            let mut code_args = vec![];
            for arg in call.args {
                let code_arg = compile_expression(compiler, arg)?;
                code_args.push(code_arg);
            }
            Ok(Code::Call {
                name: call.name,
                args: code_args,
            })
        },
    }
}

fn compile_function(compiler: &mut Compiler, name: &str) -> Result<(), CompileError> {
    if compiler.fns.contains_key(name) {
        return Ok(());
    }

    assert!(compiler.fn_asts.contains_key(name));

    todo!()
}

pub struct CompiledFunction {
    code: Vec<Code>,
}

pub struct Compiler {
    fn_asts: HashMap<String, parser::Function>,
    fns: HashMap<String, CompiledFunction>,
    module_asts: HashMap<(String, String), parser::Module>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            fn_asts: HashMap::new(),
            fns: HashMap::new(),
            module_asts: HashMap::new(),
        }
    }

    pub fn have_module(&self, group: &str, module: &str) -> bool {
        self.module_asts.contains_key(&(group.to_string(), module.to_string()))
    }

    pub fn add_module(&mut self, group: &str, module: &str, ast: parser::Module) {
        assert!(!self.have_module(group, module));

        self.module_asts.insert((group.to_string(), module.to_string()), ast);
    }
}

