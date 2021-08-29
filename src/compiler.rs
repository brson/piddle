use crate::parser;
use crate::parser::Expression;
use crate::require;

use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub enum Code {
    Nop,
    Clear,
    IntrinsicLiteralInt32(i32),
    Set(String, Box<Code>),
    SetArg {
        name: String,
        arg_no: usize,
    },
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
    #[error("unknown module in import, {0}/{1}")]
    UnknownImportModule(String, String),
    #[error("item {2} not found in module {0}/{1}")]
    ItemNotFoundInModule(String, String, String),
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
            for ast in compiler.fn_asts.values() {
                println!("{}", ast.name);
            }
            println!("## module asts");
            for (group, module) in compiler.module_asts.keys() {
                println!("{}/{}", group, module);
            }
            Ok(Code::Dump)
        }
        Expression::Require(parser::Require { group, module }) => {
            require::load(compiler, &group, &module)?;
            Ok(Code::Nop)
        },
        Expression::Import(parser::Import { group, module, item }) => {
            let module_ast = compiler.module_asts.get(&(group.clone(), module.clone()))
                .ok_or_else(|| CompileError::UnknownImportModule(group.clone(), module.clone()))?;
            let mut found_item = None;
            for decl in &module_ast.decls {
                match decl {
                    parser::Declaration::Function(parser::Function { name, .. }) => {
                        if *name == item {
                            found_item = Some(decl.clone());
                            break;
                        }
                    }
                    _ => todo!()
                }
            }
            if let Some(decl) = found_item {
                match decl {
                    parser::Declaration::Function(fn_) => {
                        let name = fn_.name.clone();
                        compiler.fn_asts.insert(name, fn_);
                    }
                    _ => todo!()
                }
            } else {
                return Err(CompileError::ItemNotFoundInModule(group.clone(), module.clone(), item.clone()));
            }
            Ok(Code::Nop)
        }
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

    let ast = &compiler.fn_asts[name];
    let ast = ast.clone();

    let mut codes = vec![];

    for (i, arg) in ast.args.into_iter().enumerate() {
        codes.push(Code::SetArg { name: arg.name, arg_no: i });
    }

    for expr in ast.exprs {
        let code = compile_expression(compiler, expr)?;
        codes.push(code);
    }

    let compiled = CompiledFunction {
        codes
    };

    compiler.fns.insert(name.to_string(), compiled);

    Ok(())
}

pub struct CompiledFunction {
    pub codes: Vec<Code>,
}

pub struct Compiler {
    fn_asts: HashMap<String, parser::Function>,
    pub fns: HashMap<String, CompiledFunction>,
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

