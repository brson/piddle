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
    IntrinsicCallInt32WrappingAdd(String, String),
    Set(String, Box<Code>),
    PopArg {
        name: String,
    },
    Read(String),
    Call {
        name: String,
        args: Vec<Code>,
    },
    Dump,
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

pub fn compile_expression(compiler: &mut Compiler, module: &parser::ModuleId, expr: Expression) -> Result<Code, CompileError> {
    match expr {
        Expression::IntrinsicCall(parser::IntrinsicCall::Nop) => {
            Ok(Code::Nop)
        },
        Expression::IntrinsicCall(parser::IntrinsicCall::Clear) => {
            Ok(Code::Clear)
        }
        Expression::IntrinsicCall(parser::IntrinsicCall::Dump) => {
            Ok(Code::Dump)
        }
        Expression::IntrinsicCall(parser::IntrinsicCall::Int32WrappingAdd(a, b)) => {
            Ok(Code::IntrinsicCallInt32WrappingAdd(a, b))
        }
        Expression::Require(parser::Require { module, .. }) => {
            require::load(compiler, &module)?;
            Ok(Code::Nop)
        },
        Expression::Import(parser::Import { module: from_module, item }) => {
            let mut from_module_ctxt = compiler.modules.get_mut(&from_module)
                .ok_or_else(|| CompileError::UnknownImportModule(from_module.group.clone(), from_module.module.clone()))?;
            let from_module_ast = &from_module_ctxt.ast;
            let mut found_item = None;
            for decl in &from_module_ast.decls {
                match decl {
                    parser::Declaration::Function(parser::Function { name, .. }) => {
                        if *name == item {
                            found_item = Some(decl.clone());
                            break;
                        }
                    }
                    _ => { }
                }
            }
            if let Some(decl) = found_item {
                match decl {
                    parser::Declaration::Function(fn_) => {
                        let name = fn_.name.clone();
                        from_module_ctxt.fn_asts.insert(name.clone(), fn_);
                        drop(from_module_ctxt);
                        let mut module_ctxt = compiler.modules.get_mut(module).expect("module");
                        module_ctxt.fn_imports.insert(name, from_module.clone());
                    }
                    _ => todo!()
                }
            } else {
                return Err(CompileError::ItemNotFoundInModule(from_module.group.clone(), from_module.module.clone(), item.clone()));
            }
            Ok(Code::Nop)
        }
        Expression::ImportAll(parser::ImportAll { module: from_module }) => {
            let mut from_module_ctxt = compiler.modules.get_mut(&from_module)
                .ok_or_else(|| CompileError::UnknownImportModule(from_module.group.clone(), from_module.module.clone()))?;
            let from_module_ast = from_module_ctxt.ast.clone();
            drop(from_module_ctxt);
            for decl in &from_module_ast.decls {
                match decl {
                    parser::Declaration::Function(fn_) => {
                        let name = fn_.name.clone();
                        let mut from_module_ctxt = compiler.modules.get_mut(&from_module).expect("module");
                        from_module_ctxt.fn_asts.insert(name.clone(), fn_.clone());
                        drop(from_module_ctxt);
                        let mut module_ctxt = compiler.modules.get_mut(module).expect("module");
                        module_ctxt.fn_imports.insert(name, from_module.clone());
                    }
                    _ => { /* todo */ }
                }
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
            let expr = compile_expression(compiler, module, *expr)?;
            Ok(Code::Set(name, Box::new(expr)))
        },
        Expression::Name(name) => {
            Ok(Code::Read(name))
        },
        Expression::Function(function) => {
            let mut module_ctxt = compiler.modules.get_mut(module).expect("module");
            let name = function.name.clone();
            assert!(!module_ctxt.fn_asts.contains_key(&name));
            module_ctxt.fn_asts.insert(name.clone(), function);
            Ok(Code::Nop)
        },
        Expression::Call(call) => {
            compile_function(compiler, module, &call.name)?;
            let mut code_args = vec![];
            for arg in call.args {
                let code_arg = compile_expression(compiler, module, arg)?;
                code_args.push(code_arg);
            }
            Ok(Code::Call {
                name: call.name,
                args: code_args,
            })
        },
    }
}

fn compile_function(compiler: &mut Compiler, module: &parser::ModuleId, name: &str) -> Result<(), CompileError> {
    let mut module_ctxt = compiler.modules.get_mut(module).expect("module");

    if module_ctxt.fns.contains_key(name) {
        return Ok(());
    }

    if module_ctxt.fn_asts.contains_key(name) {

        let ast = &module_ctxt.fn_asts[name];
        let ast = ast.clone();

        let mut codes = vec![];

        for arg in ast.args.into_iter().rev() {
            codes.push(Code::PopArg { name: arg.name });
        }

        drop(module_ctxt);

        for expr in ast.exprs {
            let code = compile_expression(compiler, module, expr)?;
            codes.push(code);
        }

        let compiled = CompiledFunction {
            codes
        };

        let mut module_ctxt = compiler.modules.get_mut(module).expect("module");
        module_ctxt.fns.insert(name.to_string(), compiled);
    }

    let mut module_ctxt = compiler.modules.get_mut(module).expect("module");

    // If the name is imported from another module
    if module_ctxt.fn_imports.contains_key(name) {
        let other_module = module_ctxt.fn_imports[name].clone();
        drop(module_ctxt);

        let other_module_ctxt = compiler.modules.get_mut(&other_module).expect("module");

        // See if the external function has already been compiled
        if other_module_ctxt.fns.contains_key(name) {
            let compiled_fn = other_module_ctxt.fns[name].clone();
            drop(other_module_ctxt);
            let module_ctxt = compiler.modules.get_mut(module).expect("module");
            module_ctxt.fns.insert(name.to_string(), compiled_fn);
            return Ok(());
        }

        let ast = &other_module_ctxt.fn_asts[name];
        let ast = ast.clone();

        let mut codes = vec![];

        for arg in ast.args.into_iter().rev() {
            codes.push(Code::PopArg { name: arg.name });
        }

        drop(other_module_ctxt);

        for expr in ast.exprs {
            let code = compile_expression(compiler, &other_module, expr)?;
            codes.push(code);
        }

        let compiled = CompiledFunction {
            codes
        };

        let mut other_module_ctxt = compiler.modules.get_mut(&other_module).expect("module");
        other_module_ctxt.fns.insert(name.to_string(), compiled.clone());
        drop(other_module_ctxt);
        let mut module_ctxt = compiler.modules.get_mut(module).expect("module");
        module_ctxt.fns.insert(name.to_string(), compiled);
    }

    Ok(())
}

#[derive(Clone)]
pub struct CompiledFunction {
    pub codes: Vec<Code>,
}

pub struct Compiler {
    pub modules: HashMap<parser::ModuleId, ModuleContext>,
}

pub struct ModuleContext {
    ast: parser::Module,
    fn_asts: HashMap<String, parser::Function>,
    pub fns: HashMap<String, CompiledFunction>,
    fn_imports: HashMap<String, parser::ModuleId>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            modules: HashMap::new(),
        }
    }

    pub fn have_module(&self, module: &parser::ModuleId) -> bool {
        self.modules.contains_key(&module)
    }

    pub fn add_module(&mut self, module: &parser::ModuleId, ast: parser::Module) {
        assert!(!self.have_module(module));

        let ctxt = ModuleContext {
            ast,
            fn_asts: HashMap::new(),
            fns: HashMap::new(),
            fn_imports: HashMap::new(),
        };

        self.modules.insert(module.clone(), ctxt);
    }
}

pub fn dump(compiler: &Compiler) {
    println!("# compiler");
    println!("## modules");
    for (module, context) in &compiler.modules {
        println!("- {}/{}", module.group, module.module);
        if !context.fn_asts.is_empty() {
            println!("  - local functions");
            for name in context.fn_asts.keys() {
                let compiled = context.fns.contains_key(name);
                if compiled {
                    println!("    - {} (compiled)", name);
                } else {
                    println!("    - {}", name);
                }
            }
        }
        if !context.fn_imports.is_empty() {
            println!("  - imported functions");
            for (name, module) in &context.fn_imports {
                let compiled = context.fns.contains_key(name);
                if compiled {
                    println!("    - {} (from {}/{}) (compiled)", name, module.group, module.module);
                } else {
                    println!("    - {} (from {}/{})", name, module.group, module.module);
                }
            }
        }
    }
}
