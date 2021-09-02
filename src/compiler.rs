use crate::ast;
use crate::ast::Expression;
use crate::require;

use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub enum Code {
    Nop,
    Clear,
    IntrinsicLiteralInt32(i32),
    IntrinsicCallInt32WrappingAdd(ast::Name, ast::Name),
    Set(ast::Name, Box<Code>),
    PopArg {
        name: ast::Name,
    },
    Read(ast::Name),
    Call {
        name: ast::Name,
        args: Vec<Code>,
    },
    Dump,
    Composite {
        fields: Vec<(ast::Name, Box<Code>)>,
    },
}

#[derive(thiserror::Error, Debug)]
pub enum CompileError {
    #[error("parsing integer")]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("loading module")]
    Require(#[from] require::Error),
    #[error("unknown module in import, {0}/{1}")]
    UnknownImportModule(ast::Name, ast::Name),
    #[error("item {2} not found in module {0}/{1}")]
    ItemNotFoundInModule(ast::Name, ast::Name, ast::Name),
}

pub fn compile_expression(compiler: &mut Compiler, module: &ast::ModuleId, expr: Expression) -> Result<Code, CompileError> {
    match expr {
        Expression::IntrinsicCall(ast::IntrinsicCall::Nop) => {
            Ok(Code::Nop)
        },
        Expression::IntrinsicCall(ast::IntrinsicCall::Clear) => {
            Ok(Code::Clear)
        }
        Expression::IntrinsicCall(ast::IntrinsicCall::Dump) => {
            Ok(Code::Dump)
        }
        Expression::IntrinsicCall(ast::IntrinsicCall::Int32WrappingAdd(a, b)) => {
            Ok(Code::IntrinsicCallInt32WrappingAdd(a, b))
        }
        Expression::Require(ast::Require { module, .. }) => {
            require::load(compiler, &module)?;
            Ok(Code::Nop)
        },
        Expression::Import(ast::Import { module: from_module, item }) => {
            import(compiler, module, &from_module, &item)?;
            Ok(Code::Nop)
        }
        Expression::ImportAll(ast::ImportAll { module: from_module }) => {
            import_all(compiler, module, &from_module)?;
            Ok(Code::Nop)
        }
        Expression::IntrinsicLiteral(ast::IntrinsicLiteral::Int32(v)) => {
            let i = v.parse()?;
            Ok(Code::IntrinsicLiteralInt32(i))
        },
        Expression::Struct(ast::Struct { name, fields }) => {
            /* pass */
            Ok(Code::Nop)
        },
        Expression::Make(ast::Make { name, fields }) => {
            let fields = fields.into_iter().map(|field| {
                let expr = compile_expression(compiler, module, field.value)?;
                Ok((field.name, Box::new(expr)))
            }).collect::<Result<Vec<_>, CompileError>>()?;
            Ok(Code::Composite { fields })
        },
        Expression::Set(ast::Set { name, type_, expr }) => {
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

pub fn import(compiler: &mut Compiler, module: &ast::ModuleId,
              from_module: &ast::ModuleId, item: &ast::Name) -> Result<(), CompileError> {
    let mut from_module_ctxt = compiler.modules.get_mut(&from_module)
        .ok_or_else(|| CompileError::UnknownImportModule(from_module.group.clone(), from_module.module.clone()))?;
    let from_module_ast = &from_module_ctxt.ast;
    let mut found_item = None;
    for decl in &from_module_ast.decls {
        match decl {
            ast::Declaration::Function(ast::Function { name, .. }) => {
                if name == item {
                    found_item = Some(decl.clone());
                    break;
                }
            }
            _ => { }
        }
    }
    if let Some(decl) = found_item {
        match decl {
            ast::Declaration::Function(fn_) => {
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

    Ok(())
}

pub fn import_all(compiler: &mut Compiler, module: &ast::ModuleId,
                  from_module: &ast::ModuleId) -> Result<(), CompileError> {
    let mut from_module_ctxt = compiler.modules.get_mut(&from_module)
        .ok_or_else(|| CompileError::UnknownImportModule(from_module.group.clone(), from_module.module.clone()))?;
    let from_module_ast = from_module_ctxt.ast.clone();
    drop(from_module_ctxt);
    for decl in &from_module_ast.decls {
        match decl {
            ast::Declaration::Function(fn_) => {
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

    Ok(())
}

fn compile_function(compiler: &mut Compiler, module: &ast::ModuleId, name: &ast::Name) -> Result<(), CompileError> {
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
        module_ctxt.fns.insert(name.clone(), compiled);
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
            module_ctxt.fns.insert(name.clone(), compiled_fn);
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
        other_module_ctxt.fns.insert(name.clone(), compiled.clone());
        drop(other_module_ctxt);
        let mut module_ctxt = compiler.modules.get_mut(module).expect("module");
        module_ctxt.fns.insert(name.clone(), compiled);
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub codes: Vec<Code>,
}

pub struct Compiler {
    pub modules: HashMap<ast::ModuleId, ModuleContext>,
}

pub struct ModuleContext {
    ast: ast::Module,
    fn_asts: HashMap<ast::Name, ast::Function>,
    pub fns: HashMap<ast::Name, CompiledFunction>,
    pub fn_imports: HashMap<ast::Name, ast::ModuleId>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            modules: HashMap::new(),
        }
    }

    pub fn have_module(&self, module: &ast::ModuleId) -> bool {
        self.modules.contains_key(&module)
    }

    pub fn add_module(&mut self, module: &ast::ModuleId, ast: ast::Module) {
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
