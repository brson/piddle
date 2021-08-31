#[derive(Debug, Clone)]
pub struct Module {
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Struct(Struct),
    Require(Require),
    Import(Import),
    ImportAll(ImportAll),
    Function(Function),
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntrinsicCall(IntrinsicCall),
    IntrinsicLiteral(IntrinsicLiteral),
    Set(Set),
    Name(String),
    Struct(Struct),
    Require(Require),
    Import(Import),
    ImportAll(ImportAll),
    Function(Function),
    Call(Call),
}

#[derive(Debug, Clone)]
pub enum IntrinsicCall {
    Nop,
    Clear,
    Dump,
    Int32WrappingAdd(String, String),
}

#[derive(Debug, Clone)]
pub enum IntrinsicLiteral {
    Int32(String),
}

#[derive(Debug, Clone)]
pub struct Set {
    pub name: String,
    pub type_: Type,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Name(TypeName),
    Intrinsic(TypeIntrinsic),
}

#[derive(Debug, Clone)]
pub struct TypeName(pub String);

#[derive(Debug, Clone)]
pub enum TypeIntrinsic {
    Nil,
    Int32,
}

#[derive(Debug, Clone)]
pub struct Require {
    pub module: ModuleId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleId {
    pub group: String,
    pub module: String,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module: ModuleId,
    pub item: String,
}

#[derive(Debug, Clone)]
pub struct ImportAll {
    pub module: ModuleId,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<Argument>,
    pub return_type: Type,
    pub exprs: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub name: String,
    pub args: Vec<Expression>,
}



