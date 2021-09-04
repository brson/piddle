#[derive(Debug, Clone)]
pub struct Module {
    pub decls: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub struct Script {
    pub exprs: Vec<ExpressionHandle>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Struct(Struct),
    Require(Require),
    Import(Import),
    ImportAll(ImportAll),
    Function(Function),
}

#[derive(Debug, Clone, Copy)]
pub struct ExpressionHandle(pub hecs::Entity);

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: ExpressionKind,
    pub type_: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    IntrinsicCall(IntrinsicCall),
    IntrinsicLiteral(IntrinsicLiteral),
    Set(Set),
    Name(Name),
    Struct(Struct),
    Make(Make),
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
    Int32WrappingAdd(Name, Name),
}

#[derive(Debug, Clone)]
pub enum IntrinsicLiteral {
    Int32(String),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name {
    pub symbol: string_interner::DefaultSymbol,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub name: Name,
    pub type_: Type,
    pub expr: ExpressionHandle,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Name,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Name,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Name(TypeName),
    Intrinsic(TypeIntrinsic),
}

#[derive(Debug, Clone)]
pub struct TypeName(pub Name);

#[derive(Debug, Clone)]
pub enum TypeIntrinsic {
    Nil,
    Int32,
}

#[derive(Debug, Clone)]
pub struct Make {
    pub name: Name,
    pub fields: Vec<StructFieldInitializer>,
}

#[derive(Debug, Clone)]
pub struct StructFieldInitializer {
    pub name: Name,
    pub value: ExpressionHandle,
}

#[derive(Debug, Clone)]
pub struct Require {
    pub module: ModuleId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleId {
    pub group: Name,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module: ModuleId,
    pub item: Name,
}

#[derive(Debug, Clone)]
pub struct ImportAll {
    pub module: ModuleId,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Name,
    pub args: Vec<Argument>,
    pub return_type: Type,
    pub exprs: Vec<ExpressionHandle>,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Name,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub name: Name,
    pub args: Vec<ExpressionHandle>,
}

impl From<string_interner::DefaultSymbol> for Name {
    fn from(s: string_interner::DefaultSymbol) -> Name {
        Name {
            symbol: s,
        }
    }
}
