use nom::{
    IResult,
    error::Error,
    branch::{
        alt,
    },
    bytes::complete::{
        tag,
    },
    character::complete::{
        alpha1,
        alphanumeric1,
        multispace0,
        multispace1,
        one_of,
        char,
    },
    combinator::{
        map,
        value,
        recognize,
    },
    multi::{
        many0,
        many1,
        separated_list0,
    },
    sequence::{
        pair,
        terminated,
        delimited,
        preceded,
        tuple,
    },
};

#[derive(Debug, Clone)]
pub struct Module {
    pub decls: Vec<Declaration>,
}

pub fn module(input: &str) -> IResult<&str, Module> {
    let(input, decls) = many0(delimited(
        multispace0,
        declaration,
        multispace0
    ))(input)?;
    Ok((input, Module {
        decls,
    }))
}

pub fn declaration(input: &str) -> IResult<&str, Declaration> {
    let (input, decl) = alt((
        map(struct_, Declaration::Struct),
        map(require, Declaration::Require),
        map(import, Declaration::Import),
        map(function, Declaration::Function),
    ))(input)?;
    Ok((input, decl))
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

pub fn expr(input: &str) -> IResult<&str, Expression> {
    let (input, expr) = alt((
        map(intrinsic_call, Expression::IntrinsicCall),
        map(intrinsic_literal, Expression::IntrinsicLiteral),
        map(set, Expression::Set),
        map(struct_, Expression::Struct),
        map(require, Expression::Require),
        map(import, Expression::Import),
        map(import_all, Expression::ImportAll),
        map(function, Expression::Function),
        map(call, Expression::Call),
        map(name, Expression::Name),
    ))(input)?;
    Ok((input, expr))
}

#[derive(Debug, Clone)]
pub enum IntrinsicCall {
    Nop,
    Clear,
    Dump,
    Int32WrappingAdd(String, String),
}

fn intrinsic_call(input: &str) -> IResult<&str, IntrinsicCall> {
    let (input, _) = tag("icall")(input)?;
    let (input, _) = multispace1(input)?;
    let (input , intrinsic) = alt((
        value(IntrinsicCall::Nop, tag("nop")),
        value(IntrinsicCall::Clear, tag("clear")),
        value(IntrinsicCall::Dump, tag("dump")),
        map(intrinsic2("int32_wrapping_add"), |(a, b)| IntrinsicCall::Int32WrappingAdd(a, b)),
    ))(input)?;

    Ok((input, intrinsic))
}

fn intrinsic2<'a>(name: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, (String, String)>
{
    preceded(
        tag(name),
        tuple((
            preceded(
                multispace1,
                map(identifier, ToString::to_string),
            ),
            preceded(
                multispace1,
                map(identifier, ToString::to_string),
            ),
        ))
    )
}

#[derive(Debug, Clone)]
pub enum IntrinsicLiteral {
    Int32(String),
}

fn intrinsic_literal(input: &str) -> IResult<&str, IntrinsicLiteral> {
    let (input, lit) = decimal(input)?;
    let lit = IntrinsicLiteral::Int32(lit.to_string());
    Ok((input, lit))
}

#[derive(Debug, Clone)]
pub struct Set {
    pub name: String,
    pub type_: Type,
    pub expr: Box<Expression>,
}

fn set(input: &str) -> IResult<&str, Set> {
    let (input, _) = tag("set")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = multispace1(input)?;
    let (input, type_) = type_(input)?;
    let (input, _) = multispace1(input)?;
    let (input, expr) = map(expr, Box::new)(input)?;

    Ok((input, Set {
        name, type_, expr,
    }))
}

fn name(input: &str) -> IResult<&str, String> {
    map(identifier, ToString::to_string)(input)
}

#[derive(Debug, Clone)]
pub struct Struct {
    name: String,
    fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    name: String,
    type_: Type,
}

fn struct_(input: &str) -> IResult<&str, Struct> {
    let (input, _) = tag("struct")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fields) = separated_list0(separator, struct_field)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Struct {
        name, fields
    }))
}

fn struct_field(input: &str) -> IResult<&str, StructField> {
    let (input, (name, type_)) = name_type(input)?;
    Ok((input, StructField {
        name, type_,
    }))
}

#[derive(Debug, Clone)]
pub enum Type {
    Name(TypeName),
    Intrinsic(TypeIntrinsic),
}

#[derive(Debug, Clone)]
pub struct TypeName(String);

#[derive(Debug, Clone)]
pub enum TypeIntrinsic {
    Nil,
    Int32,
}

fn type_(input: &str) -> IResult<&str, Type> {
    let (input, type_) = alt((
        map(type_intrinsic, Type::Intrinsic),
        map(type_name, Type::Name),
    ))(input)?;

    Ok((input, type_))
}

fn type_intrinsic(input: &str) -> IResult<&str, TypeIntrinsic> {
    let (input, _) = tag("itype")(input)?;
    let (input, _) = multispace1(input)?;
    let (input , intrinsic) = alt((
        value(TypeIntrinsic::Nil, tag("nil")),
        value(TypeIntrinsic::Int32, tag("int32")),
    ))(input)?;

    Ok((input, intrinsic))
}

fn type_name(input: &str) -> IResult<&str, TypeName> {
    let (input, name) = identifier(input)?;
    Ok((input, TypeName(name.to_string())))
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

fn require(input: &str) -> IResult<&str, Require> {
    let (input, _) = tag("require")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = map(identifier, ToString::to_string)(input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, Require {
        module,
    }))
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module: ModuleId,
    pub item: String,
}

fn import(input: &str) -> IResult<&str, Import> {
    let (input, _) = tag("import")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = multispace1(input)?;
    let (input, item) = map(identifier, ToString::to_string)(input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, Import {
        module, item,
    }))
}

#[derive(Debug, Clone)]
pub struct ImportAll {
    pub module: ModuleId,
}

fn import_all(input: &str) -> IResult<&str, ImportAll> {
    let (input, _) = tag("importall")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = map(identifier, ToString::to_string)(input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, ImportAll {
        module,
    }))
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

fn function(input: &str) -> IResult<&str, Function> {
    let (input, _) = tag("fn")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, args) = separated_list0(separator, argument)(input)?;
    let (input, _) = tag(")")(input)?;

    let (input, _) = multispace0(input)?;
    let (input, _) = tag("->")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, return_type) = type_(input)?;

    let (input, _) = multispace0(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, exprs) = separated_list0(separator, expr)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Function {
        name, args, return_type, exprs,
    }))
}

fn argument(input: &str) -> IResult<&str, Argument> {
    let (input, (name, type_)) = name_type(input)?;
    Ok((input, Argument {
        name, type_,
    }))
}

#[derive(Debug, Clone)]
pub struct Call {
    pub name: String,
    pub args: Vec<Expression>,
}

fn call(input: &str) -> IResult<&str, Call> {
    let (input, _) = tag("call")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, args) = separated_list0(separator, expr)(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, Call {
        name, args,
    }))
}


/* -------------- */

fn name_type(input: &str) -> IResult<&str, (String, Type)> {
    let (input, name) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, type_) = type_(input)?;

    Ok((input, (name, type_)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )
    )(input)
}

fn decimal(input: &str) -> IResult<&str, &str> {
    recognize(
        many1(
            terminated(one_of("0123456789"), many0(char('_')))
        )
    )(input)
}

fn separator(input: &str) -> IResult<&str, ()> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, ()))
}
