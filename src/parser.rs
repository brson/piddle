use nom::{
    IResult,
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
    },
};

#[derive(Debug)]
pub enum Expression {
    IntrinsicCall(IntrinsicCall),
    IntrinsicLiteral(IntrinsicLiteral),
    Set(Set),
    Struct(Struct),
    Require(Require),
}

pub fn expr(input: &str) -> IResult<&str, Expression> {
    let (_, expr) = alt((
        map(intrinsic_call, Expression::IntrinsicCall),
        map(intrinsic_literal, Expression::IntrinsicLiteral),
        map(set, Expression::Set),
        map(struct_, Expression::Struct),
        map(require, Expression::Require),
    ))(input)?;
    Ok((input, expr))
}

#[derive(Debug, Clone)]
pub enum IntrinsicCall {
    Nop,
    Clear,
//    Int32WrappingAdd(String, String),
}

fn intrinsic_call(input: &str) -> IResult<&str, IntrinsicCall> {
    let (input, _) = tag("icall")(input)?;
    let (input, _) = multispace1(input)?;
    let (input , intrinsic) = alt((
        value(IntrinsicCall::Nop, tag("nop")),
        value(IntrinsicCall::Clear, tag("clear")),
        
    ))(input)?;

    Ok((input, intrinsic))
}

#[derive(Debug)]
pub enum IntrinsicLiteral {
    Int32(String),
}

fn intrinsic_literal(input: &str) -> IResult<&str, IntrinsicLiteral> {
    let (input, lit) = decimal(input)?;
    let lit = IntrinsicLiteral::Int32(lit.to_string());
    Ok((input, lit))
}

#[derive(Debug)]
pub struct Set {
    name: String,
    type_: Type,
    expr: Box<Expression>,
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

#[derive(Debug)]
pub struct Struct {
    name: String,
    fields: Vec<StructField>,
}

#[derive(Debug)]
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
    let (input, fields) = separated_list0(tag(","), struct_field)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Struct {
        name, fields
    }))
}

fn struct_field(input: &str) -> IResult<&str, StructField> {
    let (input, name) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, type_) = type_(input)?;

    Ok((input, StructField {
        name, type_,
    }))
}

#[derive(Debug)]
pub enum Type {
    Name(TypeName),
    Intrinsic(TypeIntrinsic),
}

#[derive(Debug)]
pub struct TypeName(String);

#[derive(Debug, Clone)]
pub enum TypeIntrinsic {
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
        value(TypeIntrinsic::Int32, tag("int32")),
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
    pub group: String,
    pub module: String,
}

fn require(input: &str) -> IResult<&str, Require> {
    let (input, _) = tag("require")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = map(identifier, ToString::to_string)(input)?;
    let (input, _) = tag("/")(input)?;
    let (input, module) = map(identifier, ToString::to_string)(input)?;

    Ok((input, Require {
        group, module,
    }))
}



/* -------------- */

pub fn identifier(input: &str) -> IResult<&str, &str> {
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
