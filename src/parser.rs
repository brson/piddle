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
    },
    sequence::{
        pair,
        terminated,
    },
};

#[derive(Debug)]
pub enum Expression {
    Intrinsic(Intrinsic),
    Require(Require),
    Literal(Literal),
    Struct(Struct),
}

pub fn expr(input: &str) -> IResult<&str, Expression> {
    let (_, expr) = alt((
        map(intrinsic, Expression::Intrinsic),
        map(require, Expression::Require),
        map(literal, Expression::Literal),
        map(struct_, Expression::Struct),
    ))(input)?;
    Ok((input, expr))
}

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Nop,
    Clear,
}

fn intrinsic(input: &str) -> IResult<&str, Intrinsic> {
    let (input, _) = tag("intr")(input)?;
    let (input, _) = multispace1(input)?;
    let (input , intrinsic) = alt((
        value(Intrinsic::Nop, tag("nop")),
        value(Intrinsic::Clear, tag("clear")),
    ))(input)?;

    Ok((input, intrinsic))
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

#[derive(Debug)]
pub enum Literal {
    Integer(String),
}

fn literal(input: &str) -> IResult<&str, Literal> {
    let (input, lit) = decimal(input)?;
    let lit = Literal::Integer(lit.to_string());
    Ok((input, lit))
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

#[derive(Debug)]
pub enum Type {
    Name(String),
    Intrinsic(TypeIntrinsic),
}

#[derive(Debug)]
pub enum TypeIntrinsic {
    Int32,
}

fn struct_(input: &str) -> IResult<&str, Struct> {
    todo!()
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
