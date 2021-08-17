use nom::{
    IResult,
    branch::{
        alt,
    },
    bytes::streaming::{
        tag,
    },
    character::streaming::{
        alpha1,
        alphanumeric1,
        multispace1,
    },
    combinator::{
        map,
        value,
        recognize,
    },
    multi::{
        many0,
    },
    sequence::{
        pair,
    },
};

#[derive(Debug)]
pub enum Expression {
    Intrinsic(Intrinsic),
    Require(Require),
}

pub fn expr(input: &str) -> IResult<&str, Expression> {
    let (input, intrinsic) = intrinsic(input)?;
    Ok((input, Expression::Intrinsic(intrinsic)))
}

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Nop,
}

fn intrinsic(input: &str) -> IResult<&str, Intrinsic> {
    let (input, _) = tag("intr")(input)?;
    let (input, _) = multispace1(input)?;
    let (input , intrinsic) = alt((
        value(Intrinsic::Nop, tag("nop")),
        value(Intrinsic::Nop, tag("nop2")),
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


/* -------------- */

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )
    )(input)
}
