use nom::{
    IResult,
    bytes::streaming::{
        tag,
    },
    branch::alt,
    combinator::value,
};

#[derive(Debug)]
pub enum Expression {
    Intrinsic(Intrinsic),
}

#[derive(Debug)]
#[derive(Clone)]
pub enum Intrinsic {
    Nop,
}

pub fn expr(input: &str) -> IResult<&str, Expression> {
    let (input, intrinsic) = intrinsic(input)?;
    Ok((input, Expression::Intrinsic(intrinsic)))
}

fn intrinsic(input: &str) -> IResult<&str, Intrinsic> {
    let (input, _) = tag("intr")(input)?;
    let (input , intrinsic) = alt((
        value(Intrinsic::Nop, tag("nop")),
        value(Intrinsic::Nop, tag("nop2")),
    ))(input)?;

    Ok((input, intrinsic))
}
