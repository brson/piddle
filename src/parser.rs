use nom::{
    IResult,
    bytes::streaming::{
        tag,
    },
};

#[derive(Debug)]
pub enum Expression {
    Intrinsic(Intrinsic),
}

#[derive(Debug)]
pub enum Intrinsic {
    Nop,
}

pub fn expr(input: &str) -> IResult<&str, Expression> {
    let (input, intrinsic) = intrinsic(input)?;
    Ok((input, Expression::Intrinsic(intrinsic)))
}

fn intrinsic(input: &str) -> IResult<&str, Intrinsic> {
    let (input, _) = tag("intr")(input)?;
    Ok((input, Intrinsic::Nop))
}
