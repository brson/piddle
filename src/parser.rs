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
        success,
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

use hecs::World;
use string_interner::StringInterner;
use string_interner::DefaultSymbol as Symbol;

use crate::ast::*;

pub struct Context<'c> {
    pub world: &'c mut World,
    pub strings: &'c mut StringInterner,
}

pub fn module<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, Module> {
    let(input, decls) = many0(delimited(
        multispace0,
        |input| declaration(world, input),
        multispace0
    ))(input)?;
    Ok((input, Module {
        decls,
    }))
}

pub fn script<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, Script> {
    let (input, exprs) = many0(
        preceded(
            multispace0,
            |input| expr(world, input),
        )
    )(input)?;
    Ok((input, Script {
        exprs
    }))
}

pub fn declaration<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, Declaration> {
    let (input, decl) = alt((
        map(struct_, Declaration::Struct),
        map(require, Declaration::Require),
        map(import, Declaration::Import),
        map(|input| function(world, input), Declaration::Function),
    ))(input)?;
    Ok((input, decl))
}

pub fn expr<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, Expression> {
    use std::cell::RefCell;
    let world = RefCell::new(world);
    let (input, expr) = alt((
        map(intrinsic_call, ExpressionKind::IntrinsicCall),
        map(intrinsic_literal, ExpressionKind::IntrinsicLiteral),
        map(|input| set(*world.borrow_mut(), input), ExpressionKind::Set),
        map(struct_, ExpressionKind::Struct),
        map(|input| make(*world.borrow_mut(), input), ExpressionKind::Make),
        map(require, ExpressionKind::Require),
        map(import, ExpressionKind::Import),
        map(import_all, ExpressionKind::ImportAll),
        map(|input| function(*world.borrow_mut(), input), ExpressionKind::Function),
        map(|input| call(*world.borrow_mut(), input), ExpressionKind::Call),
        map(name, ExpressionKind::Name),
    ))(input)?;

    let (input, _) = multispace0(input)?;

    let (input, type_) = alt((
        map(preceded(
            pair(tag(":"), multispace0),
            type_,
        ), Some),
        map(tag(""), |_| None),
    ))(input)?;

    let expr = Expression {
        expr, type_: None,
    };
    Ok((input, expr))
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

fn intrinsic2<'a>(name_: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, (Name, Name)>
{
    preceded(
        tag(name_),
        tuple((
            preceded(
                multispace1,
                name,
            ),
            preceded(
                multispace1,
                name,
            ),
        ))
    )
}

fn intrinsic_literal(input: &str) -> IResult<&str, IntrinsicLiteral> {
    let (input, lit) = decimal(input)?;
    let lit = IntrinsicLiteral::Int32(lit.to_string());
    Ok((input, lit))
}

fn set<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, Set> {
    let (input, _) = tag("set")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, type_) = type_(input)?;
    let (input, _) = multispace1(input)?;
    let (input, expr) = map(|input| expr(world, input), Box::new)(input)?;

    Ok((input, Set {
        name, type_, expr,
    }))
}

fn name(input: &str) -> IResult<&str, Name> {
    map(identifier, |n| Name { inner: n.to_string() })(input)
}

fn struct_(input: &str) -> IResult<&str, Struct> {
    let (input, _) = tag("struct")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(input)?;
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
    let (input, name) = name(input)?;
    Ok((input, TypeName(name)))
}

fn make<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, Make> {
    let (input, _) = tag("make")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fields) = separated_list0(separator, |input| struct_field_initializer(world, input))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Make {
        name, fields
    }))
}

fn struct_field_initializer<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, StructFieldInitializer> {
    let (input, (name, value)) = name_value(world, input)?;
    Ok((input, StructFieldInitializer {
        name, value,
    }))
}

fn require(input: &str) -> IResult<&str, Require> {
    let (input, _) = tag("require")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = name(input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = name(input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, Require {
        module,
    }))
}

fn import(input: &str) -> IResult<&str, Import> {
    let (input, _) = tag("import")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = name(input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = name(input)?;
    let (input, _) = multispace1(input)?;
    let (input, item) = name(input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, Import {
        module, item,
    }))
}

fn import_all(input: &str) -> IResult<&str, ImportAll> {
    let (input, _) = tag("importall")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = name(input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = name(input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, ImportAll {
        module,
    }))
}

fn function<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, Function> {
    let (input, _) = tag("fn")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(input)?;
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
    let (input, exprs) = separated_list0(separator, |input| expr(world, input))(input)?;
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

fn call<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, Call> {
    let (input, _) = tag("call")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, args) = separated_list0(separator, |input| expr(world, input))(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, Call {
        name, args,
    }))
}


/* -------------- */

fn name_type(input: &str) -> IResult<&str, (Name, Type)> {
    let (input, name) = name(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, type_) = type_(input)?;

    Ok((input, (name, type_)))
}

fn name_value<'i>(world: &mut World, input: &'i str) -> IResult<&'i str, (Name, Expression)> {
    let (input, name) = name(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expr) = expr(world, input)?;

    Ok((input, (name, expr)))
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
