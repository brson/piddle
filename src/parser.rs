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

use std::cell::RefCell;
use hecs::World;
use string_interner::StringInterner;
use string_interner::DefaultSymbol as Symbol;

use crate::ast::*;

pub struct Context<'c> {
    pub strings: &'c mut StringInterner,
    pub world: &'c mut World,
}

pub fn module<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Module> {
    let(input, decls) = many0(delimited(
        multispace0,
        |input| declaration(ctxt, input),
        multispace0
    ))(input)?;
    Ok((input, Module {
        decls,
    }))
}

pub fn script<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Script> {
    let (input, exprs) = many0(
        preceded(
            multispace0,
            |input| expr(ctxt, input),
        )
    )(input)?;
    Ok((input, Script {
        exprs
    }))
}

pub fn declaration<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Declaration> {
    let ctxt = RefCell::new(ctxt);
    let (input, decl) = alt((
        map(|input| struct_(*ctxt.borrow_mut(), input), Declaration::Struct),
        map(|input| require(*ctxt.borrow_mut(), input), Declaration::Require),
        map(|input| import(*ctxt.borrow_mut(), input), Declaration::Import),
        map(|input| function(*ctxt.borrow_mut(), input), Declaration::Function),
    ))(input)?;
    Ok((input, decl))
}

pub fn expr<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Expression> {
    let ctxt = RefCell::new(ctxt);
    let (input, expr) = alt((
        map(|input| intrinsic_call(*ctxt.borrow_mut(), input), ExpressionKind::IntrinsicCall),
        map(intrinsic_literal, ExpressionKind::IntrinsicLiteral),
        map(|input| set(*ctxt.borrow_mut(), input), ExpressionKind::Set),
        map(|input| struct_(*ctxt.borrow_mut(), input), ExpressionKind::Struct),
        map(|input| make(*ctxt.borrow_mut(), input), ExpressionKind::Make),
        map(|input| require(*ctxt.borrow_mut(), input), ExpressionKind::Require),
        map(|input| import(*ctxt.borrow_mut(), input), ExpressionKind::Import),
        map(|input| import_all(*ctxt.borrow_mut(), input), ExpressionKind::ImportAll),
        map(|input| function(*ctxt.borrow_mut(), input), ExpressionKind::Function),
        map(|input| call(*ctxt.borrow_mut(), input), ExpressionKind::Call),
        map(|input| name(*ctxt.borrow_mut(), input), ExpressionKind::Name),
    ))(input)?;

    let (input, _) = multispace0(input)?;

    let (input, type_) = alt((
        map(preceded(
            pair(tag(":"), multispace0),
            |input| type_(*ctxt.borrow_mut(), input),
        ), Some),
        map(tag(""), |_| None),
    ))(input)?;

    let expr = Expression {
        expr, type_: None,
    };
    Ok((input, expr))
}

fn intrinsic_call<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, IntrinsicCall> {
    let (input, _) = tag("icall")(input)?;
    let (input, _) = multispace1(input)?;
    let (input , intrinsic) = alt((
        value(IntrinsicCall::Nop, tag("nop")),
        value(IntrinsicCall::Clear, tag("clear")),
        value(IntrinsicCall::Dump, tag("dump")),
        map(|input| intrinsic2(ctxt, "int32_wrapping_add", input), |(a, b)| IntrinsicCall::Int32WrappingAdd(a, b)),
    ))(input)?;

    Ok((input, intrinsic))
}

fn intrinsic2<'i>(ctxt: &mut Context, name_: &str, input: &'i str) -> IResult<&'i str, (Name, Name)> {
    let ctxt = RefCell::new(ctxt);
    let (input, names) = preceded(
        tag(name_),
        tuple((
            preceded(
                multispace1,
                |input| name(*ctxt.borrow_mut(), input),
            ),
            preceded(
                multispace1,
                |input| name(*ctxt.borrow_mut(), input),
            ),
        ))
    )(input)?;

    Ok((input, names))
}

fn intrinsic_literal(input: &str) -> IResult<&str, IntrinsicLiteral> {
    let (input, lit) = decimal(input)?;
    let lit = IntrinsicLiteral::Int32(lit.to_string());
    Ok((input, lit))
}

fn set<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Set> {
    let (input, _) = tag("set")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(ctxt, input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, type_) = type_(ctxt, input)?;
    let (input, _) = multispace1(input)?;
    let (input, expr) = map(|input| expr(ctxt, input), Box::new)(input)?;

    Ok((input, Set {
        name, type_, expr,
    }))
}

fn name<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Name> {
    map(identifier, |n| Name { inner: n.to_string() })(input)
}

fn struct_<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Struct> {
    let (input, _) = tag("struct")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(ctxt, input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fields) = separated_list0(separator, |input| struct_field(ctxt, input))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Struct {
        name, fields
    }))
}

fn struct_field<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, StructField> {
    let (input, (name, type_)) = name_type(ctxt, input)?;
    Ok((input, StructField {
        name, type_,
    }))
}

fn type_<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Type> {
    let (input, type_) = alt((
        map(type_intrinsic, Type::Intrinsic),
        map(|input| type_name(ctxt, input), Type::Name),
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

fn type_name<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, TypeName> {
    let (input, name) = name(ctxt, input)?;
    Ok((input, TypeName(name)))
}

fn make<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Make> {
    let (input, _) = tag("make")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(ctxt, input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fields) = separated_list0(separator, |input| struct_field_initializer(ctxt, input))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Make {
        name, fields
    }))
}

fn struct_field_initializer<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, StructFieldInitializer> {
    let (input, (name, value)) = name_value(ctxt, input)?;
    Ok((input, StructFieldInitializer {
        name, value,
    }))
}

fn require<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Require> {
    let (input, _) = tag("require")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = name(ctxt, input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = name(ctxt, input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, Require {
        module,
    }))
}

fn import<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Import> {
    let (input, _) = tag("import")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = name(ctxt, input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = name(ctxt, input)?;
    let (input, _) = multispace1(input)?;
    let (input, item) = name(ctxt, input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, Import {
        module, item,
    }))
}

fn import_all<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, ImportAll> {
    let (input, _) = tag("importall")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, group) = name(ctxt, input)?;
    let (input, _) = multispace1(input)?;
    let (input, module) = name(ctxt, input)?;

    let module = ModuleId {
        group, module,
    };
    Ok((input, ImportAll {
        module,
    }))
}

fn function<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Function> {
    let (input, _) = tag("fn")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(ctxt, input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, args) = separated_list0(separator, |input| argument(ctxt, input))(input)?;
    let (input, _) = tag(")")(input)?;

    let (input, _) = multispace0(input)?;
    let (input, _) = tag("->")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, return_type) = type_(ctxt, input)?;

    let (input, _) = multispace0(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, exprs) = separated_list0(separator, |input| expr(ctxt, input))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Function {
        name, args, return_type, exprs,
    }))
}

fn argument<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Argument> {
    let (input, (name, type_)) = name_type(ctxt, input)?;
    Ok((input, Argument {
        name, type_,
    }))
}

fn call<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, Call> {
    let (input, _) = tag("call")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, name) = name(ctxt, input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, args) = separated_list0(separator, |input| expr(ctxt, input))(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, Call {
        name, args,
    }))
}


/* -------------- */

fn name_type<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, (Name, Type)> {
    let (input, name) = name(ctxt, input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, type_) = type_(ctxt, input)?;

    Ok((input, (name, type_)))
}

fn name_value<'i>(ctxt: &mut Context, input: &'i str) -> IResult<&'i str, (Name, Expression)> {
    let (input, name) = name(ctxt, input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expr) = expr(ctxt, input)?;

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
