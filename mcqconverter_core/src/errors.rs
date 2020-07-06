use nom::error::{ParseError, ErrorKind};
use nom::lib::std::fmt::Debug;
use nom::{Slice, IResult};

use std::ops::RangeTo;

pub trait McqConverterError<T>: ParseError<T> + Debug
{}

impl<__ : ?Sized, T> McqConverterError<T> for __ where Self: ParseError<T> + Debug
{}

pub fn inv_cut<I: Clone + Slice<RangeTo<usize>>, O, E: ParseError<I>, F>(parser: F) -> impl Fn(I) -> IResult<I, O, E>
where
    F: Fn(I) -> IResult<I, O, E>,
{
    move |input: I| {
        let i = input.clone();
        match parser(i) {
            Err(nom::Err::Failure(e)) => Err(nom::Err::Error(e)),
            rest => rest,
        }
    }
}

#[allow(dead_code)]
pub fn context_error<'a, E: McqConverterError<&'a str>>(i: &'a str, msg: &'static str, kind: ErrorKind) -> nom::Err<E>
{
    nom::Err::Error(ParseError::add_context(
        i,
        msg,
        ParseError::from_error_kind(i, kind)
    ))
}

#[allow(dead_code)]
pub fn context_failure<'a, E: McqConverterError<&'a str>>(i: &'a str, msg: &'static str, kind: ErrorKind) -> nom::Err<E>
{
    nom::Err::Failure(ParseError::add_context(
        i,
        msg,
        ParseError::from_error_kind(i, kind)
    ))
}

#[allow(dead_code)]
pub fn nom_error<'a, I: 'a, E: McqConverterError<I>>(i: I, kind: ErrorKind) -> nom::Err<E> {
    nom::Err::Error(ParseError::from_error_kind(i, kind))
}

#[allow(dead_code)]
pub fn nom_failure<'a, I: 'a, E: McqConverterError<I>>(i: I, kind: ErrorKind) -> nom::Err<E> {
    nom::Err::Failure(ParseError::from_error_kind(i, kind))
}
