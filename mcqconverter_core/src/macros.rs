#![macro_use]

use nom::IResult;
use nom::multi::many_till;
use nom::branch::alt;
use nom::combinator::{peek};
use nom::bytes::complete::{take};
use crate::errors::McqConverterError;

pub trait ParserList<'a, O, E> {
    fn parse_until_found(&self, input: &'a str, error_if_empty: bool) -> IResult<&'a str, String, E>;
}

/// Takes input until one parser succeed
///
/// # Errors
/// - If the pattern is at the start returns an Error or Ok depending on error_if_empty
/// # Failures
/// - If many_till fails (may be when encountering EOF depending on the parser)
pub fn take_until_parsers<'a, O, List, E>(error_if_empty: bool, parsers: List) -> impl Fn(&'a str) -> IResult<&'a str, String, E>
    where
        E: McqConverterError<&'a str>,
        List: ParserList<'a, O, E>,
{
    move |i: &'a str| {
        parsers.parse_until_found(i, error_if_empty)
    }
}

macro_rules! parser_list_impl(
    ($($id:ident),+) => (
         impl<'a, Error, Output: AsRef<str>, $($id: Fn(&'a str) -> IResult<&'a str, Output, Error>),+> ParserList<'a, Output, Error> for ($($id),*)
         where
            Error: McqConverterError<&'a str> + std::fmt::Debug,
         {
            fn parse_until_found(&self, i: &'a str, error_if_empty: bool) -> IResult<&'a str, String, Error> {
                parser_list_test!(0, self, i, error_if_empty, $($id)+);
                let (i, (v, _)) = nom::combinator::cut(many_till(
                    take(1usize),
                    alt(
                        wrap_tuple_elements_with!(self, peek, $($id)*)
                    )
                ))(i)?;
                let text: String = v.into_iter().collect();

                Ok((i, text))
            }
        }
    );
);

macro_rules! parser_list(
  ($first:ident $second:ident $($id: ident)+) => (
    parser_list!(__impl $first $second; $($id)+);
  );
  (__impl $($current:ident)*; $head:ident $($id: ident)+) => (
    parser_list_impl!($($current),*);
    parser_list!(__impl $($current)* $head; $($id)+);
  );
  (__impl $($current:ident)*; $head:ident) => (
    parser_list_impl!($($current),*);
    parser_list_impl!($($current),*,$head);
  );
);

macro_rules! parser_list_test(
    ($n:tt, $self:expr, $input:expr, $err:expr, $head:ident $($id:ident)+) => (
        let r: nom::IResult<&'a str, Output, Error> = nom::combinator::peek(&$self.$n)($input);
        if let Ok(_) = r {
            if $err == true {
                return Err(crate::errors::nom_error($input, nom::error::ErrorKind::Eof));
            }
            else {
                return Ok(($input, String::new()))
            }
        }
        succ!($n, parser_list_test!($self, $input, $err, $($id)+));
    );

    ($n:tt, $self:expr, $input:expr, $err:expr, $head:ident) => (
        let r: nom::IResult<&'a str, Output, Error> = nom::combinator::peek(&$self.$n)($input);
        if let Ok(_) = r {
            if $err == true {
                return Err(crate::errors::nom_error($input, nom::error::ErrorKind::Eof));
            }
            else {
                return Ok(($input, String::new()))
            }
        }
    );
);

macro_rules! succ (
  (0, $submac:ident ! ($($rest:tt)*)) => ($submac!(1, $($rest)*));
  (1, $submac:ident ! ($($rest:tt)*)) => ($submac!(2, $($rest)*));
  (2, $submac:ident ! ($($rest:tt)*)) => ($submac!(3, $($rest)*));
  (3, $submac:ident ! ($($rest:tt)*)) => ($submac!(4, $($rest)*));
  (4, $submac:ident ! ($($rest:tt)*)) => ($submac!(5, $($rest)*));
  (5, $submac:ident ! ($($rest:tt)*)) => ($submac!(6, $($rest)*));
  (6, $submac:ident ! ($($rest:tt)*)) => ($submac!(7, $($rest)*));
  (7, $submac:ident ! ($($rest:tt)*)) => ($submac!(8, $($rest)*));
  (8, $submac:ident ! ($($rest:tt)*)) => ($submac!(9, $($rest)*));
  (9, $submac:ident ! ($($rest:tt)*)) => ($submac!(10, $($rest)*));
  (10, $submac:ident ! ($($rest:tt)*)) => ($submac!(11, $($rest)*));
);

// Haha this solution is a shame but i really couldn't come up with anything else.
// Spent 3 hours on this, couldn't find another way to return a tuple wrapped under the given function
macro_rules! tuple_element (
  ($self:expr, A) => (&$self.0);
  ($self:expr, B) => (&$self.1);
  ($self:expr, C) => (&$self.2);
  ($self:expr, D) => (&$self.3);
  ($self:expr, E) => (&$self.4);
  ($self:expr, F) => (&$self.5);
  ($self:expr, G) => (&$self.6);
  ($self:expr, H) => (&$self.7);
  ($self:expr, I) => (&$self.8);
  ($self:expr, J) => (&$self.9);
  ($self:expr, K) => (&$self.10);
);

macro_rules! wrap_tuple_elements_with(
    ($self: expr, $f: ident, $($id:ident)+) => (
        ($(
            $f(tuple_element!($self, $id))
        ),+)
    );
);


parser_list![A B C D E F G H I J K];

/*
pub fn custom_tuple<'a, O, E, List>(parsers: List) -> impl Fn(&'a str) -> IResult<&'a str, String, E>
where
    E: McqConverterError<&'a str>,
    List: ParserList<'a, O, E>,
{
    move |i: &'a str| {
        parsers.single_output_tuple(i)
    }
}

fn single_output_tuple(&self, i: &'a str) -> IResult<&'a str, String, Error> {
    let (i, res) = nom::sequence::tuple(wrap_tuple!(self, peek, $($id)*))(i)?;
    let mut text = String::new();
    parser_list_push_str!(0, res, text, push_str, $($id),*);

    Ok((i, text))
}

macro_rules! parser_list_push_str(
    ($n:tt, $tuple:ident, $string:ident, $method:ident, $head:ident, $($id:ident),+) => (
        $string.$method(&$tuple.$n.as_ref());
        succ!($n, parser_list_push_str!($tuple, $string, $method, $($id),+));
    );

    ($n:tt, $tuple:ident, $string:ident, $method:ident, $head:ident) => (
        $string.$method(&$tuple.$n.as_ref());
    );
);

macro_rules! wrap_tuple(
    ($self: expr, $f: ident, $($id:ident)+) => (
        ($(
            tuple_element!($self, $id)
        ),+)
    );
);
 */

/*
pub trait ParserListPreceded<'a, E> {
    fn parse_until_preceded_found(&self, input: &'a str, error_if_empty: bool) -> IResult<&'a str, String, E>;
}

pub fn take_until_preceded_parsers<'a, E, List>(error_if_empty:bool, parsers: List) -> impl Fn(&'a str) -> IResult<&'a str, String, E>
    where
        E: McqConverterError<&'a str>,
        List: ParserListPreceded<'a, E>,
{
    move |i: &'a str| {
        let (i, text) = parsers.parse_until_preceded_found(i, error_if_empty)?;
        Ok((i, text))
    }
}

macro_rules! parser_list_preceded_impl(
    ($(($($id:ident),+)),+) => (
        impl<'a, Error, $($($id: Fn(&'a str) -> IResult<&'a str, &'a str, Error>),+),+> ParserListPreceded<'a, Error> for ($($($id),+),+)
         where
            Error: McqConverterError<&'a str>,
         {
            fn parse_until_preceded_found(&self, i: &'a str, error_if_empty: bool) -> IResult<&'a str, String, Error>
            {
                parser_list_preceded_test!(0, self, i, error_if_empty, $(($($id),*)),*);

                let (i, (text, res)) = many_till(
                    take(1usize),
                    alt(
                        wrap_tuple_elements_preceded_with!(self, peek, $(($($id),*)),*)
                    )
                )(i)?;
                let mut text: String = text.into_iter().collect();
                parser_list_preceded_len!(0, res, text, push_str, $(($($id),*)),*);

                Ok((i, text))
            }
         }
    );
);

macro_rules! parser_list_preceded_test(
    ($n:tt, $n1:tt, $self:expr, $input:expr, $err:expr, $head:ident,$($id:ident),+) => (
        succ!($n, parser_list_preceded_test!($n1, $self, $input, $err, $($id),+));
    );

    ($n:tt, $self:expr, $input:expr, $err:expr, ($($head:ident),+),$(($($id:ident),+)),+) => (
        parser_list_preceded_test!(0, $n, $self, $input, $err, $($head),+);
        succ!($n, parser_list_preceded_test!($self, $input, $err, $(($($id),+)),+));
    );

    ($n:tt, $self:expr, $input:expr, $err:expr, ($($id:ident),+)) => (
        parser_list_preceded_test!(0, $n, $self, $input, $err, $($id),+);
    );

    ($n:tt, $n1:tt, $self:expr, $input:expr, $err:expr, $head:ident) => (
        let r: nom::IResult<&'a str, &'a str, Error> = nom::combinator::peek(&$self.$n1.$n)($input);
        if let Ok(_) = r {
            if $err == true {
                return Err(crate::errors::nom_error($input, nom::error::ErrorKind::Eof));
            }
            else {
                return Ok(($input, String::new()))
            }
        }
    );
);

macro_rules! parser_list_preceded_len(
    ($n:tt, $tuple:ident, $string:ident, $method:ident,($($head:ident),+),$(($($id:ident),+)),+) => (
        parser_list_preceded_len!($n, $tuple, $string, $method, $($head),+);
    );

    ($n:tt, $tuple:ident, $string:ident, $method:ident, $head:ident, $($id:ident),+) => (
        $string.$method($tuple.$n);
        succ!($n, parser_list_preceded_len!($tuple, $string, $method, $($id),+));
    );

    ($n:tt, $tuple:ident, $string:ident, $method:ident, $head:ident) => (
        $string.$method($tuple.$n);
    );
);

macro_rules! value_elt (
  ($self:expr, A, A) => (&($self.0).0);
  ($self:expr, A, AA) => (&($self.0).1);
  ($self:expr, A, AAA) => (&($self.0).2);
  ($self:expr, A, AAAA) => (&($self.0).3);
  ($self:expr, A, AAAAA) => (&($self.0).4);
  ($self:expr, A, AAAAAA) => (&($self.0).5);
  ($self:expr, B, B) => (&($self.1).0);
  ($self:expr, B, BB) => (&($self.1).1);
  ($self:expr, B, BBB) => (&($self.1).2);
  ($self:expr, B, BBBB) => (&($self.1).3);
  ($self:expr, B, BBBBB) => (&($self.1).4);
  ($self:expr, B, BBBBBB) => (&($self.1).5);
  ($self:expr, C, C) => (&($self.2).0);
  ($self:expr, C, CC) => (&($self.2).1);
  ($self:expr, C, CCC) => (&($self.2).2);
  ($self:expr, C, CCCC) => (&($self.2).3);
  ($self:expr, C, CCCCC) => (&($self.2).4);
  ($self:expr, C, CCCCCC) => (&($self.2).5);
);

macro_rules! wrap_tuple_elements_preceded_with(
    ($self: expr, $f: ident, $(($head:ident,$($id:ident),+)),+) => (
        ($(
            nom::sequence::tuple((
                value_elt!($self,$head,$head),
                $(value_elt!($self,$head,$id)),+
            ))
        ),+)
    );
);

/*
macro_rules! parser_list_preceded(
  (($f:ident,$s:ident) $(($first:ident,$second:ident))+) => (
    parser_list_preceded!(__impl ($f,$s); $(($first,$second))+);
  );
  (__impl $(($f:ident,$s:ident))*; ($f1:ident,$s1:ident) $(($first:ident,$second:ident))+) => (
    parser_list_preceded_impl!($(($f,$s)),*);

    parser_list_preceded!(__impl $(($f,$s))* ($f1,$s1); $(($first,$second))+);
  );
  (__impl $(($f:ident,$s:ident))*; ($f1:ident,$s1:ident)) => (
    parser_list_preceded_impl!($(($f,$s)),*);
    parser_list_preceded_impl!($(($f,$s)),*,($f1,$s1));
  );
);
 */

parser_list_preceded_impl![(A,AA),(B,BB),(C,CC)];
//parser_list_preceded_impl![(A,AA),(B,BB),(C,CC),(D,DD)];

 */