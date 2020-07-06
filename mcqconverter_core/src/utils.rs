use crate::errors::{nom_error, McqConverterError};

use nom::error::{ParseError, ErrorKind};
use nom::{IResult};
use nom::multi::{many_till};
use nom::bytes::complete::{take, take_while, escaped, tag, take_while1};
use nom::sequence::tuple;
use nom::character::complete::{char};
use nom::combinator::{peek, opt};
use nom::character::is_alphanumeric;

#[allow(dead_code)]
pub fn take_until_parser<'a, F, E>(exit_if_empty: bool, parser: F) -> impl Fn(&'a str) -> IResult<&'a str, String, E>
where
    E: McqConverterError<&'a str>,
    F: Fn(&'a str) -> IResult<&'a str, &'a str, E>,
{
    move |i: &str| {
        let r: nom::IResult<&'a str, &'a str, E> = peek(&parser)(i);
        if let Ok(_) = r {
            if exit_if_empty == true {
                return Err(crate::errors::nom_error(i, nom::error::ErrorKind::Eof));
            } else {
                return Ok((i, String::new()));
            }
        }
        let (i, (text, _)) = nom::multi::many_till(
            take(1usize),
            peek(&parser),
        )(i)?;
        let text: String = text.into_iter().collect();
        Ok((i, text.to_string()))
    }
}

#[allow(dead_code)]
pub fn eof<'a, O: Default, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, O, E> {
    if i.len() == 0 {
        Ok((i, O::default()))
    }
    else {
        return Err(nom::Err::Error(ParseError::from_error_kind(i, ErrorKind::Eof)));
    }
}

#[allow(dead_code)]
pub fn is_eof<'a>(i: &'a str) -> bool {
    i.len() == 0
}

#[allow(dead_code)]
pub fn take_opt_sp<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E>
{
    let chars = " \t\r";
    take_while(move |c| chars.contains(c))(i)
}

#[allow(dead_code)]
pub fn take_opt_newline_with_sp<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
    let(i, space1) = take_opt_sp(i)?;
    let (i, c) = opt(char('\n'))(i)?;
    if let Some(c) = c {
        let(i, space2) = take_opt_sp(i)?;
        let spaces = format!("{}{}{}", space1, c, space2);
        Ok((i, spaces))
    }
    else {
        Ok((i, space1.to_string()))
    }
}

#[allow(dead_code)]
pub fn take_double_newline_with_sp<'a, O: Default, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, O, E> {
    let (i, _) = take_opt_sp(i)?;
    let (i, _) = char('\n')(i)?;
    let (i, _) = take_opt_sp(i)?;
    let (i, _) = char('\n')(i)?;
    Ok((i, O::default()))
}

// TODO: remove this
#[allow(dead_code)]
pub fn take_until_double_newline_with_sp<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
    let (i, (data, _)) = many_till(
        take(1usize),
        peek(
            tuple((
                char('\n'),
                take_opt_sp,
                char('\n')
            ))
        )
    )(i)?;
    let data: String = data.into_iter().collect();

    Ok((i, data.to_string()))
}

#[allow(dead_code)]
pub fn is_decimal_digit_with_sp(c: char) -> bool {
    match c {
        '0'..='9' => true,
        '.' => true,
        ' ' => true,
        '\r' => true,
        '\t' => true,
        '\n' => true,
        _ => false,
    }
}

pub fn take_non_decimal_digit_with_sp<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let (i, c) = peek(take(1usize))(i)?;
    if !is_decimal_digit_with_sp(c.chars().last().unwrap()) {
        take(1usize)(i)
    }
    else {
        Err(nom_error(i, ErrorKind::Char))
    }
}

#[allow(dead_code)]
pub fn is_decimal_digit(c: char) -> bool {
    match c {
        '0'..='9' => true,
        '.' => true,
        _ => false,
    }
}

#[allow(dead_code)]
pub fn decimal_digit1<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while1(is_decimal_digit)(i)
}

#[allow(dead_code)]
pub fn alphanumeric_dash1<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let (i, v) = take_while1(|c: char| -> bool {
        if c == '-' {
            return true;
        }
        is_alphanumeric(c as u8)
    })(i)?;

    Ok((i, v))
}

#[allow(dead_code)]
pub fn none_of_str<'a, E: McqConverterError<&'a str>>(no: &'a str) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, E> {
    move |i: &str| {
        let (i, c) = take(1usize)(i)?;
        if no.contains(c) {
            return Err(nom_error(i, ErrorKind::Char));
        }
        Ok((i, c))
    }
}

#[allow(dead_code)]
pub fn custom_padding<'a, F, E>(parser: F, n: usize) -> impl Fn(&'a str) -> IResult<&'a str, String, E>
where
    E: McqConverterError<&'a str>,
    F: Fn(&'a str) -> IResult<&'a str, &'a str, E>,
{
    move |i: &str| {
        let (i, c1) = take(n)(i)?;
        let (i, c2) = parser(i)?;
        Ok((i, format!("{}{}", c1, c2)))
    }
}

#[allow(dead_code)]
pub fn eol<'a, E>(i: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: McqConverterError<&'a str>,
{
    let (i, _) = take_opt_sp(i)?;
    let (i, _) = char('\n')(i)?;
    Ok((i, ""))
}

#[allow(dead_code)]
pub fn tag_string<'a, E>(t: &'a str) -> impl Fn(&'a str) -> IResult<&'a str, String, E>
    where
        E: McqConverterError<&'a str>,
{
    move |i: &str| {
        let (i, d) = tag(t)(i)?;
        Ok((i, d.to_string()))
    }
}

#[allow(dead_code)]
pub fn custom_take<'a, E>(p: usize) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, E>
    where
        E: McqConverterError<&'a str>,
{
    move |i: &str| {
        let (i, d) = take(p)(i)?;
        Ok((i, d))
    }
}

pub fn escape_parser_with<'a, O1, O2, F, G, E>(parser: F, control_char: char, escape_parser: G) -> impl Fn(&'a str) -> IResult<&'a str, String, E>
where
    E: McqConverterError<&'a str>,
    F: Fn(&'a str) -> IResult<&'a str, O1, E>,
    G: Fn(&'a str) -> IResult<&'a str, O2, E>,
{
    move |i: &str| {
        if i.is_empty() {
            return Err(nom_error(i, ErrorKind::Eof));
        }

        let (i, data) = escaped(
            &parser,
            control_char,
            &escape_parser,
        )(i)?;

        Ok((i, data.to_string()))
    }
}






#[cfg(test)]
mod tests {
    use crate::utils::{eof, take_opt_sp, take_opt_newline_with_sp, take_double_newline_with_sp, take_until_double_newline_with_sp, escape_parser_with};
    use crate::macros::take_until_parsers;

    use nom::IResult;
    use nom::error::ErrorKind;
    use nom::bytes::complete::{tag, is_not, take};
    use nom::Err::{Error, Failure};
    use nom::branch::alt;
    use nom::combinator::peek;

    #[test]
    fn test_eof() {
        let i = "";
        let r: IResult<&str, String> = eof(i);
        assert_eq!(r, Ok(("", String::from(""))));

        let i = "test";
        let r: IResult<&str, String> = eof(i);
        assert_eq!(r, Err(nom::Err::Error(("test", ErrorKind::Eof))));
    }

    #[test]
    fn test_take_opt_sp() {
        let i = "\t\r    \t  \rtest\n";
        let r: IResult<&str, &str> = take_opt_sp(i);
        assert_eq!(r, Ok(("test\n", "\t\r    \t  \r")));

        let i = "test";
        let r: IResult<&str, &str> = take_opt_sp(i);
        assert_eq!(r, Ok(("test", "")));
    }

    #[test]
    fn test_take_opt_newline_with_sp() {
        let i = "\t\r  \n  \t  \rtest\n";
        let r: IResult<&str, String> = take_opt_newline_with_sp(i);
        assert_eq!(r, Ok(("test\n", String::from("\t\r  \n  \t  \r"))));

        let i = "test";
        let r: IResult<&str, String> = take_opt_newline_with_sp(i);
        assert_eq!(r, Ok(("test", String::from(""))));
    }

    #[test]
    fn test_take_double_newline_with_sp() {
        let i = "  \t\r  \n  \t\r  \ntest\n";
        let r: IResult<&str, &str> = take_double_newline_with_sp(i);
        assert_eq!(r, Ok(("test\n", "")));

        let i = "test";
        let r: IResult<&str, &str> = take_double_newline_with_sp(i);
        assert_eq!(r, Err(nom::Err::Error((i, ErrorKind::Char))));
    }

    #[test]
    fn test_take_until_double_newline_with_sp() {
        let i = "test  \t\r  \n  \t\r  \ntest\n";
        let r: IResult<&str, String> = take_until_double_newline_with_sp(i);
        assert_eq!(r, Ok(("\n  \t\r  \ntest\n", String::from("test  \t\r  "))));

        let i = "test";
        let r: IResult<&str, String> = take_opt_newline_with_sp(i);
        assert_eq!(r, Ok(("test", String::from(""))));
    }

    #[test]
    fn test_escape_parser_with() {
        let i = "test\\{test{}test";
        let r: IResult<&str, String> = escape_parser_with(
            take_until_parsers(
                true,
                (
                    tag("{"),
                    tag("\\")
                )
            ),
            '\\',
            tag("{"),
        )(i);
        assert_eq!(r, Ok(("{}test", String::from("test\\{test"))));

        let i = "test";
        let r: IResult<&str, String> = escape_parser_with(
            take_until_parsers(
                true,
                (
                    tag("{"),
                    eof,
                )
            ),
            '\\',
            tag("{"),
        )(i);
        assert_eq!(r, Ok(("", String::from("test"))));

        let i = "test\\test\\test{test";
        let r: IResult<&str, String> = escape_parser_with(
            is_not("\\{"),
            '\\',
            take(1usize),
        )(i);
        assert_eq!(r, Ok(("{test", String::from("test\\test\\test"))));

        let i = "\\";
        let r: IResult<&str, String> = escape_parser_with(
            alt((
                tag("\\"),
                is_not("\\"),
            )),
            '\\',
            take(1usize),
        )(i);
        assert_eq!(r, Ok(("", String::from("\\"))));

        let i = "";
        let r: IResult<&str, String> = escape_parser_with(
            tag("{"),
            '\\',
            tag("{"),
        )(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Eof))));

        let i = "test";
        let r: IResult<&str, String> = escape_parser_with(
            take_until_parsers(
                true,
                (
                    tag("{"),
                    tag("}"),
                )
            ),
            '\\',
            tag("{"),
        )(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));

        let i = "test";
        let r: IResult<&str, String> = escape_parser_with(
            take_until_parsers(
                true,
                (
                    tag("//"),
                    alt((
                        tag("#"),
                        tag("}"),
                        tag("="),
                        tag("~"),
                        take_double_newline_with_sp
                    )),
                ),
            ),
            '\\',
            alt((
                peek(tag("\n")),
                take(1usize),
            )),
        )(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));
    }
}
