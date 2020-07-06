use crate::utils::{take_opt_sp, take_opt_newline_with_sp, escape_parser_with, eof, take_double_newline_with_sp};
use crate::macros::{take_until_parsers};
use crate::common::QuestionComment;
use crate::text::{TextZone, TextType, TextElement};

use nom::{IResult};
use nom::character::complete::{char};
use nom::multi::{fold_many0, fold_many1};
use nom::branch::alt;
use nom::bytes::complete::{tag, is_not};
use nom::sequence::{preceded};
use crate::errors::{McqConverterError, inv_cut};
use nom::combinator::{peek, cut, not};
use nom::error::context;

/// Combines take_opt_newline_with_sp with the comment parser
/// Takes multiple comments in a row
///
/// # Failures
/// - If double newline is encountered
pub fn take_opt_newline_with_sp_with_comments<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<TextType>, E> {
    let (i, comments) = fold_many0(
        parse_comment_raw,
        Vec::new(),
        |mut acc, comment| {
            acc.push(comment);
            acc
        }
    )(i)?;

    context("Found unexpected double newline",
        cut(
            not(peek(take_double_newline_with_sp::<&str, E>))
        )
    )(i)?;

    let (i, _) = take_opt_newline_with_sp(i)?;

    Ok((i, comments))
}

/// Match the start of a comment
///
/// # Errors
/// - If `\n` is not matched
/// - If `//` is not matched
pub fn stop_comment_tag<'a, O: AsRef<str> + Default, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, O, E> {
    let (i, _) = char('\n')(i)?;
    let (i, _) = take_opt_sp(i)?;
    let (i, _) = tag("//")(i)?;
    Ok((i, O::default()))
}

/// Match a backslash
///
/// # Errors
/// - If `\\` is not matched
pub fn stop_backslash<'a, O: AsRef<str> + Default, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, O, E> {
    let (i, _) = tag("\\")(i)?;
    Ok((i, O::default()))
}

/// Parse a comment
/// Doesn't take the trailing \n
///
/// # Errors
/// - If `//` is not matched
fn parse_comment_raw<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextType, E> {
    let (i, _) = take_opt_newline_with_sp(i)?;
    let (i, _) = tag("//")(i)?;
    let (i, comment) = alt((
        is_not("\n"),
        peek(tag("\n")),
        eof,
    ))(i)?;

    Ok((i, TextType::Comment(
        QuestionComment::new(comment.trim().to_string())
    )))
}

/// Wrapper around parse_text_zone_until_pattern
/// Trim the value and gift unescape it
pub fn parse_text_zone_until<'a, O, F, E>(parser: F) -> impl Fn(&'a str) -> IResult<&'a str, TextZone, E>
where
    E: McqConverterError<&'a str>,
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
    O: AsRef<str> + Default,
{
    move |i: &str| {
        parse_text_zone(parse_text_zone_until_pattern(&parser))(i)
    }
}

/// Wrapper around parse_text_zone_until_unescaped_pattern
/// Trim the value and gift unescape it
pub fn parse_text_zone_until_unescaped<'a, O, F, G, E>(parser: F, escape_parser: G) -> impl Fn(&'a str) -> IResult<&'a str, TextZone, E>
where
    E: McqConverterError<&'a str>,
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
    G: Fn(&'a str) -> IResult<&'a str, O, E>,
    O: AsRef<str> + Default,
{
    move |i: &str| {
        parse_text_zone(parse_text_zone_until_unescaped_pattern(&parser, &escape_parser))(i)
    }
}

/// Parse the text zone until parser is matched
///
/// # Errors
/// - If pattern is matched directly
/// # Failures
/// - If pattern is not matched
fn parse_text_zone_until_pattern<'a, O, F, E>(parser: F) -> impl Fn(&'a str) -> IResult<&'a str, TextType, E>
    where
        E: McqConverterError<&'a str>,
        F: Fn(&'a str) -> IResult<&'a str, O, E>,
        O: AsRef<str> + Default,
{
    move |i: &str| {
        let (i, text) = take_until_parsers(
            true,
            (stop_comment_tag, &parser)
        )(i)?;
        Ok((i, TextType::Text(vec![TextElement::new(&text)])))
    }
}

/// Parse the text zone using the parser and escapes chars using the escape_parser
/// Handle special case where i == "\\".
///
/// # Errors
/// - If parser goes into an infinite loop
/// - If escape_parser fails to parse escaped char
/// - If pattern is matched directly
/// - If input is empty
/// # Failures
/// - If pattern is not matched
fn parse_text_zone_until_unescaped_pattern<'a, O, F, G, E>(parser: F, escape_parser: G) -> impl Fn(&'a str) -> IResult<&'a str, TextType, E>
    where
        E: McqConverterError<&'a str>,
        F: Fn(&'a str) -> IResult<&'a str, O, E>,
        G: Fn(&'a str) -> IResult<&'a str, O, E>,
        O: AsRef<str> + Default,
{
    move |i: &str| {
        // Special case if we have only "\\", escaped will fail
        let (i, text) = escape_parser_with(
            alt((
                take_until_parsers(
                    true,
                    (
                        stop_backslash,
                        stop_comment_tag,
                        &parser
                    )
                ),
                preceded(char('\\'), eof),
            )),
            '\\',
            &escape_parser,
        )(i)?;

        Ok((i, TextType::Text(vec![TextElement::new(&text)])))
    }
}

/// Parse a text zone (Question text, Answer text, ...)
/// Trim and gift unescape the value
///
/// # Errors
/// - If we get into an infinite loop
/// - If we don't get at least one valid result
pub fn parse_text_zone<'a, F, E>(parser: F) -> impl Fn(&'a str) -> IResult<&'a str, TextZone, E>
where
    E: McqConverterError<&'a str>,
    F: Fn(&'a str) -> IResult<&'a str, TextType, E>,
{
    move |i: &str| {
        let (i, mut text_zone) = inv_cut(
            fold_many1(
                alt((
                    parse_comment_raw,
                    &parser,
                )),
                TextZone::new(),
                |mut text_zone, text_type| {
                    text_zone.comments_and_elements.push(text_type.clone());
                    text_zone
                }
            )
        )(i)?;
        text_zone.trim_value();
        text_zone.gift_unescape_value();

        Ok((i, text_zone))
    }
}



#[cfg(test)]
mod tests {
    use crate::gift::text::{take_opt_newline_with_sp_with_comments, stop_comment_tag, stop_backslash, parse_comment_raw, parse_text_zone_until_pattern, parse_text_zone_until_unescaped_pattern, parse_text_zone_until, parse_text_zone_until_unescaped};
    use crate::common::QuestionComment;
    use crate::text::{TextType, TextElement, TextZone};
    use crate::text::TextType::{Comment, Text};

    use nom::IResult;
    use nom::Err::{Error, Failure};
    use nom::error::ErrorKind;
    use nom::bytes::complete::{tag, take};
    use crate::utils::{eof};
    use nom::branch::alt;

    #[test]
    fn test_take_opt_newline_with_sp_with_comments() {
        let i = "\n//comment\n";
        let r: IResult<&str, Vec<TextType>> = take_opt_newline_with_sp_with_comments(i);
        assert_eq!(r, Ok(("", vec![Comment(QuestionComment::new("comment".to_string()))])));

        let i = "\t\n\t//\tcomment\t\n\t\t//\tcomment2\t\n";
        let r: IResult<&str, Vec<TextType>> = take_opt_newline_with_sp_with_comments(i);
        assert_eq!(r, Ok(("", vec![
            Comment(QuestionComment::new("comment".to_string())),
            Comment(QuestionComment::new("comment2".to_string())),
        ])));

        let i = "test\ntest";
        let r: IResult<&str, Vec<TextType>> = take_opt_newline_with_sp_with_comments(i);
        assert_eq!(r, Ok((i, vec![])));

        let i = "\n//comment\n\t\n";
        let r: IResult<&str, Vec<TextType>> = take_opt_newline_with_sp_with_comments(i);
        assert_eq!(r, Err(Failure(("\n\t\n", ErrorKind::Not))));
    }

    #[test]
    fn test_stop_comment_tag() {
        let i = "\n//\tcomment";
        let r: IResult<&str, &str> = stop_comment_tag(i);
        assert_eq!(r, Ok(("\tcomment", "")));

        let i = "\n\t\r //\tcomment";
        let r: IResult<&str, String> = stop_comment_tag(i);
        assert_eq!(r, Ok(("\tcomment", "".to_string())));

        let i = "test";
        let r: IResult<&str, &str> = stop_comment_tag(i);
        assert_eq!(r, Err(Error((i, ErrorKind::Char))));
    }

    #[test]
    fn test_stop_backslash() {
        let i = "\\{";
        let r: IResult<&str, &str> = stop_backslash(i);
        assert_eq!(r, Ok(("{", "")));

        let i = "\\{";
        let r: IResult<&str, String> = stop_backslash(i);
        assert_eq!(r, Ok(("{", "".to_string())));

        let i = "test";
        let r: IResult<&str, &str> = stop_backslash(i);
        assert_eq!(r, Err(Error((i, ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_comment_raw() {
        let i = "\n\t\r//comment\t\n\ttest";
        let r: IResult<&str, TextType> = parse_comment_raw(i);
        assert_eq!(r, Ok(("\n\ttest", TextType::Comment(QuestionComment::new("comment".to_string())))));

        let i = "\n\t\r//comment";
        let r: IResult<&str, TextType> = parse_comment_raw(i);
        assert_eq!(r, Ok(("", TextType::Comment(QuestionComment::new("comment".to_string())))));

        let i = "\n\t\r//\ntest";
        let r: IResult<&str, TextType> = parse_comment_raw(i);
        assert_eq!(r, Ok(("\ntest", TextType::Comment(QuestionComment::new("".to_string())))));

        let i = "test";
        let r: IResult<&str, TextType> = parse_comment_raw(i);
        assert_eq!(r, Err(Error((i, ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_text_zone_until_pattern() {
        let i = "test\n\t\n{test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(
            tag("{")
        )(i);
        assert_eq!(r, Ok(("{test", Text(vec![
            TextElement::new("test\n\t\n")
        ]))));

        let i = "test\n//\t\n{test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(
            tag("{")
        )(i);
        assert_eq!(r, Ok(("\n//\t\n{test", Text(vec![
            TextElement::new("test")
        ]))));

        let i = "test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(
            tag("{")
        )(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));

        let i = "{test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(
            tag("{")
        )(i);
        assert_eq!(r, Err(Error((i, ErrorKind::Eof))));
    }

    #[test]
    fn test_parse_text_zone_until_unescaped_pattern() {
        let i = "test\\{\\[\\]\\}test{}test";
        let r: IResult<&str, TextType> = parse_text_zone_until_unescaped_pattern(
            alt((
                tag("{"),
                tag("["),
                tag("]"),
                tag("}")
            )),
            take(1usize),
        )(i);
        assert_eq!(r, Ok(("{}test", Text(vec![
            TextElement::new("test\\{\\[\\]\\}test")
        ]))));

        let i = "test\n// comment \ntest{}test";
        let r: IResult<&str, TextType> = parse_text_zone_until_unescaped_pattern(
            alt((
                tag("{"),
                tag("["),
                tag("]"),
                tag("}")
            )),
            take(1usize),
        )(i);
        assert_eq!(r, Ok(("\n// comment \ntest{}test", Text(vec![
            TextElement::new("test")
        ]))));

        let i = "test\\";
        let r: IResult<&str, TextType> = parse_text_zone_until_unescaped_pattern(
            alt((
                tag("{"),
                tag("["),
                tag("]"),
                tag("}"),
                eof
            )),
            alt((eof, take(1usize))),
        )(i);
        assert_eq!(r, Ok(("", Text(vec![
            TextElement::new("test\\")
        ]))));

        let i = "test";
        let r: IResult<&str, TextType> = parse_text_zone_until_unescaped_pattern(
            alt((
                tag("{"),
                eof
            )),
            take(1usize),
        )(i);
        assert_eq!(r, Ok(("", Text(vec![
            TextElement::new("test")
        ]))));

        // Special case
        let i = "\\";
        let r: IResult<&str, TextType> = parse_text_zone_until_unescaped_pattern(
            tag("{"),
            take(1usize),
        )(i);
        assert_eq!(r, Ok(("", Text(vec![
            TextElement::new("\\")
        ]))));

        let i = "test";
        let r: IResult<&str, TextType> = parse_text_zone_until_unescaped_pattern(
            tag("{"),
            take(1usize),
        )(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));
    }

    #[test]
    fn test_parse_text_zone_until() {
        let i = "test\n\t//\tcomment\t\ntest\n\t{test";
        let r: IResult<&str, TextZone> = parse_text_zone_until(
            tag("{")
        )(i);
        assert_eq!(r, Ok(("{test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "test\n// comment\ntest";
        let r: IResult<&str, TextZone> = parse_text_zone_until(
            tag("{")
        )(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Eof))));
    }

    #[test]
    fn test_parse_text_zone_until_unescaped() {
        let i = "test\\{\n\t//\tcomment\t\ntest\\}\n\t{test";
        let r: IResult<&str, TextZone> = parse_text_zone_until_unescaped(
            alt((
                tag("{"),
                tag("}"),
            )),
            take(1usize),
        )(i);
        assert_eq!(r, Ok(("{test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test{")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test}")]),
            ]
        })));

        let i = "test\\{\n// comment\ntest";
        let r: IResult<&str, TextZone> = parse_text_zone_until_unescaped(
            tag("{"),
            take(1usize)
        )(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Eof))));
    }
}
