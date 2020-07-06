use crate::gift::gift::{Gift};
use crate::gift::question::{parse_question, QuestionGroup, Question};
use crate::gift::text::{parse_text_zone_until};
use crate::errors::{context_failure, McqConverterError};
use crate::utils::{take_opt_sp, take_double_newline_with_sp, eof};

use nom::IResult;
use nom::character::complete::{char};
use nom::bytes::complete::{is_not, tag};
use nom::combinator::{all_consuming, cut, opt};
use nom::branch::alt;
use nom::multi::{fold_many0};
use nom::error::{ErrorKind, context};
use crate::common::{QuestionComment, AllElements};
use crate::text::TextZone;

/*
GLOBAL TODO:
-
 */

#[derive(Debug, PartialEq)]
pub enum GiftObject {
    Category(TextZone),
    Comment(String),
    Question(Question),
    BlankLine,
}

/// Parse a blank line (`[\t\r]\n`)
///
/// # Errors
/// - If input doesn't start with `\n`
fn parse_blank_line<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, GiftObject, E> {
    let (i, _) = take_opt_sp(i)?;
    let(i, _) = char('\n')(i)?;
    Ok((i, GiftObject::BlankLine))
}

/// Parse a category (`$CATEGORY`)
/// Takes the optional trailing `\n`
/// Trim the value and gift unescape it
///
/// # Errors
/// - If input doesn't start with `$CATEGORY:`
/// # Failures
/// - If value is empty
fn parse_category<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, GiftObject, E> {
    let (i, _) = take_opt_sp(i)?;
    let (i, _) = context("category",
                         tag("$CATEGORY:")
    )(i)?;
    let (i, category) = cut(
        parse_text_zone_until(
            alt((
                take_double_newline_with_sp::<&str, E>,
                eof::<&str, E>,
            ))
        )
    )(i)?;
    if category.is_empty() {
        return Err(context_failure(i, "Category is empty", ErrorKind::Not))
    }

    let (i, _) = opt(char('\n'))(i)?;

    Ok((i, GiftObject::Category(category)))
}

/// Parse a comment (`// comment`)
/// Takes the optional trailing `\n`
/// Trim the value
///
/// # Errors
/// - If input doesn't start with `\\`
pub fn parse_comment<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, GiftObject, E> {
    let (i, _) = tag("//")(i)?;
    let (i, comment) = alt((
        is_not("\n"),
        tag("\n"),
        eof,
    ))(i)?;
    let(i, _) = opt(char('\n'))(i)?;

    Ok((i, GiftObject::Comment(comment.trim().to_string())))
}

fn run_parsing<'a, E: McqConverterError<&'a str>>(i: &'a str, gift: Gift) -> IResult<&'a str, Gift, E> {
    // TODO: Maybe change this, it looks like it clones the object at each loop, slow process
    let(i, mut gift) = all_consuming(
        fold_many0(
            alt((
                context("parse_category",
                    parse_category
                ),
                context("parse_comment",
                    parse_comment
                ),
                context("parse_blank_line",
                    parse_blank_line
                ),
                // We don't set a context to avoid flooding the error output
                parse_question,
            )),
            gift,
            |mut gift: Gift, item| {
                match item {
                    GiftObject::Question(q) => {
                        if let Some(c) = gift.current_category.as_mut() {
                            c.content.push(AllElements::Single(q));
                        }
                        else {
                            gift.content.push(AllElements::Single(q));
                        }
                    },
                    GiftObject::Comment(c) => {
                        if let Some(cat) = gift.current_category.as_mut() {
                            cat.content.push(AllElements::Comment(QuestionComment::new(c)));
                        }
                        else {
                            gift.content.push(AllElements::Comment(QuestionComment::new(c)));
                        }
                    },
                    GiftObject::Category(category) => {
                        if let Some(g) = gift.current_category {
                            gift.content.push(AllElements::Group(g));
                        }
                        let group = QuestionGroup {
                           category: category,
                           content: vec![],
                        };
                        gift.current_category = Some(group);
                    }
                    _ => ()
                }
                gift
            }
        )
    )(i)?;

    // Appending last category and cleaning
    if let Some(g) = &gift.current_category {
        gift.content.push(AllElements::Group(g.clone()));
    }
    gift.current_category = None;

    Ok((i, gift))
}

pub fn parse_file<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, Gift, E> {
    let gift = Gift {
        current_category: None,
        content: vec![],
    };

    let(i, gift) = run_parsing(i, gift)?;

    Ok((i, gift))
}








#[cfg(test)]
mod tests {
    use crate::gift::parser::{parse_blank_line, parse_comment, GiftObject, parse_category};
    use crate::common::QuestionComment;
    use crate::text::{TextElement, TextZone};
    use crate::text::TextType::{Text, Comment};

    use nom::IResult;
    use nom::Err::{Failure, Error};
    use nom::error::ErrorKind;

    #[test]
    fn test_parse_blank_line() {
        let i = "\t  \r \t \t\ntest";
        let r: IResult<&str, GiftObject> = parse_blank_line(i);
        assert_eq!(r, Ok(("test", GiftObject::BlankLine)));

        let i = "\t\n\t\ntest";
        let r: IResult<&str, GiftObject> = parse_blank_line(i);
        assert_eq!(r, Ok(("\t\ntest", GiftObject::BlankLine)));

        let i = "test";
        let r: IResult<&str, GiftObject> = parse_blank_line(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Char))));
    }

    #[test]
    fn test_parse_category() {
        let i = "$CATEGORY: a b c d e f\n// test comment\ntest";
        let r: IResult<&str, GiftObject> = parse_category(i);
        assert_eq!(r, Ok(("", GiftObject::Category(TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("a b c d e f")]),
                Comment(QuestionComment::new("test comment".to_string())),
                Text(vec![TextElement::new("test")]),
            ],
        }))));

        let i = "$CATEGORY:\n\ta\n\tb\n\tc\n\td\n\te\n\tf\n\t // comment\n\t // comment2\n\t test";
        let r: IResult<&str, GiftObject> = parse_category(i);
        assert_eq!(r, Ok(("", GiftObject::Category(TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("a \tb \tc \td \te \tf")]),
                Comment(QuestionComment::new("comment".to_string())),
                Comment(QuestionComment::new("comment2".to_string())),
                Text(vec![TextElement::new("test")]),
            ],
        }))));

        let i = "$CATEGORY:";
        let r: IResult<&str, GiftObject> = parse_category(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Many1))));

        let i = "$CATEGORY:\t\r\n";
        let r: IResult<&str, GiftObject> = parse_category(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Not))));

        let i = "test";
        let r: IResult<&str, GiftObject> = parse_category(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_comment() {
        let i = "//test\ntest";
        let r: IResult<&str, GiftObject> = parse_comment(i);
        assert_eq!(r, Ok(("test", GiftObject::Comment("test".to_string()))));

        let i = "//\t test \t\ntest";
        let r: IResult<&str, GiftObject> = parse_comment(i);
        assert_eq!(r, Ok(("test", GiftObject::Comment("test".to_string()))));

        let i = "test";
        let r: IResult<&str, GiftObject> = parse_comment(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Tag))));

        let i = "//\n";
        let r: IResult<&str, GiftObject> = parse_comment(i);
        assert_eq!(r, Ok(("", GiftObject::Comment("".to_string()))));
    }

    #[test]
    fn test_run_parsing() {
        // TODO
    }

    #[test]
    fn test_parse_file() {
        // TODO
    }
}