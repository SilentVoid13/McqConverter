use crate::amctxt::AmcTxt;
use crate::amctxt::amctxt::AmcTxtObject;
use crate::amctxt::question::{QuestionGroup, parse_question};
use crate::amctxt::text::{parse_text_zone};
use crate::errors::{context_failure, McqConverterError};
use crate::common::{AllElements, QuestionComment};
use crate::utils::{take_opt_sp, alphanumeric_dash1, eof};

use std::collections::HashMap;

use nom::IResult;
use nom::error::{context, ErrorKind};
use nom::combinator::{all_consuming, opt, cut};
use nom::character::complete::{char, alpha1, alphanumeric1};
use nom::multi::{fold_many0, separated_list};
use nom::branch::alt;
use nom::bytes::complete::{tag, is_not};
use nom::lib::std::collections::VecDeque;
use nom::sequence::{tuple, delimited};

/// Parse a blank line (`[\t\r]\n`)
///
/// # Errors
/// - If input doesn't start with `\n`
// TODO: Create a generic func to avoid code duplication
fn parse_blank_line<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, AmcTxtObject, E> {
    let (i, _) = take_opt_sp(i)?;
    let(i, _) = char('\n')(i)?;
    Ok((i, AmcTxtObject::BlankLine))
}


/// Parse a comment (`# comment`)
/// Takes the optional trailing `\n`
/// Trim the value
///
/// # Errors
/// - If input doesn't start with `#`
pub fn parse_comment<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, AmcTxtObject, E> {
    let (i, _) = tag("#")(i)?;
    let (i, comment) = alt((
        is_not("\n"),
        tag("\n"),
        eof,
    ))(i)?;
    let(i, _) = opt(char('\n'))(i)?;

    Ok((i, AmcTxtObject::Comment(comment.trim().to_string())))
}

/// Parse the start of a question group (`*(`)
/// Trim and amctxt unescape the text value
///
/// # Errors
/// - If input doesn't start with `*(`
fn parse_opening_group<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, AmcTxtObject, E> {
    let (i, _) = tag("*(")(i)?;
    let (i, options) = opt(parse_options_with_value)(i)?;

    let (i, text) = cut(
        parse_text_zone
    )(i)?;

    Ok((i, AmcTxtObject::Group(text, options, true)))
}

/// Parse the end of a question group (`*)`)
/// Trim and amctxt unescape the text value
///
/// # Errors
/// - If input doesn't start with `*)`
fn parse_closing_group<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, AmcTxtObject, E> {
    let (i, _) = tag("*)")(i)?;

    let (i, text) = cut(
        parse_text_zone
    )(i)?;

    Ok((i, AmcTxtObject::Group(text, None, false)))
}

/// Tag a general option (`test:`)
///
/// # Errors
/// - If start of input doesn't contains alphanumeric or `-` chars
/// - If no `:` is matched after alphanumeric_dash value
pub fn tag_general_option<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    // TODO: Add check for valid general option
    let (i, (option_name, _)) = tuple((
        alphanumeric_dash1,
        char(':')
    ))(i)?;

    Ok((i, option_name))
}

/// Parse a general option (`test: test`)
/// Trim and amctxt unescape the value
///
/// # Errors
/// - If input doesn't start with general option tag
/// # Failures
/// - If value is empty
fn parse_general_option<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, AmcTxtObject, E> {
    let (i, option_name, ) = tag_general_option(i)?;

    // TODO: See if opt is possible
    let (i, value) = cut(
        parse_text_zone
    )(i)?;
    if value.is_empty() {
        return Err(context_failure(i, "General option must have a value", ErrorKind::Not));
    }

    Ok((i, AmcTxtObject::GeneralOption(option_name.to_string(), value)))
}

/// Parse options that must contain a value (`[shuffle=true]`)
/// Turn the key value to lowercase
///
/// # Errors
/// - If input doesn't start with `[`
/// - If first_part doesn't contain alpha chars
/// - if no `=` is matched between 2 parts
/// - If second_part doesn't contain alphanumeric chars
/// - If no closing `]` is found
pub fn parse_options_with_value<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, HashMap<String, String>, E> {
    // TODO: check if when not close ("[test") it is interpreted as text or throws and error (if it does, we need to use cut)
    // TODO: Check if we can have spaces
    // TODO: Check if empty options is possible
    let (i, options) = delimited(
        char('['),
        separated_list(
            tag(","),
            tuple((
                alpha1,
                char('='),
                alphanumeric1,
            ))
        ),
        tuple((
            opt(char(',')),
            char(']'),
        )),
    )(i)?;
    // TODO: Check for valid options

    let mut h = HashMap::new();
    for (key, _, value) in options.into_iter() {
        let key = key.to_ascii_lowercase();
        let value = value.to_string();
        h.insert(key, value);
    }
    Ok((i, h))
}

/// Parse options that can contain an optional value (`[ordered,id=test]`)
/// Turn the key value to lowercase
///
/// # Errors
/// - If input doesn't start with `[`
/// - If first_part doesn't contain alpha chars
/// - If no closing `]` is found
pub fn parse_options_with_opt_value<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, HashMap<String, Option<String>>, E> {
    // TODO: check if when not close ("[test") it is interpreted as text or throws and error (if it does, we need to use cut)
    // TODO: Check if we can have spaces
    // TODO: Check if empty options is possible
    let (i, options) = delimited(
        char('['),
        separated_list(
            tag(","),
            tuple((
                alpha1,
                opt(
                    tuple((
                            char('='),
                            alphanumeric1,
                    ))
                )
            ))
        ),
        tuple((
            opt(char(',')),
            char(']'),
        )),
    )(i)?;
    // TODO: Check for valid options

    let mut h = HashMap::new();
    for (key, o) in options.into_iter() {
        let key = key.to_ascii_lowercase();
        if let Some((_, value)) = o {
            let value = value.to_string();
            h.insert(key, Some(value));
        }
        else {
            h.insert(key, None);
        }
    }

    Ok((i, h))
}

/// Main parsing loop
fn run_parsing<'a, E: McqConverterError<&'a str>>(i: &'a str, amctxt: AmcTxt) -> IResult<&'a str, AmcTxt, E> {
    // TODO: Maybe change this, it looks like it clones the object at each loop, slow process
    let (i, mut amctxt) = all_consuming(
        fold_many0(
            alt((
                context("parse_comment",
                        parse_comment
                ),
                context("parse_general_option",
                        parse_general_option
                ),
                context("parse_opening_group",
                        parse_opening_group
                ),
                context("parse_closing_group",
                        parse_closing_group
                ),
                context("parse_blank_line",
                        parse_blank_line
                ),
                // We don't set a context to avoid flooding the error output
                parse_question,
            )),
            amctxt,
            |mut amctxt: AmcTxt, item| {
                match item {
                    AmcTxtObject::GeneralOption(option, value) => {
                        // TODO: Check if value is overwritten
                        amctxt.general_options.insert(option, value);
                    }
                    AmcTxtObject::Question(q) => {
                        if let Some(g) = amctxt.current_group.front_mut() {
                            g.content.push(AllElements::Single(q));
                        }
                        else {
                            amctxt.content.push(AllElements::Single(q));
                        }
                    },
                    AmcTxtObject::Comment(c) => {
                        if let Some(cat) = amctxt.current_group.front_mut() {
                            cat.content.push(AllElements::Comment(QuestionComment::new(c)));
                        }
                        else {
                            amctxt.content.push(AllElements::Comment(QuestionComment::new(c)));
                        }
                    },
                    AmcTxtObject::Group(text, options, opening) => {
                        if opening {
                            let group = QuestionGroup {
                                start_text: Some(text),
                                end_text: None,
                                options: options,
                                content: vec![],
                            };
                            amctxt.current_group.push_front(group);
                        }
                        else {
                            if let Some(g) = amctxt.current_group.front() {
                                amctxt.content.push(AllElements::Group(g.clone()));
                                amctxt.current_group.pop_front();
                            }
                        }
                    }
                    _ => ()
                }
                amctxt
            }
        )
    )(i)?;

    // Appending last group and cleaning
    if let Some(g) = amctxt.current_group.front() {
            amctxt.content.push(AllElements::Group(g.clone()));
    }
    amctxt.current_group.clear();

    Ok((i, amctxt))
}

/// Parse an AMC file
pub fn parse_file<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, AmcTxt, E> {
    let amctxt = AmcTxt {
        general_options: HashMap::new(),
        current_group: VecDeque::new(),
        content: vec![],
    };

    let(i, amctxt) = run_parsing(i, amctxt)?;

    Ok((i, amctxt))
}




#[cfg(test)]
mod tests {
    use crate::amctxt::parser::{parse_blank_line, parse_comment, parse_opening_group, parse_closing_group, tag_general_option, parse_general_option, parse_options_with_value, parse_options_with_opt_value};
    use crate::text::{TextZone, TextElement};
    use crate::text::TextType::{Text, Comment};
    use crate::common::QuestionComment;
    use crate::amctxt::amctxt::AmcTxtObject;

    use nom::IResult;
    use nom::Err::{Error, Failure};
    use nom::error::ErrorKind;
    use nom::lib::std::collections::HashMap;

    #[test]
    fn test_parse_blank_line() {
        let i = "\t  \r \t \t\ntest";
        let r: IResult<&str, AmcTxtObject> = parse_blank_line(i);
        assert_eq!(r, Ok(("test", AmcTxtObject::BlankLine)));

        let i = "\t\n\t\ntest";
        let r: IResult<&str, AmcTxtObject> = parse_blank_line(i);
        assert_eq!(r, Ok(("\t\ntest", AmcTxtObject::BlankLine)));

        let i = "test";
        let r: IResult<&str, AmcTxtObject> = parse_blank_line(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Char))));
    }

    #[test]
    fn test_parse_comment() {
        let i = "#test\ntest";
        let r: IResult<&str, AmcTxtObject> = parse_comment(i);
        assert_eq!(r, Ok(("test", AmcTxtObject::Comment("test".to_string()))));

        let i = "#\t test \t\ntest";
        let r: IResult<&str, AmcTxtObject> = parse_comment(i);
        assert_eq!(r, Ok(("test", AmcTxtObject::Comment("test".to_string()))));

        let i = "#\n";
        let r: IResult<&str, AmcTxtObject> = parse_comment(i);
        assert_eq!(r, Ok(("", AmcTxtObject::Comment("".to_string()))));

        let i = "test";
        let r: IResult<&str, AmcTxtObject> = parse_comment(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_opening_group() {
        let i = "*( a b c d e f\n# comment\ntest";
        let r: IResult<&str, AmcTxtObject> = parse_opening_group(i);
        assert_eq!(r, Ok(("", AmcTxtObject::Group(
            TextZone {
                comments_and_elements: vec![
                    Text(vec![TextElement::new_amctxt("a b c d e f")]),
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new_amctxt("test")]),
                ],
            },
            None,
            true,
        ))));

        let i = "*(\n\ta\n\t\n\t\n\tb\n\tc\n\td\n\te\n\tf\n\t # comment\n\t # comment2\n\t test";
        let r: IResult<&str, AmcTxtObject> = parse_opening_group(i);
        assert_eq!(r, Ok(("", AmcTxtObject::Group(
            TextZone {
                comments_and_elements: vec![
                    Text(vec![TextElement::new_amctxt("a\n\tb \tc \td \te \tf")]),
                    Comment(QuestionComment::new("comment".to_string())),
                    Comment(QuestionComment::new("comment2".to_string())),
                    Text(vec![TextElement::new_amctxt("test")]),
                ],
            },
            None,
            true,
        ))));

        let mut options = HashMap::new();
        options.insert("shuffle".to_string(), "false".to_string());
        options.insert("columns".to_string(), "2".to_string());
        options.insert("group".to_string(), "test".to_string());
        options.insert("numquestions".to_string(), "5".to_string());

        let i = "*([shuffle=false,columns=2,group=test,numquestions=5] test\n# comment\ntest";
        let r: IResult<&str, AmcTxtObject> = parse_opening_group(i);
        assert_eq!(r, Ok(("", AmcTxtObject::Group(
            TextZone {
                comments_and_elements: vec![
                    Text(vec![TextElement::new_amctxt("test")]),
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new_amctxt("test")]),
                ],
            },
            Some(options),
            true,
        ))));

        let i = "*(";
        let r: IResult<&str, AmcTxtObject> = parse_opening_group(i);
        assert_eq!(r, Ok(("", AmcTxtObject::Group(
            TextZone::new(),
            None, true
        ))));

        let i = "test";
        let r: IResult<&str, AmcTxtObject> = parse_opening_group(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_closing_group() {
        let i = "*) a b c d e f\n# comment\ntest";
        let r: IResult<&str, AmcTxtObject> = parse_closing_group(i);
        assert_eq!(r, Ok(("", AmcTxtObject::Group(
            TextZone {
                comments_and_elements: vec![
                    Text(vec![TextElement::new_amctxt("a b c d e f")]),
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new_amctxt("test")]),
                ],
            },
            None,
            false,
        ))));

        let i = "*)\n\ta\n\t\n\t\n\tb\n\tc\n\td\n\te\n\tf\n\t # comment\n\t # comment2\n\t test";
        let r: IResult<&str, AmcTxtObject> = parse_closing_group(i);
        assert_eq!(r, Ok(("", AmcTxtObject::Group(
            TextZone {
                comments_and_elements: vec![
                    Text(vec![TextElement::new_amctxt("a\n\tb \tc \td \te \tf")]),
                    Comment(QuestionComment::new("comment".to_string())),
                    Comment(QuestionComment::new("comment2".to_string())),
                    Text(vec![TextElement::new_amctxt("test")]),
                ],
            },
            None,
            false,
        ))));

        let i = "test";
        let r: IResult<&str, AmcTxtObject> = parse_closing_group(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Tag))));
    }

    #[test]
    fn test_tag_general_option() {
        // TODO: Add tests for valid general option

        let i = "test:";
        let r: IResult<&str, &str> = tag_general_option(i);
        assert_eq!(r, Ok(("", "test")));

        let i = "test-test-test:";
        let r: IResult<&str, &str> = tag_general_option(i);
        assert_eq!(r, Ok(("", "test-test-test")));

        let i = "test";
        let r: IResult<&str, &str> = tag_general_option(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));

        let i = "test test:";
        let r: IResult<&str, &str> = tag_general_option(i);
        assert_eq!(r, Err(Error((" test:", ErrorKind::Char))));

        let i = ":";
        let r: IResult<&str, &str> = tag_general_option(i);
        assert_eq!(r, Err(Error((":", ErrorKind::TakeWhile1))));
    }

    #[test]
    fn test_parse_general_option() {
        let i = "test: test\n# comment";
        let r: IResult<&str, AmcTxtObject> = parse_general_option(i);
        assert_eq!(r, Ok(("", AmcTxtObject::GeneralOption(
            "test".to_string(),
            TextZone {
                comments_and_elements: vec![
                    Text(vec![TextElement::new_amctxt("test")]),
                    Comment(QuestionComment::new("comment".to_string())),
                ]
            },
        ))));

        let i = "test:\n\ta\n\t\n\t\n\tb\n\tc\n\td\n\te\n\tf\n\t # comment\n\t # comment2\n\t test \n*(";
        let r: IResult<&str, AmcTxtObject> = parse_general_option(i);
        assert_eq!(r, Ok(("\n*(", AmcTxtObject::GeneralOption(
            "test".to_string(),
            TextZone {
                comments_and_elements: vec![
                    Text(vec![TextElement::new_amctxt("a\n\tb \tc \td \te \tf")]),
                    Comment(QuestionComment::new("comment".to_string())),
                    Comment(QuestionComment::new("comment2".to_string())),
                    Text(vec![TextElement::new_amctxt("test")]),
                ],
            },
        ))));

        let i = "test";
        let r: IResult<&str, AmcTxtObject> = parse_general_option(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));

        let i = "test:";
        let r: IResult<&str, AmcTxtObject> = parse_general_option(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Not))));
    }

    #[test]
    fn test_parse_options_with_value() {
        let mut options = HashMap::new();
        options.insert("shuffle".to_string(), "false".to_string());
        options.insert("columns".to_string(), "2".to_string());
        options.insert("group".to_string(), "test".to_string());
        options.insert("numquestions".to_string(), "5".to_string());

        let i = "[Shuffle=false,Columns=2,Group=test,numquestions=5,]test";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Ok(("test", options)));

        let i = "[]";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Ok(("", HashMap::new())));

        let i = "[,]";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Ok(("", HashMap::new())));

        let i = "[Shuffle=false,,Columns=2,Group=test,numquestions=5,]test";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error((",Columns=2,Group=test,numquestions=5,]test", ErrorKind::Char))));

        let i = "[ordered,horiz,id=sum]";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error(("ordered,horiz,id=sum]", ErrorKind::Char))));

        let i = "[shuffle=false,columns=2";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));

        let i = "[shuffle=false$]";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error(("$]", ErrorKind::Char))));

        let i = "[shuffle=$]";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error(("shuffle=$]", ErrorKind::Char))));

        let i = "[shuffle=]";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error(("shuffle=]", ErrorKind::Char))));
    }

    #[test]
    fn test_parse_options_with_opt_value() {
        let mut options = HashMap::new();
        options.insert("ordered".to_string(), None);
        options.insert("id".to_string(), Some("sum".to_string()));
        options.insert("columns".to_string(), Some("10".to_string()));
        options.insert("horiz".to_string(), None);

        let i = "[Ordered,Horiz,Id=sum,Columns=10,]test";
        let r: IResult<&str, HashMap<String, Option<String>>> = parse_options_with_opt_value(i);
        assert_eq!(r, Ok(("test", options)));

        let i = "[]";
        let r: IResult<&str, HashMap<String, Option<String>>> = parse_options_with_opt_value(i);
        assert_eq!(r, Ok(("", HashMap::new())));

        let i = "[,]";
        let r: IResult<&str, HashMap<String, Option<String>>> = parse_options_with_opt_value(i);
        assert_eq!(r, Ok(("", HashMap::new())));

        let i = "[Ordered,Horiz,,Id=sum,Columns=10,]test";
        let r: IResult<&str, HashMap<String, Option<String>>> = parse_options_with_opt_value(i);
        assert_eq!(r, Err(Error((",Id=sum,Columns=10,]test", ErrorKind::Char))));

        let i = "[shuffle=false,columns=2";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));

        let i = "[shuffle=$]";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error(("shuffle=$]", ErrorKind::Char))));

        let i = "[shuffle=]";
        let r: IResult<&str, HashMap<String, String>> = parse_options_with_value(i);
        assert_eq!(r, Err(Error(("shuffle=]", ErrorKind::Char))));
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