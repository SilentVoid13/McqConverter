use crate::amctxt::parser::{tag_general_option};
use crate::utils::{take_double_newline_with_sp, take_opt_sp, eof, take_opt_newline_with_sp};
use crate::macros::take_until_parsers;
use crate::errors::{context_failure, context_error, McqConverterError};
use crate::text::{TextElement, TextZone, TextType};
use crate::common::QuestionComment;

use nom::IResult;
use nom::error::{context, ErrorKind};
use nom::bytes::complete::{tag, is_not};
use nom::branch::alt;
use nom::combinator::{cut, peek};
use nom::multi::{fold_many0};
use nom::character::complete::{char};
use nom::lib::std::collections::HashMap;

impl TextZone {
    /// Returns the first title occurence it finds
    pub fn get_title(&mut self) -> TextZone {
        let mut elts = vec![];
        let mut matched_title = false;
        for el in self.comments_and_elements.iter_mut() {
            match el {
                TextType::Text(t) => {
                    let mut indexes = vec![];
                    for (i,e) in t.iter().enumerate() {
                        if *e.styles.get("title").unwrap() == true {
                            matched_title = true;
                            indexes.push(i);
                            elts.push(e.clone());
                        }
                        else {
                            if matched_title {
                                break;
                            }
                        }
                    }
                    for i in indexes {
                        t.remove(i);
                    }
                },
                _ => (),
            }
            if matched_title {
                break;
            }
        }
        let text_zone = TextZone {
            comments_and_elements: vec![TextType::Text(elts)],
        };
        self.clean_value();
        text_zone
    }
}

impl TextElement {
    pub fn new_amctxt(text: &str) -> Self {
        TextElement {
            styles: TextElement::default_styles(),
            value: text.to_string(),
        }
    }

    pub fn default_styles() -> HashMap<String, bool> {
        let mut styles = HashMap::new();
        styles.insert("title".to_string(), false);
        styles.insert("verbatim".to_string(), false);
        styles.insert("italic".to_string(), false);
        styles.insert("typewriter".to_string(), false);
        styles.insert("underlined".to_string(), false);
        styles.insert("bold".to_string(), false);
        styles.insert("latex".to_string(), false);
        styles
    }

    pub fn tag2key(tag: &str) -> Result<(String, bool), &'static str> {
        let (key, open) = match tag {
            "[==" | "==]" => {
                ("title".to_string(), (tag == "[=="))
            },
            "[verbatim]" | "[/verbatim]" => {
                ("verbatim".to_string(), (tag == "[verbatim]"))
            },
            "[_" | "_]" => {
                ("italic".to_string(), (tag == "[_"))
            },
            "[|" | "|]" => {
                ("typewriter".to_string(), (tag == "[|"))
            },
            "[/" | "/]" => {
                ("underlined".to_string(), (tag == "[/"))
            },
            "[*" | "*]" => {
                ("bold".to_string(), (tag == "[*"))
            },
            "[[" | "]]" => {
                ("latex".to_string(), (tag == "[["))
            }
            _ => {
                return Err("Unknown tag");
            }
        };
        Ok((key, open))
    }

    pub fn update_status(&mut self, tag: &str) -> Result<(), &'static str> {
        let (key, open) = Self::tag2key(tag)?;
        let value = match self.styles.get(&key) {
            Some(v) => v,
            None => {
                return Err("Unknown error during status update");
            }
        };
        if open == false && *value == false {
            return Err("Unexpected closing tag");
        }
        self.styles.insert(key, open);

        Ok(())
    }

    pub fn has_opened_tag(&self) -> bool {
        for v in self.styles.values() {
            if *v == true {
                return true;
            }
        }
        return false;
    }

    #[allow(dead_code)]
    pub fn opening_tags<'a>() -> Vec<&'a str> {
        vec![
            "[==",
            "[verbatim]",
            "[_",
            "[|",
            "[/",
            "[*",
            "[[",
        ]
    }

    #[allow(dead_code)]
    pub fn closing_tags<'a>() -> Vec<&'a str> {
        vec![
            "==]",
            "[/verbatim]",
            "_]",
            "|]",
            "/]",
            "*]",
            "]]",
        ]
    }
}

/// Match a stop_tag (see doc for a list of stop tags)
///
/// # Errors
/// - If input doesn't start with `\n`
/// - If stop tag is not matched
pub fn stop_tag<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let (i, _) = char('\n')(i)?;
    let (i, _) = take_opt_sp(i)?;

    let (i, d) = alt((
        tag("**"),
        tag("*)"),
        tag("*("),
        tag("*"),
        tag("+"),
        tag("-"),
        // Errors will come from tag_general_option if no stop tag is matched
        tag_general_option,
    ))(i)?;
    Ok((i, d))
}

/// Match a stop comment tag (`\n#`)
///
/// # Errors
/// - If input doesn't start with `\n`
/// - If stop tag `#` is not matched
fn stop_comment_tag<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let (i, _) = char('\n')(i)?;
    let (i, _) = take_opt_sp(i)?;
    tag("#")(i)
}

/// Match a special closing tag for text zones
///
/// # Errors
/// - If no closing tag is found
fn special_closing_tag<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let (i, _) = take_opt_newline_with_sp(i)?;

    let (i, d) = alt((
        tag("==]"),
        tag("[/verbatim]"),
        tag("_]"),
        tag("|]"),
        tag("/]"),
        tag("*]"),
        tag("]]"),
    ))(i)?;
    Ok((i, d))
}


/// Match a special opening tag for text zones
///
/// # Errors
/// - If no opening tag is found
fn special_opening_tag<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let (i, _) = take_opt_newline_with_sp(i)?;

    let (i, d) = alt((
        tag("[=="),
        tag("[verbatim]"),
        tag("[_"),
        tag("[|"),
        tag("[/"),
        tag("[*"),
        tag("[["),
    ))(i)?;
    Ok((i, d))
}

/// Parse a comment, but doesn't consume the trailing `\n`
///
/// # Errors
/// - If the opening tag `#` is not matched
fn parse_comment_raw<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextType, E> {
    let (i, _) = take_opt_newline_with_sp(i)?;
    let (i, _) = tag("#")(i)?;
    let (i, comment) = alt((
        is_not("\n"),
        tag("\n"),
        eof,
    ))(i)?;

    Ok((i, TextType::Comment(
        QuestionComment::new(comment.trim().to_string())
    )))
}

/// Parse a special combination in a text zone (`[* bold text *]`)
/// This is a recursive function that is able to parse nested special combinations
///
/// # Errors
/// - If input doesn't contain a special opening / closing tag
/// # Failures
/// - If an incoherent text state is found (a closing tag with no opening tag)
/// - If a stop tag is found before the special tag is closed
fn parse_special_combination<'a, E: McqConverterError<&'a str>>(text_element: TextElement) -> impl Fn(&'a str) -> IResult<&'a str, TextType, E> {
    move |i: &str| {
        // TODO: Add image support (!uri!)
        // We need to choose this order because [/verbatim] is getting mistaken for [/ (underlined)
        let (i, tag1) = alt((
            special_closing_tag,
            special_opening_tag,
        ))(i)?;

        let mut text_element = text_element.clone();
        let r = text_element.update_status(tag1);
        if let Err(e) = r {
            return Err(context_failure(i, e, ErrorKind::Tag));
        }

        let (i, text) = take_until_parsers(
            false,
           (
                special_opening_tag,
                special_closing_tag,
                stop_tag,
                take_double_newline_with_sp,
                eof
            )
        )(i)?;
        let text = text.trim().to_string();
        text_element.value = text;

        if !text_element.has_opened_tag() {
            return Ok((i, TextType::Text(vec![text_element])));
        }

        // Sanity check
        let (i, _) = context("Unexpected opening or closing tag",
            cut(
                // We need to choose this order because [/verbatim] is getting mistaken for [/ (underlined)
                peek(
                    alt((
                        special_closing_tag,
                        special_opening_tag,
                    ))
                )
            )
        )(i)?;

        let mut v = vec![text_element.clone()];
        let (i, tt) = parse_special_combination(text_element)(i)?;
        if let TextType::Text(new_v) = tt {
            v.extend(new_v.into_iter());
            Ok((i, TextType::Text(v)))
        }
        else {
            Err(context_error(i, "Unexpected TextType value", ErrorKind::Not))
        }
    }
}

/// Parse the text zone until a known pattern is matched
///
/// # Errors
/// - If the pattern is matched right away
fn parse_text_zone_until_pattern<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextType, E> {
    let (i, text) = context("parse_text_zone_until_pattern",
        take_until_parsers(
            true,
            (
                special_opening_tag,
                special_closing_tag,
                stop_tag,
                stop_comment_tag,
                eof,
            )
        )
    )(i)?;
    let text = text.trim().to_string();

    Ok((i, TextType::Text(vec![TextElement::new_amctxt(text.as_str())])))
}

/// Parse a text zone (Question text, Answer text, ...)
/// Trim and amctxt unescape the value
///
/// # Errors
/// - If we get into an infinite loop
pub fn parse_text_zone<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextZone, E> {
    let (i, mut text_zone) = fold_many0(
        alt((
            parse_special_combination(TextElement::new_amctxt("")),
            parse_comment_raw,
            parse_text_zone_until_pattern,
        )),
        TextZone::new(),
        |mut text_zone, text_type| {
            text_zone.comments_and_elements.push(text_type.clone());
            text_zone
        }
    )(i)?;
    text_zone.trim_value();
    text_zone.amctxt_unescape_value();

    Ok((i, text_zone))
}





#[cfg(test)]
mod tests {
    use crate::amctxt::text::{stop_tag, stop_comment_tag, parse_special_combination, parse_text_zone_until_pattern, parse_comment_raw, parse_text_zone};
    use crate::text::{TextType, TextElement, TextZone};
    use crate::common::QuestionComment;
    use crate::text::TextType::{Text, Comment};

    use nom::Err::{Error, Failure};
    use nom::IResult;
    use nom::error::ErrorKind;

    #[test]
    fn test_stop_tag() {
        let i = "\n\t\r **test";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Ok(("test", "**")));

        let i = "\n*(test";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Ok(("test", "*(")));

        let i = "\n*)test";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Ok(("test", "*)")));

        let i = "\n*test";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Ok(("test", "*")));

        let i = "\n+test";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Ok(("test", "+")));

        let i = "\n-test";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Ok(("test", "-")));

        let i = "\ntest:test";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Ok(("test", "test")));

        let i = "test";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Char))));

        let i = "\ntest";
        let r: IResult<&str, &str> = stop_tag(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));
    }

    #[test]
    fn test_stop_comment_tag() {
        let i = "\n\t\r #test";
        let r: IResult<&str, &str> = stop_comment_tag(i);
        assert_eq!(r, Ok(("test", "#")));

        let i = "test";
        let r: IResult<&str, &str> = stop_comment_tag(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Char))));

        let i = "\ntest";
        let r: IResult<&str, &str> = stop_comment_tag(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_comment_raw() {
        let i = "\n\t\r#comment\t\n\ttest";
        let r: IResult<&str, TextType> = parse_comment_raw(i);
        assert_eq!(r, Ok(("\n\ttest", TextType::Comment(QuestionComment::new("comment".to_string())))));

        let i = "\n\t\r#comment";
        let r: IResult<&str, TextType> = parse_comment_raw(i);
        assert_eq!(r, Ok(("", TextType::Comment(QuestionComment::new("comment".to_string())))));

        let i = "\n\t\r#\ntest";
        let r: IResult<&str, TextType> = parse_comment_raw(i);
        assert_eq!(r, Ok(("test", TextType::Comment(QuestionComment::new("".to_string())))));

        let i = "test";
        let r: IResult<&str, TextType> = parse_comment_raw(i);
        assert_eq!(r, Err(Error((i, ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_special_combination() {
        let mut t1 = TextElement::new_amctxt("test");
        let i = "[* test [_ test [/ test /] test _] test *]";
        let r: IResult<&str, TextType> = parse_special_combination(TextElement::new_amctxt(""))(i);
        assert_eq!(r, Ok(("", TextType::Text(vec![
            {
                t1.styles.insert("bold".to_string(), true);
                t1.clone()
            },
            {
                t1.styles.insert("italic".to_string(), true);
                t1.clone()
            },
            {
                t1.styles.insert("underlined".to_string(), true);
                t1.clone()
            },
            {
                t1.styles.insert("underlined".to_string(), false);
                t1.clone()
            },
            {
                t1.styles.insert("italic".to_string(), false);
                t1.clone()
            },
            {
                t1.styles.insert("bold".to_string(), false);
                t1.value.clear();
                t1.clone()
            },
        ]))));

        let mut t1 = TextElement::new_amctxt("test");
        let i = "[*\n\t\rtest\n\t\r[_\n\t\rtest\n\t\r[/\n\t\rtest\n\t\r/]\n\t\rtest\n\t\r_]\n\t\rtest\n\t\r*]";
        let r: IResult<&str, TextType> = parse_special_combination(TextElement::new_amctxt(""))(i);
        assert_eq!(r, Ok(("", TextType::Text(vec![
            {
                t1.styles.insert("bold".to_string(), true);
                t1.clone()
            },
            {
                t1.styles.insert("italic".to_string(), true);
                t1.clone()
            },
            {
                t1.styles.insert("underlined".to_string(), true);
                t1.clone()
            },
            {
                t1.styles.insert("underlined".to_string(), false);
                t1.clone()
            },
            {
                t1.styles.insert("italic".to_string(), false);
                t1.clone()
            },
            {
                t1.styles.insert("bold".to_string(), false);
                t1.value.clear();
                t1.clone()
            },
        ]))));

        let mut t2 = TextElement::new_amctxt("test");
        let i = "[* test [_ test [/ test _] test *] test /]";
        let r: IResult<&str, TextType> = parse_special_combination(TextElement::new_amctxt(""))(i);
        assert_eq!(r, Ok(("", TextType::Text(vec![
            {
                t2.styles.insert("bold".to_string(), true);
                t2.clone()
            },
            {
                t2.styles.insert("italic".to_string(), true);
                t2.clone()
            },
            {
                t2.styles.insert("underlined".to_string(), true);
                t2.clone()
            },
            {
                t2.styles.insert("italic".to_string(), false);
                t2.clone()
            },
            {
                t2.styles.insert("bold".to_string(), false);
                t2.clone()
            },
            {
                t2.styles.insert("underlined".to_string(), false);
                t2.value.clear();
                t2.clone()
            },
        ]))));

        // Unclosed tag
        let i = "[* test [_ test [/ test _] test *] test \n**";
        let r: IResult<&str, TextType> = parse_special_combination(TextElement::new_amctxt(""))(i);
        assert_eq!(r, Err(Failure(("**", ErrorKind::Tag))));

        // Double closing tag
        let i = "[* test [_ test [/ test _] test _] test *] test _] \n**";
        let r: IResult<&str, TextType> = parse_special_combination(TextElement::new_amctxt(""))(i);
        assert_eq!(r, Err(Failure((" test *] test _] \n**", ErrorKind::Tag))));

        // Double newline (forbidden)
        let i = "[* test [_ test [/ test \n\n /] test _] test *]";
        let r: IResult<&str, TextType> = parse_special_combination(TextElement::new_amctxt(""))(i);
        assert_eq!(r, Err(Failure(("\n /] test _] test *]", ErrorKind::Tag))));

        let i = "test";
        let r: IResult<&str, TextType> = parse_special_combination(TextElement::new_amctxt(""))(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Tag))));

        let i = "*]test";
        let r: IResult<&str, TextType> = parse_special_combination(TextElement::new_amctxt(""))(i);
        assert_eq!(r, Err(Failure(("test", ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_text_zone_until_pattern() {
        let i = "test \n** test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(i);
        assert_eq!(r, Ok(("\n** test", TextType::Text(vec![TextElement::new_amctxt("test")]))));

        let i = "test \n# test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(i);
        assert_eq!(r, Ok(("\n# test", TextType::Text(vec![TextElement::new_amctxt("test")]))));

        let i = "test *] test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(i);
        assert_eq!(r, Ok((" *] test", TextType::Text(vec![TextElement::new_amctxt("test")]))));

        let i = "test [* test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(i);
        assert_eq!(r, Ok((" [* test", TextType::Text(vec![TextElement::new_amctxt("test")]))));

        let i = "test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(i);
        assert_eq!(r, Ok(("", TextType::Text(vec![TextElement::new_amctxt("test")]))));

        let i = "\n **test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(i);
        assert_eq!(r, Err(Error(("\n **test", ErrorKind::Eof))));

        let i = "*]test";
        let r: IResult<&str, TextType> = parse_text_zone_until_pattern(i);
        assert_eq!(r, Err(Error(("*]test", ErrorKind::Eof))));
    }

    #[test]
    fn test_parse_text_zone() {
        let i = "test\n# comment\n test \n** test";
        let r: IResult<&str, TextZone> = parse_text_zone(i);
        assert_eq!(r, Ok(("\n** test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new_amctxt("test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new_amctxt("test")]),
            ]
        })));

        let i = "test\n\t\r\n\t\r\n\t\rtest\n\t\r#\t\rcomment\n\t\rtest\n\t\r**\n\t\rtest";
        let r: IResult<&str, TextZone> = parse_text_zone(i);
        assert_eq!(r, Ok(("\n\t\r**\n\t\rtest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new_amctxt("test\n\t\rtest")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new_amctxt("test")]),
            ]
        })));

        let i = "";
        let r: IResult<&str, TextZone> = parse_text_zone(i);
        assert_eq!(r, Ok(("", TextZone {
            comments_and_elements: vec![],
        })));
    }
}