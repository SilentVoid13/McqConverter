use crate::gift::question::QuestionType;
use crate::{ConversionTrait, McqConfig, McqInfos};
use crate::errors::{context_failure, McqConverterError};
use crate::gift::text::{take_opt_newline_with_sp_with_comments, parse_text_zone_until, parse_text_zone_until_unescaped};
use crate::text::{TextType, TextZone, TextElement};
use crate::utils::{take_opt_newline_with_sp, take_double_newline_with_sp, decimal_digit1, take_non_decimal_digit_with_sp};

use std::error::Error;

use nom::error::{ErrorKind, context};
use nom::IResult;
use nom::character::complete::{char, one_of, none_of};
use nom::combinator::{opt, peek, cut, not};
use nom::multi::{fold_many1};
use nom::bytes::complete::{tag, take};
use nom::branch::alt;
use nom::sequence::{preceded, delimited, tuple};

#[derive(Debug, Clone, PartialEq)]
pub struct Answer {
    pub answer_type: QuestionType,
    pub correct: bool,
    pub text: TextZone,
    pub matched: Option<TextZone>,
    pub feedback: Option<TextZone>,
    pub credit: Option<String>,
    pub tolerance: Option<TextZone>,
    pub starting_comments: TextZone,
}

impl Answer {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Answer {
            answer_type: QuestionType::Unknown,
            correct: false,
            text: TextZone::new(),
            matched: None,
            feedback: None,
            credit: None,
            tolerance: None,
            starting_comments: TextZone::new(),
        }
    }

    pub fn valid_credit_value(v: f64) -> bool {
        vec![100.0,90.0,83.33333,80.0,75.0,70.0,66.66667,60.0,50.0,40.0,33.33333,30.0,25.0,20.0,16.66667,14.28571,12.5,11.11111,10.0,5.0,0.0].contains(&v.abs())
    }
}

impl ConversionTrait for Answer {
    fn convert2amctxt(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        infos.current_answer += 1;

        match self.answer_type {
            QuestionType::Matching |
            QuestionType::Description |
            QuestionType::TrueFalse |
            QuestionType::Essay |
            QuestionType::Unknown => {
                let m = infos.log_answer_warning(&format!(
                    "Type {:?} can't be formatted correctly with AMC-TXT format",
                    self.answer_type,
                ));
                return Err(m.into())
            }
            _ => ()
        }

        if self.feedback.is_some() {
            infos.log_answer_warning(
                "Answers feedbacks are not supported with AMC-TXT format, it will be ignored for this answer",
            );
        }

        let sign;
        if self.correct == true {
            sign = "+";
        }
        else {
            sign = "-";
        }

        let mut scoring = String::new();
        if let Some(c) = &self.credit {
            let value: f64 = c.parse()?;
            let value = value / 100.0;
            scoring = format!("{{{}}}", value.to_string());
        }

        let text = self.text.amctxt_sanitize();

        let answer = format!("{}{} {}\n", sign, scoring, text.convert2amctxt(config, infos)?);

        Ok(answer)
    }

    fn convert2gift(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        Err("Answer is already in GIFT format".into())
    }
}

/// Parse the correct or incorrect sign for an answer (`=~`) and returns a boolean
///
/// # Errors
/// - If input doesn't start with `=` or `~`
fn parse_correct<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, bool, E> {
    let (i, c) = one_of("=~")(i)?;
    Ok((i, (c == '=')))
}

/// Parse the credit of an answer (`%-10%`)
/// Spaces, Tabs, Newlines are not authorized
///
/// # Errors
/// - If input doesn't start with `%`
/// # Failures
/// - If empty value
/// - If no closing `%` is found
/// - If credit value is not a valid one (not a digit, ...)
fn parse_credit<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
    let credit_i = i;
    let (i, (n, cred)) = delimited(
        char('%'),
        context("Credit must contain a value",
            cut(
                    tuple((
                        opt(char('-')),
                        decimal_digit1,
                    ))
            )
        ),
        cut(char('%'))
    )(i)?;
    let mut credit = String::new();
    if let Some(c) = n {
        credit.push(c);
    }
    credit.push_str(cred);

    match credit.parse::<f64>() {
        Ok(c) => {
            if !Answer::valid_credit_value(c) {
                return Err(context_failure(credit_i, "Invalid credit value", ErrorKind::AlphaNumeric));
            }
        }
        Err(_) => {
            return Err(context_failure(credit_i, "Invalid credit value", ErrorKind::AlphaNumeric));
        }
    }

    Ok((i, credit))
}

/// Parse the feedback of an answer (`#feedback`)
/// Spaces, tabs and newlines are authorized
/// Trim and gift unescape the value
///
/// # Errors
/// - If input doesn't start with `#`
/// # Failures
/// - If stop tag is not matched
fn parse_feedback<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextZone, E> {
    peek(not(tag("####")))(i)?;

    let(i, feedback) = preceded(
        char('#'),
        cut(parse_text_zone_until_unescaped(
            alt((
                tag("#"),
                tag("}"),
                tag("="),
                tag("~"),
                take_double_newline_with_sp
            )),
            alt((
                peek(tag("\n")),
                take(1usize),
            )),
        ))
    )(i)?;

    Ok((i, feedback))
}

/// Parse the global feedback of an answer (`#### global feedback`)
/// Takes the optional trailing `\n`/// Spaces, tabs and newlines are authorized
/// Trim and gift unescape the value
///
/// # Errors
/// - If input doesn't start with `####`
/// # Failures
/// - If stop tag is not matched
fn parse_global_feedback<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextZone, E> {
    let (i, _) = tag("####")(i)?;
    let (i, global_feedback) = cut(
        parse_text_zone_until_unescaped(
            alt((
                tag("}"),
                take_double_newline_with_sp
            )),
            alt((
                peek(tag("\n")),
                take(1usize),
            ))
        )
    )(i)?;
    let (i, _) = take_opt_newline_with_sp(i)?;

    Ok((i, global_feedback))
}

/// Parse the tolerance of an answer (`:10`)
/// Spaces, tabs and newlines are authorized
/// The numerical value can't be split (with comments or newlines)
/// Trim and gift unescape the value
///
/// # Errors
/// - If input doesn't start with `:`
/// # Failures
/// - If stop tag is not matched
/// - If no closing `#` `=` `}` is found
/// - If tolerance value is not a valid one (not a digit, empty, ...)
/// - If empty value
/// - If more than one tolerance value is found (i.e: can't be splitted with comment)
fn parse_tolerance<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextZone, E> {
    let (i, tolerance) = delimited(
        char(':'),
        context("Expected a digit value",
            cut(
                parse_text_zone_until(
                    alt((
                        take_double_newline_with_sp,
                        take_non_decimal_digit_with_sp,
                    ))
                )
            )
        ),
        context("Unexpected char",
            cut(
                tuple((
                    take_opt_newline_with_sp,
                    peek(
                        one_of("#=}")
                    )
                ))
            )
        )
    )(i)?;
    let len = tolerance.get_text_elements().len();
    if len == 0 {
        return Err(context_failure(i, "Expected a value for tolerance", ErrorKind::Tag));
    }
    if len > 1 {
        return Err(context_failure(i, "Tolerance contains multiple values", ErrorKind::Tag));
    }
    if let Err(_) = tolerance.value(false).parse::<f64>() {
        return Err(context_failure(i, "Invalid tolerance value", ErrorKind::AlphaNumeric));
    }

    Ok((i, tolerance))
}

/// Parse a numerical range notation (`1..5`)
/// The actual value is computed using the middle of the 2 numbers
///
/// # Failures
/// - If the middle tag `..` is not matched
/// - If we have an empty range value
/// - If range contains more than one value
/// - If range value is invalid (not a number, ...)
/// - If the first range value is bigger than the second one
fn parse_range_notation<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, (TextZone, TextZone), E> {
    let (i, (tz1, _, tz2)) = context("Invalid range notation",
        cut(
            tuple((
                parse_text_zone_until_unescaped(
                    alt((
                        tag(".."),
                        tag("="),
                        tag("}"),
                        tag("#"),
                        tag(":"),
                        take_double_newline_with_sp
                    )),
                    alt((
                        peek(tag("\n")),
                        take(1usize),
                    ))
                ),
                tag(".."),
                parse_text_zone_until_unescaped(
                    alt((
                        tag("="),
                        tag("}"),
                        tag("#"),
                        tag(":"),
                        take_double_newline_with_sp
                    )),
                    alt((
                        peek(tag("\n")),
                        take(1usize),
                    ))
                ),
            ))
        )
    )(i)?;

    println!("tz1: {:?}, tz2: {:?}", tz1, tz2);
    let len1 = tz1.get_text_elements().len();
    let len2 = tz2.get_text_elements().len();
    if len1 == 0 || len2 == 0 {
        return Err(context_failure(i, "Range is missing a value", ErrorKind::Tag));
    }
    if len1 > 1 || len2 > 1 {
        return Err(context_failure(i, "Range contains too much values", ErrorKind::Tag));
    }
    let v1 = match tz1.value(false).parse::<f64>() {
        Ok(v) => v,
        Err(_) => {
            return Err(context_failure(i, "Expected a number for first range value", ErrorKind::Tag));
        }
    };
    let v2 = match tz2.value(false).parse::<f64>() {
        Ok(v) => v,
        Err(_) => {
            return Err(context_failure(i, "Expected a number for second range value", ErrorKind::Tag));
        }
    };
    if v1 > v2 || v1 == v2 {
        return Err(context_failure(i, "Invalid range, second value must be greater than the first", ErrorKind::Tag));
    }
    let v = (v1+v2)/2.0;
    let t = v2 - v;

    let mut text_values = vec![];
    for c in tz1.get_comment_elements().into_iter() {
        text_values.push(TextType::Comment(c));
    }
    for c in tz2.get_comment_elements().into_iter() {
        text_values.push(TextType::Comment(c));
    }
    text_values.push(TextType::Text(vec![TextElement::new(&format!("{}", v))]));

    let text_zone = TextZone {
        comments_and_elements: text_values
    };
    let tolerance = TextZone {
        comments_and_elements: vec![TextType::Text(vec![TextElement::new(&format!("{}", t))])],
    };

    Ok((i, (text_zone, tolerance)))
}

/// Parse the match of an answer (`-> match`)
/// Spaces, tabs and newlines are authorized
/// Trim and gift unescape the value
///
/// # Errors
/// - If input doesn't start with `->`
/// # Failures
/// - If stop tag is not matched
/// - If match is empty
fn parse_match<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextZone, E> {
    let(i, matched) = preceded(
        tag("->"),
        cut(
            parse_text_zone_until_unescaped(
                alt((
                    tag("}"),
                    tag("~"),
                    tag("="),
                    take_double_newline_with_sp
                )),
                alt((
                    peek(tag("\n")),
                    take(1usize),
                ))
            )
        )
    )(i)?;
    if matched.is_empty() {
        return Err(context_failure(i, "Match can't be empty", ErrorKind::Not));
    }

    Ok((i, matched))
}

/// Parse either a Multiple, Short or Matching answer
/// Returns the Answer with the guessed QuestionType set
///
/// # Errors
/// - If input doesn't start with `=` or `~`
/// # Failures
/// - If a double newline is present in the answer
/// - If the parsing for the credit, text, match or feedback fails
/// - If the matching answer has an impossible state (see doc)
/// - If an unexpected char is found at the end of the answer
fn parse_other<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, Answer, E> {
    let (i, mut starting_comments) = take_opt_newline_with_sp_with_comments(i)?;

    let correct_i = i;
    let (i, correct) = parse_correct(i)?;

    let (i, s_c) = take_opt_newline_with_sp_with_comments(i)?;
    starting_comments.extend(s_c.into_iter());

    // Not supported by matching
    let credit_i = i;
    let (i, credit) = opt(parse_credit)(i)?;

    let (i, text) = cut(
        parse_text_zone_until_unescaped(
            alt((
                tag(":"),
                tag("#"),
                tag("~"),
                tag("="),
                tag("}"),
                tag("->"),
                take_double_newline_with_sp,
            )),
            alt((
                peek(tag("\n")),
                take(1usize),
            )),
        )
    )(i)?;
    println!("text: {:#?}", text);
    if text.is_empty() {
        return Err(context_failure(i, "Answer must contain some text", ErrorKind::Tag));
    }

    // Only for matched
    let (i, matched) = opt(parse_match)(i)?;
    if matched.is_some() {
        if correct == false {
            return Err(context_failure(correct_i, "Can't have an invalid answer in a matching question, must be '='", ErrorKind::Char));
        }
        if credit.is_some() {
            return Err(context_failure(credit_i, "Credit is not supported by matching questions", ErrorKind::Tag));
        }
    }

    // Not supported by matching
    let (i, feedback) = opt(parse_feedback)(i)?;
    if matched.is_some() && feedback.is_some() {
        return Err(context_failure(i, "Feedback isn't supported by matching answers", ErrorKind::Tag));
    }

    // Sanity check
    context("Expected one of '}', '~', '=', '####'",
        cut(
            peek(
                alt((
                    tag("}"),
                    tag("~"),
                    tag("="),
                    tag("####")
                ))
            )
        )
    )(i)?;

    let question_type;
    if matched.is_some() {
        question_type = QuestionType::Matching;
    }
    else if correct == false {
        question_type = QuestionType::MultipleChoice;
    }
    else {
        question_type = QuestionType::ShortAnswer;
    }

    let answer = Answer {
        answer_type: question_type,
        correct: correct,
        text: text,
        matched: matched,
        feedback: feedback,
        credit: credit,
        tolerance: None,
        starting_comments: TextZone {
            comments_and_elements: starting_comments,
        },
    };

    Ok((i, answer))
}

/// Parse a Numerical answer
///
/// # Errors
/// - If input starts with `####`
/// - If input starts with `}`
/// # Failures
/// - If a double newline is present in the answer
/// - If the parsing for the credit, text, tolerance or feedback fails
/// - If an unexpected char is found at the end of the answer
fn parse_numerical<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, Answer, E> {
    // For global feedback
    not(peek(tag("####")))(i)?;
    // For end of numerical
    peek(none_of("}"))(i)?;

    let (i, mut starting_comments) = take_opt_newline_with_sp_with_comments(i)?;

    let (i, single) = opt(char('='))(i)?;

    let (i, s_c) = take_opt_newline_with_sp_with_comments(i)?;
    starting_comments.extend(s_c.into_iter());

    let (i, credit) = opt(parse_credit)(i)?;

    let text_i = i;
    let (i, mut text) = cut(
        parse_text_zone_until_unescaped(
            alt((
                tag(":"),
                tag("#"),
                tag("="),
                tag("}"),
                take_double_newline_with_sp
            )),
            alt((
                peek(tag("\n")),
                take(1usize),
            )),
        )
    )(i)?;
    let text_value = text.value(false);
    let mut tolerance = None;

    if text_value.contains("..") {
        let (_, (v, t)) = parse_range_notation(text_i)?;
        text = v;
        tolerance = Some(t);
    }
    else {
        let len = text.get_text_elements().len();
        if len == 0 {
            return Err(context_failure(text_i, "Answer must contain some text", ErrorKind::Tag));
        }
        if len > 1 {
            return Err(context_failure(text_i, "Answer contains multiple numerical values", ErrorKind::Tag));
        }

        if let Err(_) = text_value.parse::<f64>() {
            return Err(context_failure(text_i, "Expected a number for a Numerical question", ErrorKind::Tag));
        }
    }

    // When range notation is used, tolerance is still parsed but not used
    let (i, t) = opt(parse_tolerance)(i)?;
    if tolerance.is_none() {
        tolerance = t;
    }

    let (i, feedback) = opt(parse_feedback)(i)?;

    // Sanity checks
    if single.is_none() {
        // No "=", this should be a single answer
        cut(
            peek(
                alt((
                    tag("}"),
                    tag("####"),
                ))
            )
        )(i)?;
    }
    else {
        cut(
            peek(
                alt((
                    tag("="),
                    tag("}"),
                    tag("####"),
                ))
            )
        )(i)?;
    }

    let answer = Answer {
        answer_type: QuestionType::Numerical,
        correct: true,
        text: text,
        matched: None,
        feedback: feedback,
        credit: credit,
        tolerance: tolerance,
        starting_comments: TextZone {
            comments_and_elements: starting_comments,
        }
    };

    Ok((i, answer))
}

/// Parse a TrueFalse answer
///
///
/// # Errors
/// - If input starts with `####`
/// - If a `T`, `F` or `#` is not matched
/// # Failures
/// - If a double newline is present in the answer
/// - If the parsing for the text, or feedback fails
/// - If an unexpected char is found at the end of the answer
fn parse_true_false<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, Answer, E> {
    // For global feedback
    not(peek(tag("####")))(i)?;

    let (i, mut text_elts) = take_opt_newline_with_sp_with_comments(i)?;

    let (i, value) = alt((
        tag("TRUE"),
        tag("FALSE"),
        tag("T"),
        tag("F"),
        peek(tag("#")),
    ))(i)?;
    let mut value = value.to_string();

    let (i, s_c) = take_opt_newline_with_sp_with_comments(i)?;

    // Sanity check
    context("Expected one of '}', '#'",
        cut(
            peek(one_of("}#"))
        )
    )(i)?;

    let mut correct = true;
    if value.as_str() == "#" {
        correct = false;
        value.clear();
    }
    else {
        text_elts.push(TextType::Text(vec![TextElement::new(&value)]));
    }
    text_elts.extend(s_c.into_iter());

    let (i, feedback) = opt(parse_feedback)(i)?;

    let answer = Answer {
        answer_type: QuestionType::TrueFalse,
        correct: correct,
        text: TextZone {
            comments_and_elements: text_elts,
        },
        matched: None,
        feedback: feedback,
        credit: None,
        tolerance: None,
        starting_comments: TextZone {
            comments_and_elements: vec![],
        }
    };

    Ok((i, answer))
}

/// Parse all the answers of a question
///
/// # Errors
/// - If input doesn't start with `{`
/// - If an answer starting tag is not found
/// - If the answer text contains a double newline
/// - If the parsing of an answer fails
/// - If question is not closed with a `}`
pub fn parse_answers<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, (Vec<Answer>, QuestionType, Option<TextZone>), E> {
    let (i, _) = char('{')(i)?;

    let (i, mut starting_comments) = take_opt_newline_with_sp_with_comments(i)?;

    let (i, global_feedback) = opt(parse_global_feedback)(i)?;

    let (i, c) = cut(
        alt((
            char('#'),
            peek(one_of("=~TF}"))
        ))
    )(i)?;

    // Essay question
    if global_feedback.is_some() || c == '}' {
        let (i, _) = cut(
            char('}')
        )(i)?;
        return Ok((i, (vec![], QuestionType::Essay, global_feedback)));
    }

    let parser = match c {
        '#' => {
            parse_numerical
        },
        'T'|'F' => {
            parse_true_false
        },
        '=' | '~' => {
            parse_other
        },
        _ => {
            return Err(context_failure(
                i,
                "Expected one of '#', 'T', 'F', '=', '~'",
                ErrorKind::Char
            ));
        },
    };

    let (i, mut answers) = cut(
        fold_many1(
            parser,
            Vec::new(),
            |mut acc: Vec<_>, answer| {
                acc.push(answer);
                acc
            }
        )
    )(i)?;

    // Checks for error on questions types
    let mut question_type = QuestionType::Unknown;
    for answer in answers.iter() {
        if answer.answer_type == QuestionType::ShortAnswer && question_type == QuestionType::MultipleChoice {
            question_type = QuestionType::MultipleChoice;
        }
        else {
            question_type = answer.answer_type.clone();
        }
    }

    // TODO: check for the whole question set correctness, e.g:
    // - Multiple questions can have only one good answer with "=" or multiple with "~" and credit
    // - ...

    let(i, global_feedback) = opt(parse_global_feedback)(i)?;
    let(i, _) = cut(
        char('}')
    )(i)?;

    // We add the very first comment part to the first answer
    let first_a = answers.get_mut(0).unwrap();
    starting_comments.extend(first_a.starting_comments.comments_and_elements.clone().into_iter());
    first_a.starting_comments.comments_and_elements = starting_comments;

    Ok((i, (answers, question_type, global_feedback)))
}












#[cfg(test)]
mod tests {
    use crate::gift::answer::{parse_correct, parse_credit, parse_feedback, parse_global_feedback, parse_tolerance, parse_match, parse_other, parse_range_notation, parse_numerical, parse_true_false};//, parse_feedback, parse_tolerance, parse_match, parse_global_feedback, parse_other, Answer, parse_numerical, parse_true_false, parse_answers};
    use crate::text::{TextZone, TextElement};
    use crate::text::TextType::{Text, Comment};
    use crate::common::{QuestionComment};
    use crate::gift::Answer;
    use crate::gift::question::QuestionType;
    use crate::tests::{F_SP, SP};

    use nom::IResult;
    use nom::error::ErrorKind;
    use nom::Err::{Error, Failure};

    #[test]
    fn test_convert2amctxt_answer() {
        // TODO
    }

    #[test]
    fn test_parse_correct() {
        let i = "=test";
        let r: IResult<&str, bool> = parse_correct(i);
        assert_eq!(r, Ok(("test", true)));

        let i = "~test";
        let r: IResult<&str, bool> = parse_correct(i,);
        assert_eq!(r, Ok(("test", false)));

        let i = "test";
        let r: IResult<&str, bool> = parse_correct(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::OneOf))));
    }

    #[test]
    fn test_parse_credit() {
        let i = "%-12.5%test";
        let r: IResult<&str, String> = parse_credit(i);
        assert_eq!(r, Ok(("test", String::from("-12.5"))));

        let i = "%57%test";
        let r: IResult<&str, String> = parse_credit(i);
        assert_eq!(r, Err(Failure(("%57%test", ErrorKind::AlphaNumeric))));

        let i = "%test%";
        let r: IResult<&str, String> = parse_credit(i);
        assert_eq!(r, Err(Failure(("test%", ErrorKind::TakeWhile1))));

        let i = "%50";
        let r: IResult<&str, String> = parse_credit(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Char))));
    }

    #[test]
    fn test_parse_feedback() {
        let i = "#test\n// comment\ntest =test";
        let r: IResult<&str, TextZone> = parse_feedback(i);
        assert_eq!(r, Ok(("=test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "#\\#\\}\\~\\=test\n\t//\tcomment\n\ttest\t\n=test";
        let r: IResult<&str, TextZone> = parse_feedback(i);
        assert_eq!(r, Ok(("=test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("#}~=test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "#test\n// comment\ntest\\\n\n";
        let r: IResult<&str, TextZone> = parse_feedback(i);
        assert_eq!(r, Ok(("\n\n", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test\\")]),
            ]
        })));

        let i = "=test";
        let r: IResult<&str, TextZone> = parse_feedback(i);
        assert_eq!(r, Err(Error(("=test", ErrorKind::Char))));

        let i = "#test";
        let r: IResult<&str, TextZone> = parse_feedback(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));

        let i = "####test";
        let r: IResult<&str, TextZone> = parse_feedback(i);
        assert_eq!(r, Err(Error((i, ErrorKind::Not))));
    }

    #[test]
    fn test_parse_global_feedback() {
        let i = "####test\n\t//\tcomment\n\ttest\n\t}\n\ntest";
        let r: IResult<&str, TextZone> = parse_global_feedback(i);
        assert_eq!(r, Ok(("}\n\ntest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "####test~=:\\}test\n\t//\tcomment\n\ttest\n\t}\n\ntest";
        let r: IResult<&str, TextZone> = parse_global_feedback(i);
        assert_eq!(r, Ok(("}\n\ntest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test~=:}test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "####test\n// comment\ntest\\\n\n";
        let r: IResult<&str, TextZone> = parse_global_feedback(i);
        assert_eq!(r, Ok(("\n", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test\\")]),
            ]
        })));

        let i = "#test";
        let r: IResult<&str, TextZone> = parse_global_feedback(i);
        assert_eq!(r, Err(nom::Err::Error(("#test", ErrorKind::Tag))));

        let i = "####test";
        let r: IResult<&str, TextZone> = parse_global_feedback(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));
    }

    #[test]
    fn test_parse_tolerance() {
        let i = ":\n // comment\n3.37\n // comment2\n\t =test";
        let r: IResult<&str, TextZone> = parse_tolerance(i);
        assert_eq!(r, Ok(("=test", TextZone {
            comments_and_elements: vec![
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("3.37")]),
                Comment(QuestionComment::new("comment2".to_string())),
            ]
        })));

        let i = ":\n\t\r//\t\rcomment\t\r\n\t\r3.37\t\r\n //\t\rcomment2\t\r\n\t\r=test";
        let r: IResult<&str, TextZone> = parse_tolerance(i);
        assert_eq!(r, Ok(("=test", TextZone {
            comments_and_elements: vec![
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("3.37")]),
                Comment(QuestionComment::new("comment2".to_string())),
            ]
        })));

        let i = ":3.37\n//comment\n3.38} =test";
        let r: IResult<&str, TextZone> = parse_tolerance(i);
        assert_eq!(r, Err(Failure(("} =test", ErrorKind::Tag))));

        let i = ":test";
        let r: IResult<&str, TextZone> = parse_tolerance(i);
        assert_eq!(r, Err(Failure(("test", ErrorKind::Many1))));

        let i = ":\n//comment \n=test";
        let r: IResult<&str, TextZone> = parse_tolerance(i);
        assert_eq!(r, Err(Failure(("=test", ErrorKind::Tag))));

        let i = ":3.37";
        let r: IResult<&str, TextZone> = parse_tolerance(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));

        let i = "=test";
        let r: IResult<&str, TextZone> = parse_tolerance(i);
        assert_eq!(r, Err(Error(("=test", ErrorKind::Char))));
    }

    #[test]
    fn test_parse_range_notation() {
        let i = "1..6\n=";
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r,Ok((
            "=",
           (TextZone {
                comments_and_elements: vec![
                    Text(vec![TextElement::new("3.5")])
                ],
           },
           TextZone {
               comments_and_elements: vec![
                   Text(vec![TextElement::new("2.5")]),
               ]
           }),
        )));

        let i = &format!(
            "{f_sp}1{f_sp}..{f_sp}6{f_sp}=",
            f_sp = F_SP,
        );
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r, Ok((
           "=",
           (TextZone {
               comments_and_elements: vec![
                   Text(vec![TextElement::new("3.5")])
               ],
           },
           TextZone {
               comments_and_elements: vec![
                   Text(vec![TextElement::new("2.5")]),
               ]
           }),
        )));

        let i = &format!(
            "{f_sp}//{sp}comment{f_sp}1{f_sp}..{f_sp}//{sp}comment2{f_sp}6{f_sp}//{sp}comment3{f_sp}=",
            f_sp = F_SP,
            sp = SP,
        );
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r, Ok((
            "=",
           (TextZone {
               comments_and_elements: vec![
                   Comment(QuestionComment::new("comment".to_string())),
                   Comment(QuestionComment::new("comment2".to_string())),
                   Comment(QuestionComment::new("comment3".to_string())),
                   Text(vec![TextElement::new("3.5")]),
               ],
           },
           TextZone {
               comments_and_elements: vec![
                   Text(vec![TextElement::new("2.5")]),
               ]
           }),
        )));

        let i = "1..\n\n6=";
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r, Err(Failure(("\n\n6=", ErrorKind::Many1))));

        let i = "test..5=";
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r, Err(Failure(("=", ErrorKind::Tag))));

        let i = "1..test=";
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r, Err(Failure(("=", ErrorKind::Tag))));

        let i = "1\n//comment\n1..5=";
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r, Err(Failure(("=", ErrorKind::Tag))));

        let i = "1..5\n//comment\n5=";
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r, Err(Failure(("=", ErrorKind::Tag))));

        let i = "5..1=";
        let r: IResult<&str, (TextZone, TextZone)> = parse_range_notation(i);
        assert_eq!(r, Err(Failure(("=", ErrorKind::Tag))));
    }

    #[test]
    fn test_parse_match() {
        let i = "-> \n//comment\ntest\n//comment2\n=test";
        let r: IResult<&str, TextZone> = parse_match(i);
        assert_eq!(r, Ok(("=test", TextZone {
            comments_and_elements: vec![
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment2".to_string())),
            ]
        })));

        let i = "->\t\r\n\t\r//\t\rcomment\t\r\n\t\rtest\t\r\n\t\r//\t\rcomment2\t\r\n\t\r=test";
        let r: IResult<&str, TextZone> = parse_match(i);
        assert_eq!(r, Ok(("=test", TextZone {
            comments_and_elements: vec![
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment2".to_string())),
            ]
        })));

        let i = "-> \\}\\~\\=test\n\t//\tcomment\n\t =test";
        let r: IResult<&str, TextZone> = parse_match(i);
        assert_eq!(r, Ok(("=test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("}~=test")]),
                Comment(QuestionComment::new("comment".to_string())),
            ]
        })));

        let i = "-> test\\\n\ntesttest";
        let r: IResult<&str, TextZone> = parse_match(i);
        assert_eq!(r, Ok(("\n\ntesttest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test\\")]),
            ]
        })));

        let i = "=test";
        let r: IResult<&str, TextZone> = parse_match(i);
        assert_eq!(r, Err(Error(("=test", ErrorKind::Tag))));

        let i = "-> test";
        let r: IResult<&str, TextZone> = parse_match(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));

        let i = "-> =test";
        let r: IResult<&str, TextZone> = parse_match(i);
        assert_eq!(r, Err(Failure(("=test", ErrorKind::Not))));
    }

    #[test]
    fn test_parse_other() {
        let mut v = Answer {
            answer_type: QuestionType::ShortAnswer,
            correct: true,
            text: TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new("test")]),
                    Comment(QuestionComment::new("comment2".to_string())),
                ]
            },
            matched: None,
            feedback: Some(TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new("test")]),
                    Comment(QuestionComment::new("comment2".to_string())),
                ]
            }),
            credit: Some(String::from("-10")),
            tolerance: None,
            starting_comments: TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("start_comment".to_string())),
                    Comment(QuestionComment::new("start_comment2".to_string())),
                ]
            },
        };

        let i = "\n // start_comment\n=\n//start_comment2\n%-10%\n//comment\ntest\n//comment2\n#\n//comment\ntest\n//comment2\n}";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Ok(("}", v.clone())));

        let i = &format!(
            "{f_sp}//{sp}start_comment{f_sp}={f_sp}//{sp}start_comment2{f_sp}%-10%{f_sp}//{sp}comment{f_sp}test{f_sp}//{sp}comment2{f_sp}#{f_sp}//{sp}comment{f_sp}test{f_sp}//{sp}comment2{f_sp}}}",
            f_sp=F_SP,
            sp=SP
        );
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Ok(("}", v.clone())));

        // Testing type
        v.answer_type = QuestionType::MultipleChoice;
        v.correct = false;
        let i = "\n // start_comment\n~ \n // start_comment2 \n%-10% \n//comment\n test \n//comment2\n # \n//comment\ntest\n//comment2\n}";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Ok(("}", v.clone())));

        // Matching question
        v.answer_type = QuestionType::Matching;
        v.correct = true;
        v.text = TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment2".to_string())),
            ]
        };
        v.matched = Some(TextZone {
            comments_and_elements: vec![
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment2".to_string())),
            ]
        });
        v.feedback = None;
        v.credit = None;

        let i = &format!(
            "{f_sp}//{sp}start_comment{f_sp}={f_sp}//{sp}start_comment2{f_sp}test{f_sp}//{sp}comment2{f_sp}->{f_sp}//{sp}comment{f_sp}test{f_sp}//{sp}comment2{f_sp}}}",
            f_sp=F_SP,
            sp=SP,
        );
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Ok(("}", v.clone())));


        let i = "test";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Err(Error((i, ErrorKind::OneOf))));

        let i = "=\n\n10}";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Err(Failure(("\n\n10}", ErrorKind::Not))));

        let i = "\n\n=10}";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Err(Failure((i, ErrorKind::Not))));

        let i = "\n//comment\n\n=10}";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Err(Failure(("\n\n=10}", ErrorKind::Not))));

        let i = "\n=\n//comment\n\n10}";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Err(Failure(("\n\n10}", ErrorKind::Not))));

        let i = "~b -> c}";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Err(Failure(("~b -> c}", ErrorKind::Char))));

        let i = "=%10%b -> c}";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Err(Failure(("%10%b -> c}", ErrorKind::Tag))));

        let i = "=\n//comment\n10#test\n\n";
        let r: IResult<&str, Answer> = parse_other(i);
        assert_eq!(r, Err(Failure(("\n\n", ErrorKind::Tag))));

        let i = "\n=}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("}", ErrorKind::Many1))));
    }

    #[test]
    fn test_numerical() {
        let mut v = Answer {
            answer_type: QuestionType::Numerical,
            correct: true,
            text: TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new("3.5")]),
                    Comment(QuestionComment::new("comment2".to_string())),
                ]
            },
            matched: None,
            feedback: Some(TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new("test")]),
                    Comment(QuestionComment::new("comment2".to_string())),
                ]
            }),
            credit: Some(String::from("-10")),
            tolerance: Some(TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new("2.5")]),
                    Comment(QuestionComment::new("comment2".to_string())),
                ]
            }),
            starting_comments: TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("start_comment".to_string())),
                    Comment(QuestionComment::new("start_comment2".to_string())),
                ]
            },
        };

        let i = "\n//start_comment\n=\n//start_comment2\n %-10% \n//comment\n3.5\n//comment2\n:\n//comment\n2.5\n//comment2\n#\n//comment\ntest\n//comment2\n}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Ok(("}", v.clone())));

        let i = &format!(
            "{f_sp}//{sp}start_comment{f_sp}={f_sp}//{sp}start_comment2{f_sp}%-10%{f_sp}//{sp}comment{f_sp}3.5{f_sp}//{sp}comment2{f_sp}:{f_sp}//{sp}comment{f_sp}2.5{f_sp}//{sp}comment2{f_sp}#{f_sp}//comment{f_sp}test{f_sp}//{sp}comment2{f_sp}}}",
            f_sp = F_SP,
            sp = SP,
        );
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Ok(("}", v.clone())));

        // Range notation
        v.text = TextZone {
            comments_and_elements: vec![
                Comment(QuestionComment::new("comment".to_string())),
                Comment(QuestionComment::new("comment2".to_string())),
                Text(vec![TextElement::new("3.5")]),
            ]
        };
        v.tolerance = Some(TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("2.5")]),
            ]
        });
        let i = &format!(
            "{f_sp}//{sp}start_comment{f_sp}={f_sp}//{sp}start_comment2{f_sp}%-10%{f_sp}//{sp}comment{f_sp}1{f_sp}..{f_sp}6{f_sp}//{sp}comment2{f_sp}#{f_sp}//comment{f_sp}test{f_sp}//{sp}comment2{f_sp}}}",
            f_sp = F_SP,
            sp = SP,
        );
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Ok(("}", v.clone())));


        let i = "test";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));

        let i = "=\n\n10}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("\n\n10}", ErrorKind::Not))));

        let i = "\n\n=10}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure((i, ErrorKind::Not))));

        let i = "\n//comment\n\n=10}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("\n\n=10}", ErrorKind::Not))));

        let i = "\n=\n//comment\n\n10}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("\n\n10}", ErrorKind::Not))));

        let i = "\n=10\n//comment\n10}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("10\n//comment\n10}", ErrorKind::Tag))));

        let i = "\n=test}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("test}", ErrorKind::Tag))));

        let i = "\n=}";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("}", ErrorKind::Many1))));

        let i = "%-10%10:5#feedback=";
        let r: IResult<&str, Answer> = parse_numerical(i);
        assert_eq!(r, Err(Failure(("=", ErrorKind::Tag))));
    }

    #[test]
    fn test_true_false() {
        let mut v = Answer {
            answer_type: QuestionType::TrueFalse,
            correct: true,
            text: TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new("TRUE")]),
                    Comment(QuestionComment::new("comment2".to_string())),
                ]
            },
            matched: None,
            feedback: Some(TextZone {
                comments_and_elements: vec![
                    Comment(QuestionComment::new("comment".to_string())),
                    Text(vec![TextElement::new("test")]),
                    Comment(QuestionComment::new("comment2".to_string())),
                ]
            }),
            credit: None,
            tolerance: None,
            starting_comments: TextZone {
                comments_and_elements: vec![]
            },
        };

        let i = "\n//comment\nTRUE\n//comment2\n#\n//comment\ntest\n//comment2\n}";
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Ok(("}", v.clone())));

        let i = &format!(
            "{f_sp}//{sp}comment{f_sp}TRUE{f_sp}//{sp}comment2{f_sp}#{f_sp}//{sp}comment{f_sp}test{f_sp}//{sp}comment2{f_sp}}}",
            f_sp = F_SP,
            sp = SP,
        );
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Ok(("}", v.clone())));

        v.correct = false;
        v.text = TextZone::new();
        let i = &format!(
            "#{f_sp}//{sp}comment{f_sp}test{f_sp}//{sp}comment2{f_sp}}}",
            f_sp = F_SP,
            sp = SP,
        );
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Ok(("}", v.clone())));


        let i = "test";
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Err(Error((i, ErrorKind::Tag))));

        let i = "\n\nTRUE}";
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Err(Failure(("\n\nTRUE}", ErrorKind::Not))));

        let i = "TRUE\n\n}";
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Err(Failure(("\n\n}", ErrorKind::Not))));

        let i = "\n//comment\n\nTRUE}";
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Err(Failure(("\n\nTRUE}", ErrorKind::Not))));

        let i = "\nTRUE\n//comment\n\n}";
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Err(Failure(("\n\n}", ErrorKind::Not))));

        let i = "\nTRUE\n//comment\nFALSE\n}";
        let r: IResult<&str, Answer> = parse_true_false(i);
        assert_eq!(r, Err(Failure(("FALSE\n}", ErrorKind::OneOf))));
    }
}