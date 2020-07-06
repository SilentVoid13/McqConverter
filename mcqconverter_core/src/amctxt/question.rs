use crate::amctxt::answer::{Answer, parse_answers};
use crate::amctxt::amctxt::AmcTxtObject;
use crate::amctxt::parser::{parse_options_with_opt_value};
use crate::errors::{context_failure, McqConverterError};
use crate::amctxt::text::{parse_text_zone};
use crate::text::TextZone;
use crate::common::AllElements;
use crate::{ConversionTrait, McqConfig, McqInfos};
use crate::utils::decimal_digit1;

use std::collections::HashMap;
use std::error::Error;

use nom::IResult;
use nom::error::{ErrorKind};
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::combinator::{opt, cut};
use nom::sequence::{delimited, tuple};
use nom::multi::{separated_list};
use nom::character::complete::{char, alpha1, alphanumeric1};

#[derive(Debug, Clone, PartialEq)]
pub struct Question {
    pub question_type: QuestionType,
    pub text: TextZone,
    pub answers: Vec<Answer>,
    pub options: Option<HashMap<String, Option<String>>>,
    pub open_options: Option<HashMap<String, String>>,
    pub scoring: Option<HashMap<String, f64>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct QuestionGroup {
    pub start_text: Option<TextZone>,
    pub end_text: Option<TextZone>,
    pub options: Option<HashMap<String, String>>,
    pub content: Vec<AllElements<Question, Self>>,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum QuestionType {
    Single,
    Multiple,
    Open
}

impl ConversionTrait for QuestionGroup {
    fn convert2amctxt(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        Err("QuestionGroup is already in AMC-TXT format".into())
    }

    fn convert2gift(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        infos.current_group += 1;

        let mut gift_group = String::new();

        // TODO: Add support for end_text
        if let Some(s) = self.start_text.as_ref() {
            gift_group += &format!("$CATEGORY: {}\n\n", s.convert2gift(config, infos)?);
        }
        for el in self.content.iter() {
            match el {
                AllElements::Single(q) => {
                    gift_group += &q.convert2gift(config, infos)?;
                }
                AllElements::Comment(c) => {
                    gift_group += &c.convert2gift(config, infos)?;
                }
                AllElements::Group(_) => {
                    return Err("Unexpected QuestionGroup inside a QuestionGroup".into());
                }
            }
        }

        Ok(gift_group)
    }
}

impl ConversionTrait for Question {
    fn convert2amctxt(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        Err("Question is already in GIFT format".into())
    }

    fn convert2gift(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        infos.current_question += 1;

        let mut gift_question = String::new();

        let mut text = self.text.gift_sanitize();

        let mut title = String::new();
        let title_zone = text.get_title();
        if !title_zone.is_empty() {
            title += &format!("::\n{}\n::\n", title_zone.convert2gift(config, infos)?);
        }

        match self.question_type {
            // TODO: Add this in answer conversion ?
            QuestionType::Open => {
                gift_question += &format!(
                    "{}{}\n{{}}\n\n",
                    title,
                    text.convert2gift(config, infos)?,
                );
            },
            _ => {
                gift_question += &format!(
                    "{}{}\n{{\n",
                    title,
                    text.convert2gift(config, infos)?
                );
                for answer in self.answers.iter() {
                    gift_question += &answer.convert2gift(config, infos)?;
                }
                gift_question.push_str("}\n\n");
            },
        }

        Ok(gift_question)
    }
}

/// Parse the scoring value for a question (`{b=1,m=-2}`)
/// Turn the key value to lowercase
///
/// # Errors
/// - If the input doesn't start with `{`
/// - If the closing tag `}` is not matched
/// - If value for scoring is not a valid one (not a number, ...)
fn parse_scoring<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, HashMap<String, f64>, E> {
    // TODO: Add other possibilities (https://www.auto-multiple-choice.net/auto-multiple-choice.en/graphical-interface.shtml#bareme)
    let (i, scores) = delimited(
        char('{'),
        separated_list(
            tag(","),
            tuple((
                alpha1,
                char('='),
                opt(char('-')),
                decimal_digit1,
            ))
        ),
        tuple((
            opt(char(',')),
            char('}')
        ))
    )(i)?;

    let mut h = HashMap::new();
    for (key, _, c1, value) in scores.into_iter() {
        let mut v = String::new();
        if let Some(c) = c1 {
            v.push(c);
        }
        v += value;

        let key = key.to_ascii_lowercase();
        let value: f64 = match v.parse() {
            Ok(v) => v,
            Err(_) => {
                return Err(context_failure(i, "Non numerical value", ErrorKind::Digit));
            }
        };
        h.insert(key, value);
    }

    Ok((i, h))
}

/// Parse the options for an open question (`<lines=4>`)
/// Turn the key value to lowercase
///
/// # Errors
/// - If input doesn't start with `<`
/// - If the closing tag `>` is not matched
/// - If open questions is not respected (`key=value`)
pub fn parse_open_options<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, HashMap<String, String>, E> {
    // TODO: Add verifications for open options
    // TODO: See if weird old implementation was useful
    let (i, options) = delimited(
        char('<'),
        separated_list(
            tag(","),
            tuple((
                alpha1,
                char('='),
                alphanumeric1
            ))
        ),
        tuple((
            opt(char(',')),
            char('>'),
        ))
    )(i)?;

    let mut h = HashMap::new();
    for (key, _, value) in options.into_iter() {
        let key = key.to_ascii_lowercase();
        h.insert(key, value.to_string());
    }
    Ok((i, h))
}

pub fn parse_question<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, AmcTxtObject, E> {
    let (i, stars) = alt((
        // Can't change the order
        tag("**"),
        tag("*"),
    ))(i)?;

    let mut question_type = if stars == "**" {
        QuestionType::Multiple
    }
    else {
        QuestionType::Single
    };

    // TODO: Open questions
    let (i, open_options) = opt(parse_open_options)(i)?;
    if open_options.is_some() {
        question_type = QuestionType::Open;
    }
    let (i, options) = opt(parse_options_with_opt_value)(i)?;
    let (i, scoring) = opt(parse_scoring)(i)?;
    let (i, text_zone) = parse_text_zone(i)?;
    let (i, answers) = cut(
        parse_answers
    )(i)?;

    let question = Question {
        question_type: question_type,
        text: text_zone,
        answers: answers,
        options: options,
        open_options: open_options,
        scoring: scoring,
    };

    Ok((i, AmcTxtObject::Question(question)))
}




#[cfg(test)]
mod tests {
    use crate::amctxt::question::{parse_scoring, parse_open_options};

    use std::collections::HashMap;

    use nom::IResult;
    use nom::error::ErrorKind;
    use nom::Err::Error;

    #[test]
    fn test_convert2amctxt_question() {
        // TODO
    }

    #[test]
    fn test_convert2amctxt_question_group() {
        // TODO
    }

    #[test]
    fn test_parse_scoring() {
        let mut options = HashMap::new();
        options.insert("b".to_string(), 1.0);
        options.insert("m".to_string(), -2.5);

        let i = "{b=1,m=-2.5,}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Ok(("test", options)));

        let i = "{}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Ok(("test", HashMap::new())));

        let i = "{,}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Ok(("test", HashMap::new())));

        let i = "{b=1,,m=-2.5,}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Err(Error((",m=-2.5,}test", ErrorKind::Char))));

        let i = "{b=1,m=test,}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Err(Error(("m=test,}test", ErrorKind::Char))));

        let i = "{b=1,m=-2.5,";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));

        let i = "{b=1,m=}";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Err(Error(("m=}", ErrorKind::Char))));
    }

    #[test]
    fn test_parse_open_options() {
        let mut options = HashMap::new();
        options.insert("lines".to_string(), "4".to_string());
        options.insert("lineuptext".to_string(), "test".to_string());
        options.insert("dots".to_string(), "true".to_string());

        let i = "<lines=4,lineuptext=test,dots=true,>test";
        let r: IResult<&str, HashMap<String, String>> = parse_open_options(i);
        assert_eq!(r, Ok(("test", options)));

        let i = "<>test";
        let r: IResult<&str, HashMap<String, String>> = parse_open_options(i);
        assert_eq!(r, Ok(("test", HashMap::new())));

        let i = "<,>test";
        let r: IResult<&str, HashMap<String, String>> = parse_open_options(i);
        assert_eq!(r, Ok(("test", HashMap::new())));

        let i = "<lines=4,lineuptext=test,,dots=true,>test";
        let r: IResult<&str, HashMap<String, String>> = parse_open_options(i);
        assert_eq!(r, Err(Error((",dots=true,>test", ErrorKind::Char))));

        let i = "<lines=4,lineuptext=$,dots=true,>test";
        let r: IResult<&str, HashMap<String, String>> = parse_open_options(i);
        assert_eq!(r, Err(Error(("lineuptext=$,dots=true,>test", ErrorKind::Char))));

        let i = "<lines=4,lineuptext=,dots=true,>test";
        let r: IResult<&str, HashMap<String, String>> = parse_open_options(i);
        assert_eq!(r, Err(Error(("lineuptext=,dots=true,>test", ErrorKind::Char))));

        let i = "<lines=4,lineuptext=test,dots=true,";
        let r: IResult<&str, HashMap<String, String>> = parse_open_options(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));

        let i = "<lines=4,lineuptext=test ,dots=true,>";
        let r: IResult<&str, HashMap<String, String>> = parse_open_options(i);
        assert_eq!(r, Err(Error((" ,dots=true,>", ErrorKind::Char))));
    }

    #[test]
    fn test_parse_question() {
        // TODO
    }
}