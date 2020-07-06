use crate::utils::{take_opt_newline_with_sp, decimal_digit1};
use crate::errors::{context_failure, McqConverterError};
use crate::amctxt::text::{parse_text_zone};
use crate::text::TextZone;
use crate::{ConversionTrait, McqConfig, McqInfos};
use crate::gift::Answer as GiftAnswer;

use std::collections::HashMap;
use std::error::Error;

use nom::IResult;
use nom::error::{ErrorKind};
use nom::character::complete::{char, one_of, alpha1};
use nom::multi::{fold_many_m_n, separated_list};
use nom::combinator::opt;
use nom::sequence::{delimited, tuple};
use nom::bytes::complete::tag;

#[derive(Debug, Clone, PartialEq)]
pub struct Answer {
    pub correct: bool,
    pub text: TextZone,
    pub scoring: Option<HashMap<String, f64>>,
}

impl ConversionTrait for Answer {
    fn convert2amctxt(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        Err("Answer is already in AMC-TXT format".into())
    }

    fn convert2gift(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        infos.current_answer += 1;

        let sign;
        if self.correct == true {
            sign = "=";
        }
        else {
            sign = "~";
        }

        // TODO: Maybe improve this
        let mut scoring = String::new();
        if let Some(h) = self.scoring.as_ref() {
            // In GIFT we have only value for scoring.
            // If absolute value is specified, that's the easiest option for us
            // If not we will use the "b" option first or the "m" if "b" is not specified

            let absolute_score= h.get("absolute_score");
            let correct_a = h.get("b");
            let incorrect_a = h.get("m");
            if let Some(value) = absolute_score {
                let value = value * 100.0;
                if GiftAnswer::valid_credit_value(value) {
                    scoring += &format!("%{}%", value);
                }
            }
            else if let Some(value) = correct_a {
                let value = value * 100.0;
                if GiftAnswer::valid_credit_value(value) {
                    scoring += &format!("%{}%", value);
                }
            }
            else if let Some(value) = incorrect_a {
                let value = value * 100.0;
                if GiftAnswer::valid_credit_value(value) {
                    scoring += &format!("%{}%", value);
                }
            }
        }
        let text = self.text.gift_sanitize();

        let answer = format!("{}{} {}\n", sign, scoring, text.convert2gift(config, infos)?);

        Ok(answer)
    }


}

/// Parse the correct or incorrect sign for an answer (`+-`) and returns a boolean
///
/// # Errors
/// - If input doesn't start with `+` or `-`
fn parse_correct<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, bool, E> {
    let (i, c) = one_of("+-")(i)?;
    Ok((i, (c == '+')))
}


/// Parse the optional option for an answer (`[test]`)
/// Returns the lowercase option value
///
/// # Errors
/// - If input doesn't start with `[`
/// - If closing tag `]` is not matched
/// - If option value contains non alpha chars
fn parse_options<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
    let (i, o) = delimited(
        char('['),
        alpha1,
        char(']'),
    )(i)?;
    Ok((i, o.to_ascii_lowercase()))
}

/// Parse the scoring value for an answer (`{b=1,m=-2}`)
/// This can contain just the raw score if the answer is ticked (`{2}`)
/// Turn the key value to lowercase
///
/// # Errors
/// - If the input doesn't start with `{`
/// - If the key value is not one of `bmBM`
/// - If the closing tag `}` is not matched
/// - If value for scoring is not a valid one (not a number, ...)
fn parse_scoring<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, HashMap<String, f64>, E> {
    // TODO: Add other possibilities (https://www.auto-multiple-choice.net/auto-multiple-choice.en/graphical-interface.shtml#bareme)
    // TODO: Check for valid cases (can't use absolute_score if this is a multiple question)
    let (i, scores) = delimited(
        char('{'),
        separated_list(
            tag(","),
            tuple((
                opt(
                    tuple((
                        one_of("bmBM"),
                        char('='),
                    ))
                ),
                opt(char('-')),
                decimal_digit1,
            )),
        ),
        tuple((
            opt(char(',')),
            char('}'),
        ))
    )(i)?;

    let mut h = HashMap::new();
    for (o, c1, value) in scores.into_iter() {
        let mut v = String::new();
        if let Some(c) = c1 {
            v.push(c);
        }
        v += value;
        let value: f64 = match v.parse() {
            Ok(v) => v,
            Err(_) => {
                return Err(context_failure(i, "Non numerical value", ErrorKind::Digit));
            }
        };

        if let Some((key, _)) = o {
            let key = key.to_ascii_lowercase();
            h.insert(key.to_string(), value);
        }
        else {
            h.insert(String::from("absolute_score"), value);
        }
    }

    Ok((i, h))
}

fn parse_answer<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, Answer, E> {
    let (i, _) = take_opt_newline_with_sp(i)?;
    let (i, correct) = parse_correct(i)?;
    // TODO: Understand what are answer options and if that's useful
    let (i, _) = opt(parse_options)(i)?;
    let (i, scoring) = opt(parse_scoring)(i)?;
    let (i, text_zone) = parse_text_zone(i)?;

    let answer = Answer {
        correct: correct,
        text: text_zone,
        scoring: scoring,
    };
    //println!("{:#?}", answer);

    Ok((i, answer))
}

pub fn parse_answers<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<Answer>, E> {
    let (i, _) = take_opt_newline_with_sp(i)?;
    // TODO: See max number of answers
    let (i, answers) = fold_many_m_n(
        2,
        50,
        parse_answer,
        Vec::new(),
        |mut acc, item| {
            acc.push(item);
            acc
        }
    )(i)?;

    Ok((i, answers))
}







#[cfg(test)]
mod tests {
    use crate::amctxt::answer::{parse_correct, parse_options, parse_scoring};

    use std::collections::HashMap;

    use nom::IResult;
    use nom::error::ErrorKind;
    use nom::Err::Error;

    #[test]
    fn test_convert2amctxt_answer() {
        // TODO
    }

    #[test]
    fn test_parse_correct() {
        let i = "+test";
        let r: IResult<&str, bool> = parse_correct(i);
        assert_eq!(r, Ok(("test", true)));

        let i = "-test";
        let r: IResult<&str, bool> = parse_correct(i);
        assert_eq!(r, Ok(("test", false)));

        let i = "test";
        let r: IResult<&str, bool> = parse_correct(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::OneOf))));
    }

    #[test]
    fn test_parse_options() {
        let i = "[test]test";
        let r: IResult<&str, String> = parse_options(i);
        assert_eq!(r, Ok(("test", "test".to_string())));

        let i = "[test";
        let r: IResult<&str, String> = parse_options(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));

        let i = "[1337";
        let r: IResult<&str, String> = parse_options(i);
        assert_eq!(r, Err(Error(("1337", ErrorKind::Alpha))));

        let i = "test";
        let r: IResult<&str, String> = parse_options(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Char))));
    }

    #[test]
    fn test_parse_scoring() {
        let mut options = HashMap::new();
        options.insert("b".to_string(), 1.0);
        options.insert("m".to_string(), -2.5);

        let mut options2 = HashMap::new();
        options2.insert("absolute_score".to_string(), -2.5);

        let i = "{b=1,m=-2.5,}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Ok(("test", options)));

        let i = "{-2.5}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Ok(("test", options2)));

        let i = "{}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Ok(("test", HashMap::new())));

        let i = "{,}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Ok(("test", HashMap::new())));

        let i = "{test}test";
        let r: IResult<&str, HashMap<String, f64>> = parse_scoring(i);
        assert_eq!(r, Err(Error(("test}test", ErrorKind::Char))));

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
    fn test_parse_answer() {
        // TODO
    }

    #[test]
    fn test_parse_answers() {
        // TODO
    }
}
