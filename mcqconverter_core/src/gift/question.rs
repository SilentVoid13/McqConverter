use crate::gift::answer::{parse_answers, Answer};
use crate::{ConversionTrait, McqConfig, McqInfos};
use crate::errors::{nom_failure, context_failure, McqConverterError};
use crate::gift::parser::{GiftObject};
use crate::gift::text::{parse_text_zone_until, parse_text_zone_until_unescaped};
use crate::common::{AllElements};
use crate::text::{TextType, TextElement, TextZone};
use crate::utils::{take_opt_sp, take_double_newline_with_sp, eof, take_opt_newline_with_sp};

use std::error::Error;

use nom::IResult;
use nom::sequence::{delimited, tuple};
use nom::character::complete::{char};
use nom::bytes::complete::{tag, take};
use nom::combinator::{opt, map, cut, peek};
use nom::branch::alt;
use nom::error::{context, ErrorKind};

#[derive(Debug, Clone, PartialEq)]
pub struct Question {
    pub question_type: QuestionType,
    pub missing_word: bool,
    pub title: Option<TextZone>,
    pub text: TextZone,
    pub format: Option<QuestionFormat>,
    pub answers: Vec<Answer>,
    pub global_feedback: Option<TextZone>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct QuestionGroup {
    pub category: TextZone,
    pub content: Vec<AllElements<Question, Self>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum QuestionType {
    MultipleChoice,
    TrueFalse,
    ShortAnswer,
    Matching,
    Numerical,
    Essay,
    Description,
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub enum QuestionFormat {
    Html,
    Moodle,
    Plain,
    Markdown,
    Unknown,
}

impl QuestionGroup {
    pub fn full_questions(&self) -> Vec<Question> {
        let mut questions = vec![];
        for e in self.content.iter() {
            if let AllElements::Single(q) = e {
                questions.push(q.clone());
            }
        }
        questions
    }
}

impl Question {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Question {
            question_type: QuestionType::Unknown,
            missing_word: false,
            title: None,
            text: TextZone {
                comments_and_elements: vec![],
            },
            format: None,
            answers: vec![],
            global_feedback: None,
        }
    }

    /// Count the number of correct answers in a question
    pub fn count_correct_answers(&self) -> u32 {
        let mut c = 0;
        for answer in self.answers.iter() {
            if answer.correct == true {
                c+=1;
            }
        }
        c
    }
}

impl ConversionTrait for QuestionGroup {
    fn convert2amctxt(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        infos.current_group += 1;

        let mut amctxt_group = String::new();

        amctxt_group += &format!("*( {}\n\n", self.category.convert2amctxt(config, infos)?);
        for el in self.content.iter() {
            match el {
                AllElements::Single(q) => {
                    amctxt_group += &q.convert2amctxt(config, infos)?;
                }
                AllElements::Comment(c) => {
                    amctxt_group += &c.convert2amctxt(config, infos)?;
                }
                AllElements::Group(_) => {
                    return Err("Unexpected QuestionGroup inside a QuestionGroup".into());
                }
            }
        }
        amctxt_group += "*)\n\n";

        Ok(amctxt_group)
    }

    fn convert2gift(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        Err("QuestionGroup is already in GIFT format".into())
    }
}

impl ConversionTrait for Question {
    fn convert2amctxt(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        infos.current_question += 1;

        match self.question_type {
            QuestionType::ShortAnswer |
            QuestionType::Numerical |
            QuestionType::Matching |
            QuestionType::Description |
            QuestionType::Unknown => {
                infos.log_question_warning(&format!("{:?} Question type is not supported with AMC-TXT format", self.question_type));
                return Ok(String::new());
            }
            _ => ()
        }

        let good_answers = self.count_correct_answers();
        if good_answers == 0 && self.question_type != QuestionType::Essay {
            infos.log_question_warning("This question contains no good answer, it will be ignored. Consider adding at least one good answer");
            return Ok(String::new());
        }
        if self.format.is_some() {
            infos.log_question_warning("Questions formats are not supported with AMC-TXT format, this may cause display errors for this question");
        }
        if self.global_feedback.is_some() {
            infos.log_question_warning("Questions global feedbacks are not supported with AMC-TXT format, it will be ignored for this question");
        }

        let mut amctxt_question = String::new();

        let mut title = String::new();
        if let Some(t) = self.title.as_ref() {
            title.push_str("[==");
            title += &t.convert2amctxt(config, infos)?;
            title.push_str("==]");
        }

        let text = self.text.amctxt_sanitize();

        match self.question_type {
            QuestionType::Essay => {
                // TODO: Add supports for comments
                amctxt_question += &format!(
                    "*<lines=4> {}{}\n\
                    -[O]{{0}} O\n\
                    -[P]{{1}} P\n\
                    +[V]{{2}} V\n\n",
                    title,
                    text.convert2amctxt(config, infos)?,
                );
            },
            QuestionType::TrueFalse => {
                let mut amctxt_answers = String::new();

                // TODO: Add supports for comments
                for answer in self.answers.iter() {
                    if answer.correct {
                        if answer.text.value(true).starts_with("T") {
                            amctxt_answers +=
                                "+ TRUE\n\
                                - FALSE\n";
                        }
                        else {
                            amctxt_answers +=
                                "- TRUE\n\
                                + FALSE\n";
                        }
                    }
                }
                amctxt_question += &format!(
                    "* {}{}\n\
                    {}\n",
                    title,
                    text.convert2amctxt(config, infos)?,
                    amctxt_answers,
                )
            }
            _ => {
                let stars;
                if good_answers > 1 {
                    stars = "**";
                }
                else {
                    stars = "*";
                }

                amctxt_question += &format!("{} {}{}\n", stars, title, text.convert2amctxt(config, infos)?);
                for answer in self.answers.iter() {
                    amctxt_question += &answer.convert2amctxt(config, infos)?;
                }
                amctxt_question.push('\n');
            },
        }

        Ok(amctxt_question)
    }

    fn convert2gift(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        Err("Question is already in GIFT format".into())
    }
}

/// Parse the title (`::title::`)
/// Spaces and newlines are authorized
/// Trim and gift unescape the value
///
/// # Errors
/// - If input doesn't start with `::`
/// # Failures
/// - If EOF is encountered before a closing tag
/// - If the closing tag is different from `::`
/// - If the title is empty
fn parse_title<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextZone, E> {
    let (i, title) = delimited(
        tag("::"),
        context("No matching closing tag for title",
            cut(
                parse_text_zone_until_unescaped(
                    alt((
                        tag("::"),
                        tag("{"),
                        tag("}"),
                        tag("["),
                        tag("]"),
                    )),
                    take(1usize),
                ),
            )
        ),
        context("No matching closing tag for title",
            cut(tag("::"))
        )
    )(i)?;

    // Empty is authorized but is not appreciated by the web interface so that's more a bug than a feature
    // That's why do not authorize it
    if title.is_empty() {
        return Err(context_failure(i, "Title can't be empty", ErrorKind::Not));
    }

    Ok((i, title))
}

/// Parse the format of a question (`[html]`)
/// No spaces or newlines authorized.
/// Trim and gift unescape the value
///
/// # Errors
/// - If input doesn't start with `[`
/// - If format value isn't a valid one
/// - If no closing tag `]` is found
fn parse_format<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, QuestionFormat, E> {
    map(
        delimited(
            char('['),
            alt((
                tag("html"),
                tag("moodle"),
                tag("plain"),
                tag("markdown"),
            )),
            char(']')
        ),
        |f: &str| {
            match f {
                "html" => QuestionFormat::Html,
                "moodle" => QuestionFormat::Moodle,
                "plain" => QuestionFormat::Plain,
                "markdown" => QuestionFormat::Markdown,
                _ => QuestionFormat::Unknown,
            }
        }
    )(i)
}

/// Parse the First part of the question text, until a `{` or double newline or EOF is found
/// Takes the optional trailing `\n`/// Spaces, newlines authorized
/// Trim and gift unescape the value
///
/// # Errors
/// - If text value is empty
fn parse_start_text<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextZone, E> {
    let (i, start_text) = parse_text_zone_until_unescaped(
        alt((
            tag("{"),
            take_double_newline_with_sp,
            eof,
        )),
        // We don't want to use take if the char is a \n or EOF
        alt((
            eof,
            peek(tag("\n")),
            take(1usize),
        )),
    )(i)?;

    // No question text
    if start_text.is_empty() {
        return Err(nom_failure(i, ErrorKind::Tag));
    }
    let (i, _) = opt(char('\n'))(i)?;

    Ok((i, start_text))
}

/// Parse the Last part of the question text
/// Takes the optional trailing `\n`/// Spaces, newlines authorized
/// Trim and gift unescape the value
///
/// # Errors
/// Can't fail, consumes all the input if it has to.
fn parse_end_text<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, TextZone, E> {
    let (i, text) = parse_text_zone_until(
        alt((
            take_double_newline_with_sp::<&'a str, E>,
            eof
        ))
    )(i)?;
    let (i, _) = opt(char('\n'))(i)?;

    Ok((i, text))
}

/// Parse a question
pub fn parse_question<'a, E: McqConverterError<&'a str>>(i: &'a str) -> IResult<&'a str, GiftObject, E> {
    let (i, _) = take_opt_sp(i)?;
    let (i, title) = opt(parse_title)(i)?;

    let (i, _) = take_opt_newline_with_sp(i)?;

    let (i, format) = opt(parse_format)(i)?;

    let (i, mut text) = context("Questions must contain some text",
        parse_start_text
    )(i)?;

    let (i, res) = opt(
        tuple((
            parse_answers,
            opt(parse_end_text),
        ))
    )(i)?;

    let mut answers: Vec<Answer> = Vec::new();
    let question_type;
    let mut missing_word = false;
    let mut global_feedback = None;

    if let Some(((ans, q_type, g_feedback), end_t)) = res {
        answers = ans;
        question_type = q_type;
        global_feedback = g_feedback;
        if let Some(end_text) = end_t {
            if !end_text.is_empty() {
                missing_word = true;
                text.comments_and_elements.push(TextType::Text(vec![TextElement::new(" _____ ")]));
                text.comments_and_elements.push(TextType::Text(vec![TextElement::new(&end_text.value(true))]));
            }
        }
    }
    else {
        question_type = QuestionType::Description;
    }

    let question = Question {
        question_type: question_type,
        missing_word: missing_word,
        title: title,
        text: text,
        format: format,
        answers: answers,
        global_feedback: global_feedback,
    };

    Ok((i, GiftObject::Question(question)))
}











#[cfg(test)]
mod tests {
    use crate::gift::question::{Question, parse_title, parse_format, QuestionFormat, parse_start_text, parse_end_text};
    use crate::gift::answer::Answer;
    use crate::common::QuestionComment;
    use crate::text::{TextElement, TextZone};
    use crate::text::TextType::{Text, Comment};

    use nom::IResult;
    use nom::error::ErrorKind;
    use nom::Err::{Error, Failure};

    #[test]
    fn test_count_good_answers() {
        let mut q = Question::new();
        assert_eq!(0, q.count_correct_answers());

        let mut v = vec![];
        let mut a = Answer::new();
        v.push(a.clone());
        v.push(a.clone());
        a.correct = true;
        v.push(a.clone());
        v.push(a.clone());
        v.push(a.clone());
        q.answers = v;

        assert_eq!(3, q.count_correct_answers());
    }

    #[test]
    fn test_full_questions_group() {
        // TODO
    }

    #[test]
    fn test_convert2amctxt_question() {
        // TODO
    }

    #[test]
    fn test_convert2amctxt_question_group() {
        // TODO
    }

    #[test]
    fn test_title() {
        let i = "::Test\n// comment\nTest::test\ntest";
        let r: IResult<&str, TextZone> = parse_title(i);
        assert_eq!(r, Ok(("test\ntest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("Test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("Test")])
            ],
        }
        )));

        let i = "::Test\n\t//\t comment 1 \t\n//\t comment 2 \t\nTest\t\n ::test\ntest";
        let r: IResult<&str, TextZone> = parse_title(i);
        assert_eq!(r, Ok(("test\ntest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("Test")]),
                Comment(QuestionComment::new("comment 1".to_string())),
                Comment(QuestionComment::new("comment 2".to_string())),
                Text(vec![TextElement::new("Test")])
            ],
        })));

        let i = "::\\{\\}\\[\\]\\\\\\::::test";
        let r: IResult<&str, TextZone> = parse_title(i);
        assert_eq!(r, Ok((":test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("{}[]\\\\:")]),
            ],
        })));

        let i = "test";
        let r: IResult<&str, TextZone> = parse_title(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Tag))));

        let i = "::test";
        let r: IResult<&str, TextZone> = parse_title(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Eof))));

        let i = "::test}";
        let r: IResult<&str, TextZone> = parse_title(i);
        assert_eq!(r, Err(Failure(("}", ErrorKind::Tag))));

        let i = "::\t \n::";
        let r: IResult<&str, TextZone> = parse_title(i);
        assert_eq!(r, Err(Failure(("", ErrorKind::Not))));
    }

    #[test]
    fn test_format() {
        let i = "[html]test\n";
        let r: IResult<&str, QuestionFormat> = parse_format(i);
        assert_eq!(r, Ok(("test\n", QuestionFormat::Html)));

        let i = "[markdown]test\n";
        let r: IResult<&str, QuestionFormat> = parse_format(i);
        assert_eq!(r, Ok(("test\n", QuestionFormat::Markdown)));

        let i = "test";
        let r: IResult<&str, QuestionFormat> = parse_format(i);
        assert_eq!(r, Err(Error(("test", ErrorKind::Char))));

        let i = "[test]test";
        let r: IResult<&str, QuestionFormat> = parse_format(i);
        assert_eq!(r, Err(Error(("test]test", ErrorKind::Tag))));

        let i = "[\tmarkdown]test";
        let r: IResult<&str, QuestionFormat> = parse_format(i);
        assert_eq!(r, Err(Error(("\tmarkdown]test", ErrorKind::Tag))));

        let i = "[html";
        let r: IResult<&str, QuestionFormat> = parse_format(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Char))));
    }

    #[test]
    fn test_start_text() {
        let i = "test\n// comment\ntest\t\n\t\ntest";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("\t\n\t\ntest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "test{}test";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("{}test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "test\\{test{}test";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("{}test", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test{test")]),
            ]
        })));

        let i = "test";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "\\\n\ntest";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("\ntest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("\\")]),
            ]
        })));

        let i = "\\";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("\\")]),
            ]
        })));

        let i = "\\\\";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("\\\\")]),
            ]
        })));

        let i = "";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Err(Error(("", ErrorKind::Many1))));

        let i = "\n\n";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Err(Error(("\n\n", ErrorKind::Many1))));

        let i = "{";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Err(Error(("{", ErrorKind::Many1))));

        let i = "a\\{b\\\\{c";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("{c", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("a{b\\\\")]),
            ]
        })));

        let i = "a\\{b\\\\\\{c{d";
        let r: IResult<&str, TextZone> = parse_start_text(i);
        assert_eq!(r, Ok(("{d", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("a{b\\\\{c")]),
            ]
        })));
    }

    #[test]
    fn test_end_text() {
        let i = "test\n\ntest";
        let r: IResult<&str, TextZone> = parse_end_text(i);
        assert_eq!(r, Ok(("\ntest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "test\n";
        let r: IResult<&str, TextZone> = parse_end_text(i);
        assert_eq!(r, Ok(("", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("test")]),
            ]
        })));

        let i = "a\n\tb\n\tc\n\t\n\ttest";
        let r: IResult<&str, TextZone> = parse_end_text(i);
        assert_eq!(r, Ok(("\t\n\ttest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("a \tb \tc")]),
            ]
        })));

        let i = "a\n\tb\n\t//\tcomment\t\n\tc\n\t//\tcomment2\n\t\n\t\ntest";
        let r: IResult<&str, TextZone> = parse_end_text(i);
        assert_eq!(r, Ok(("\t\n\t\ntest", TextZone {
            comments_and_elements: vec![
                Text(vec![TextElement::new("a \tb")]),
                Comment(QuestionComment::new("comment".to_string())),
                Text(vec![TextElement::new("c")]),
                Comment(QuestionComment::new("comment2".to_string())),
            ]
        })));
    }

    /*
    #[test]
    fn test_question() {
        // TODO
    }

    #[test]
    fn test_description() {
        let mut v = Question {
            question_type: QuestionType::Description,
            missing_word: false,
            title: None,
            text: TextElement {
                comments: vec![],
                value: String::from("test"),
            },
            format: None,
            answers: vec![],
            global_feedback: None
        };

        let i = "test";
        let r: IResult<&str, GiftObject> = parse_question(i);
        assert_eq!(r, Ok(("", GiftObject::Question(v.clone()))));

        let i = "test\n\t\n\tabc";
        let r: IResult<&str, GiftObject> = parse_question(i);
        assert_eq!(r, Ok(("\t\n\tabc", GiftObject::Question(v.clone()))));

        v.text.comments.push("comment".to_string());
        let i = "test\n// comment\n";
        let r: IResult<&str, GiftObject> = parse_question(i);
        assert_eq!(r, Ok(("", GiftObject::Question(v.clone()))));
    }

    #[test]
    fn test_essay() {
        let v = Question {
            question_type: QuestionType::Essay,
            missing_word: false,
            title: None,
            text: TextElement {
                comments: vec![],
                value: String::from("test"),
            },
            format: None,
            answers: vec![],
            global_feedback: None
        };

        let i = "test {}";
        let r: IResult<&str, GiftObject> = parse_question(i);
        assert_eq!(r, Ok(("", GiftObject::Question(v.clone()))));

        let i = "test\n\t{\n\t}\n\t\n\tabc";
        let r: IResult<&str, GiftObject> = parse_question(i);
        assert_eq!(r, Ok(("\t\n\tabc", GiftObject::Question(v.clone()))));
    }
    */
}