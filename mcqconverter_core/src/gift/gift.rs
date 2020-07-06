use std::error::Error;

use crate::gift::parser;
use crate::gift::question::{Question, QuestionGroup};
use crate::{ConversionTrait, McqConfig, McqInfos};

use nom::error::{VerboseError, convert_error};
use crate::common::AllElements;

#[derive(Debug, Clone, PartialEq)]
pub struct Gift {
    // TODO: Try to remove this tmp variable
    pub current_category: Option<QuestionGroup>,
    pub content: Vec<AllElements<Question, QuestionGroup>>,
}

impl Gift {
    pub fn new(data: &str) -> Result<Gift, Box<dyn Error>> {
        let gift = match parser::parse_file::<VerboseError<&str>>(data) {
            Ok((_remaining_i, gift)) => {
                gift
            },
            Err(e) => {
                match e {
                    nom::Err::Error(e) | nom::Err::Failure(e) => {
                        return Err(convert_error(data, e).into());
                    },
                    nom::Err::Incomplete(_) => {
                        return Err("Incomplete parsing".into());
                    }
                }
            }
        };
        //println!("Gift: {:#?}", gift);

        Ok(gift)
    }

    /// Returns all the Question in a single Vector
    #[allow(dead_code)]
    pub fn full_questions(&self) -> Vec<Question> {
        let mut questions = Vec::new();
        for q in self.content.iter() {
            match q {
                AllElements::Single(s) => {
                    questions.push(s.clone());
                },
                AllElements::Group(g) => {
                    questions.extend(g.full_questions());
                },
                AllElements::Comment(_) => ()
            }
        }
        questions
    }
}

impl ConversionTrait for Gift {
    fn convert2amctxt(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        let mut amctxt_data = String::new();

        // TODO: Add support for Numerical, ShortAnswer, Description if possible
        for q in self.content.iter() {
            match q {
                AllElements::Single(question) => {
                    amctxt_data += &question.convert2amctxt(config, infos)?;
                },
                AllElements::Group(group) => {
                    amctxt_data += &group.convert2amctxt(config, infos)?;
                },
                AllElements::Comment(comment) => {
                    if config.convert_comments {
                        amctxt_data += &comment.convert2amctxt(config, infos)?;
                    }
                }
            }
        }
        Ok(amctxt_data)
    }

    fn convert2gift(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        Err("File is already in GIFT format".into())
    }
}









#[cfg(test)]
mod tests {
    use crate::gift::parser;
    use crate::{ConversionTrait, McqConfig, McqInfos};

    use nom::error::VerboseError;
    use crate::tests::{F_SP, SP};

    #[test]
    fn test_new() {
        // TODO
    }

    #[test]
    fn test_full_questions() {
        // TODO
    }

    #[test]
    fn test_sanitize_special_chars() {
        // TODO
    }

    #[test]
    fn test_unescape_special_chars() {
        // TODO
    }

    #[test]
    fn test_convert2amctxt() {
        // Multiple choice
        let i = format!(
"::{f_sp}title{f_sp}::{f_sp}test{f_sp}test{f_sp}
{{{f_sp}
    ={f_sp}%50%{f_sp}a{f_sp}#{f_sp}f1{f_sp}
    ~{f_sp}%-12.5%{f_sp}b{f_sp}#{f_sp}f2{f_sp}
    ~{f_sp}c{f_sp}#{f_sp}f3{f_sp}
    ####{f_sp}f4{f_sp}
}",
            f_sp = F_SP,
            sp = SP,
        );
        let config = McqConfig {
            input_filename: String::new(),
            convert_comments: false,
        };
        let mut infos = McqInfos {
            current_group: 0,
            current_question: 0,
            current_answer: 0,
            warnings: vec![],
        };

        let (_, gift) = parser::parse_file::<VerboseError<&str>>(i).unwrap();
        let r = gift.convert2amctxt(&config, &mut infos).unwrap();

        assert_eq!(r, String::from(
"* [==title==]test test
+{0.5} a
-{-0.125} b
- c

"));

        // True False
        let i =
"::abc:: a b c
{T#f1####f2}";

        let (_, gift) = parser::parse_file::<VerboseError<&str>>(i).unwrap();
        let r = gift.convert2amctxt(&config, &mut infos).unwrap();

        assert_eq!(r, String::from(
"* [==abc==]a b c
+ TRUE
- FALSE

"));

        // Essay
        let i =
"::abc:: a b c
{####f1}";

        let (_, gift) = parser::parse_file::<VerboseError<&str>>(i).unwrap();
        let r = gift.convert2amctxt(&config, &mut infos).unwrap();

        assert_eq!(r, String::from(
"*<lines=4> [==abc==]a b c
-[O]{0} O
-[P]{1} P
+[V]{2} V

"));

        let i =
"::abc:: a b c
{}";

        let (_, gift) = parser::parse_file::<VerboseError<&str>>(i).unwrap();
        let r = gift.convert2amctxt(&config, &mut infos).unwrap();

        assert_eq!(r, String::from(
            "*<lines=4> [==abc==]a b c
-[O]{0} O
-[P]{1} P
+[V]{2} V

"));
    }
}


