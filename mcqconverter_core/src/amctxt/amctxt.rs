use crate::{ConversionTrait, McqConfig, McqInfos};
use crate::amctxt::parser;
use crate::amctxt::question::{Question, QuestionGroup};
use crate::text::TextZone;

use std::error::Error;
use std::collections::HashMap;

use nom::error::{VerboseError, convert_error};
use nom::lib::std::collections::VecDeque;
use crate::common::AllElements;

#[derive(Debug, Clone, PartialEq)]
pub struct AmcTxt {
    pub general_options: HashMap<String, TextZone>,
    pub current_group: VecDeque<QuestionGroup>,
    pub content: Vec<AllElements<Question, QuestionGroup>>,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum AmcTxtObject {
    GeneralOption(String, TextZone),
    Group(TextZone, Option<HashMap<String, String>>, bool),
    Comment(String),
    Question(Question),
    BlankLine,
}

impl AmcTxt {
    pub fn new(data: &str) -> Result<AmcTxt, Box<dyn Error>> {
        let amctxt = match parser::parse_file::<VerboseError<&str>>(data) {
            Ok((_remaining_i, amctxt)) => {
                amctxt
            }
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
        //println!("amctxt: {:#?}", amctxt);

        Ok(amctxt)
    }
}

impl ConversionTrait for AmcTxt {
    fn convert2amctxt(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        Err("File is already in AMC-TXT format".into())
    }

    fn convert2gift(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        let mut gift_data = String::new();

        // TODO: Add support for Numerical, ShortAnswer, Description if possible
        for q in self.content.iter() {
            match q {
                AllElements::Single(question) => {
                    gift_data += &question.convert2gift(config, infos)?;
                },
                AllElements::Group(group) => {
                    gift_data += &group.convert2gift(config, infos)?;
                },
                AllElements::Comment(c) => {
                    gift_data += &c.convert2gift(config, infos)?;
                }
            }
        }
        Ok(gift_data)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_new() {
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
    fn test_convert2gift() {
        // TODO
    }
}