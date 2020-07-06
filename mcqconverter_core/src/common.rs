use crate::{ConversionTrait, McqConfig, McqInfos};

use std::error::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum AllElements<Q, QG> {
    Single(Q),
    Group(QG),
    Comment(QuestionComment),
}

#[derive(Debug, Clone, PartialEq)]
pub struct QuestionComment {
    pub value: String,
}

impl QuestionComment {
    pub fn new(s: String) -> Self {
        QuestionComment {
            value: s,
        }
    }
}

impl ConversionTrait for QuestionComment {
    fn convert2amctxt(&self, config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        if config.convert_comments {
            let amctxt_comment = format!("# {}\n", self.value);
            Ok(amctxt_comment)
        }
        else {
            Ok(String::new())
        }
    }

    fn convert2gift(&self, config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        if config.convert_comments {
            let gift_comment = format!("// {}", self.value);
            Ok(gift_comment)
        }
        else {
            Ok(String::new())
        }
    }
}
