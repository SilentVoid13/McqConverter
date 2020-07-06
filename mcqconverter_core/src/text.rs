use crate::common::QuestionComment;

use std::collections::HashMap;
use crate::{ConversionTrait, McqConfig, McqInfos};
use std::error::Error;
use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
pub struct TextZone {
    pub comments_and_elements: Vec<TextType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextElement {
    // TODO: Add support for images
    pub styles: HashMap<String, bool>,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TextType {
    Comment(QuestionComment),
    Text(Vec<TextElement>),
}

impl TextZone {
    pub fn new() -> Self {
        TextZone {
            comments_and_elements: vec![],
        }
    }

    pub fn get_text_elements(&self) -> Vec<TextElement> {
        let mut elements = vec![];
        for el in self.comments_and_elements.iter() {
            match el {
                TextType::Text(t) => elements.extend(t.clone().into_iter()),
                _ => (),
            }
        }
        elements
    }

    pub fn get_comment_elements(&self) -> Vec<QuestionComment> {
        let mut elements = vec![];
        for el in self.comments_and_elements.iter() {
            match el {
                TextType::Comment(q) => elements.push(q.clone()),
                _ => (),
            }
        }
        elements
    }

    pub fn value(&self, space: bool) -> String {
        let elements = self.get_text_elements();
        match elements.len() {
            0 => String::new(),
            1 => {
                elements.get(0).unwrap().clone().value
            }
            _ => {
                elements.iter().map(|e| {
                    if space {
                        e.value.clone() + " "
                    }
                    else {
                        e.value.clone()
                    }
                }).collect::<String>()
            }
        }
    }

    pub fn clean_value(&mut self) {
        let mut new_v = vec![];
        for el in self.comments_and_elements.iter() {
            match el {
                TextType::Text(t) => {
                    if !t.is_empty() {
                        new_v.push(el.clone());
                    }
                }
                _ => new_v.push(el.clone()),
            }
        }
        self.comments_and_elements = new_v;
    }

    pub fn trim_value(&mut self) {
        let mut new_v = vec![];
        for el in self.comments_and_elements.clone().into_iter() {
            match el {
                TextType::Text(t) => {
                    let mut elements = vec![];
                    for mut e in t.into_iter() {
                        e.trim_value();
                        if !e.value.is_empty() {
                            elements.push(e);
                        }
                    }
                    if !elements.is_empty() {
                        new_v.push(TextType::Text(elements));
                    }
                }
                _ => new_v.push(el),
            }
        }
        self.comments_and_elements = new_v;
    }

    pub fn is_empty(&self) -> bool {
        self.get_text_elements().is_empty()
    }

    pub fn amctxt_unescape_value(&mut self) {
        for el in self.comments_and_elements.iter_mut() {
            match el {
                TextType::Text(t) => {
                    for e in t.iter_mut() {
                        e.amctxt_unescape_value();
                    }
                }
                _ => ()
            }
        }
    }

    pub fn amctxt_sanitize(&self) -> Self {
        let mut new_v = vec![];
        for el in self.comments_and_elements.clone().into_iter() {
            match el {
                TextType::Text(t) => {
                    let mut elements = vec![];
                    for e in t.into_iter() {
                        elements.push(e.amctxt_sanitize());
                    }
                    new_v.push(TextType::Text(elements));
                }
                _ => new_v.push(el),
            }
        }
        TextZone {
            comments_and_elements: new_v,
        }
    }

    pub fn gift_unescape_value(&mut self) {
        for el in self.comments_and_elements.iter_mut() {
            match el {
                TextType::Text(t) => {
                    for e in t.iter_mut() {
                        e.gift_unescape_value();
                    }
                }
                _ => ()
            }
        }
    }

    pub fn gift_sanitize(&self) -> Self {
        let mut new_v = vec![];
        for el in self.comments_and_elements.clone().into_iter() {
            match el {
                TextType::Text(t) => {
                    let mut elements = vec![];
                    for e in t.into_iter() {
                        elements.push(e.gift_sanitize());
                    }
                    new_v.push(TextType::Text(elements));
                }
                _ => new_v.push(el),
            }
        }
        TextZone {
            comments_and_elements: new_v,
        }
    }
}

impl TextElement {
    pub fn new(text: &str) -> Self {
        TextElement {
            styles: HashMap::new(),
            value: text.to_string(),
        }
    }

    pub fn trim_value(&mut self) {
        self.value = self.value.trim().to_string();
    }

    pub fn gift_unescape_value(&mut self) {
        self.value = self.value.replace("\n", " ");
        self.value = self.value.replace("\\n", "\n");
        self.value = self.value.replace("\\=", "=");
        self.value = self.value.replace("\\~", "~");
        self.value = self.value.replace("\\#", "#");
        self.value = self.value.replace("\\{", "{");
        self.value = self.value.replace("\\}", "}");
        self.value = self.value.replace("\\[", "[");
        self.value = self.value.replace("\\]", "]");
        self.value = self.value.replace("\\:", ":");
    }

    pub fn gift_sanitize(&self) -> Self {
        let mut t_copy = self.clone();
        t_copy.value = t_copy.value.replace("=", "\\=");
        t_copy.value = t_copy.value.replace("~", "\\~");
        t_copy.value = t_copy.value.replace("#", "\\#");
        t_copy.value = t_copy.value.replace("{", "\\{");
        t_copy.value = t_copy.value.replace("}", "\\}");
        t_copy.value = t_copy.value.replace("[", "\\[");
        t_copy.value = t_copy.value.replace("]", "\\]");
        t_copy.value = t_copy.value.replace(":", "\\:");

        t_copy
    }

    // TODO pub fn gift_sanitize(&self) -> Self {}

    pub fn amctxt_unescape_value(&mut self) {
        // TODO: Find a better way to do this
        let re = Regex::new(r"((?:(?:[\t\r ]*)?\n){2,})").unwrap();
        self.value = re.replace_all(&self.value, "LONGSTRINGTOREPLACE").to_string();
        self.value = self.value.replace("\n", " ");
        self.value = self.value.replace("LONGSTRINGTOREPLACE", "\n");
    }

    // TODO: check thoroughly what are the special combinations
    pub fn amctxt_sanitize(&self) -> Self {
        let mut t_copy = self.clone();
        t_copy.value = t_copy.value.replace("[_", "[ _");
        t_copy.value = t_copy.value.replace("[*", "[ *");
        t_copy.value = t_copy.value.replace("[|", "[ |");
        t_copy.value = t_copy.value.replace("[/", "[ /");
        t_copy.value = t_copy.value.replace("[==", "[ ==");
        t_copy.value = t_copy.value.replace("[[", "[ [");
        t_copy.value = t_copy.value.replace("_]", "_ ]");
        t_copy.value = t_copy.value.replace("*]", "* ]");
        t_copy.value = t_copy.value.replace("|]", "| ]");
        t_copy.value = t_copy.value.replace("/]", "/ ]");
        t_copy.value = t_copy.value.replace("==]", "== ]");
        t_copy.value = t_copy.value.replace("]]", "] ]");

        t_copy
    }
}

impl ConversionTrait for TextZone {
    fn convert2amctxt(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        let mut amctxt_text_zone = String::new();
        for (i, el) in self.comments_and_elements.iter().enumerate() {
            match el {
                TextType::Text(t) => {
                    for (j, e) in t.iter().enumerate() {
                        amctxt_text_zone += &e.convert2amctxt(config, infos)?;
                        if j != t.len()-1 {
                            amctxt_text_zone += " ";
                        }
                    }
                }
                TextType::Comment(c) => {
                    amctxt_text_zone += "\n";
                    amctxt_text_zone += &c.convert2amctxt(config, infos)?;
                    if i != self.comments_and_elements.len()-1 {
                        amctxt_text_zone += "\n";
                    }
                }
            }
        }
        Ok(amctxt_text_zone)
    }

    fn convert2gift(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        let mut gift_text_zone = String::new();
        for (i, el) in self.comments_and_elements.iter().enumerate() {
            match el {
                TextType::Text(t) => {
                    for (j, e) in t.iter().enumerate() {
                        gift_text_zone += &e.convert2gift(config, infos)?;
                        if j != t.len()-1 {
                            gift_text_zone += " ";
                        }
                    }
                }
                TextType::Comment(c) => {
                    gift_text_zone += "\n";
                    gift_text_zone += &c.convert2gift(config, infos)?;
                    if i != self.comments_and_elements.len()-1 {
                        gift_text_zone += "\n";
                    }
                }
            }
        }
        Ok(gift_text_zone)
    }
}

impl ConversionTrait for TextElement {
    fn convert2amctxt(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        let mut amctxt_text_elt = String::new();

        // TODO: Maybe add bold or stuff like that
        amctxt_text_elt += &self.value;

        Ok(amctxt_text_elt)
    }

    fn convert2gift(&self, _config: &McqConfig, _infos: &mut McqInfos) -> Result<String, Box<dyn Error>> {
        let mut gift_text_elt = String::new();

        // TODO: Maybe add bold or stuff like that
        // TODO: Add support for verbatim

        gift_text_elt += &self.value;

        Ok(gift_text_elt)
    }
}
