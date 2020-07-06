mod macros;
mod amctxt;
mod gift;
mod utils;
mod errors;
mod text;
mod common;
mod tests;

use std::error::Error;
use std::path::Path;

use amctxt::AmcTxt;
use gift::Gift;
use std::fs::File;
use std::io::Write;
use std::fs;

/// Enum containing the available conversion formats
/// None is not intended to be used by users
#[derive(Debug, PartialEq)]
pub enum Format {
    AmcTxt,
    Gift,
    None
}

/// The core struct of the library
/// The converter attribute will contain an Object that implements the ConversionTrait
/// This is intended to be a cascade implementation, that will use the ConversionTrait recursively
/// on each of the converter's children to convert all the objects to the specified format
pub struct McqConverter
{
    config: McqConfig,
    infos: McqInfos,
    converter: Box<dyn ConversionTrait>,
}

/// Struct containing configuration infos to be passed around conversion methods
#[derive(Clone)]
pub struct McqConfig {
    pub input_filename: String,
    pub convert_comments: bool,
}

/// Struct containing infos to be passed around conversion methods
/// Contains things like warnings, or the current question number
pub struct McqInfos {
    pub current_group: u32,
    pub current_question: u32,
    pub current_answer: u32,
    pub warnings: Vec<String>,
}

/// The core trait of the application
/// Objects implementing this trait have to be able to convert to the available formats (Check Formats enum)
/// Conversion method is called at root by the McqConverter object to initiate the conversion to a specific format
pub trait ConversionTrait {
    fn convert2amctxt(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>>;
    fn convert2gift(&self, config: &McqConfig, infos: &mut McqInfos) -> Result<String, Box<dyn Error>>;
}

impl McqConverter {
    pub fn new(config: &McqConfig, input_format: Format) -> Result<McqConverter, Box<dyn Error>> {
        if !Path::new(&config.input_filename).exists() {
            return Err("[-] Input file doesn't exist".into());
        }

        let data = fs::read_to_string(&config.input_filename)?;
        let data = Self::clean_data(data);

        let converter: Box<dyn ConversionTrait> = match input_format {
            Format::AmcTxt => Box::from(AmcTxt::new(data.as_str())?),
            Format::Gift => Box::from(Gift::new(data.as_str())?),
            _ => {
                return Err("Invalid Format".into());
            }
        };

        let infos = McqInfos {
            current_group: 0,
            current_question: 0,
            current_answer: 0,
            warnings: vec![],
        };

        Ok(McqConverter {
            config: config.clone(),
            infos: infos,
            converter: converter,
        })
    }

    pub fn clean_data(data: String) -> String {
        // https://en.wikipedia.org/wiki/Byte_order_mark
        // UTF8-BOM
        let mut chars = data.chars();
        if let Some('\u{feff}') = chars.next() {
            chars.collect()
        }
        else {
            data
        }
    }

    pub fn convert_to(&mut self, format: Format) -> Result<String, Box<dyn Error>> {
        match format {
            Format::AmcTxt => {
                self.converter.convert2amctxt(&self.config, &mut self.infos)
            },
            Format::Gift => {
                self.converter.convert2gift(&self.config, &mut self.infos)
            },
            _ => {
                return Err("Invalid Format".into());
            }
        }
    }

    pub fn convert_and_write_to(&mut self, output_filename: &str, format: Format) -> Result<(), Box<dyn Error>> {
        let mut file = File::create(output_filename)?;
        let converted_data = self.convert_to(format)?;
        file.write_all(converted_data.as_bytes())?;

        Ok(())
    }

    pub fn get_warnings(&self) -> Vec<String> {
        self.infos.warnings.clone()
    }
}

impl McqInfos {
    pub fn log_answer_warning(&mut self, msg: &str) -> String {
        let w = format!(
            "[-] Answer n° {} of Question n° {} of Group n° {}: {}",
            self.current_answer,
            self.current_question,
            self.current_group,
            msg,
        );
        println!("{}", w);
        self.warnings.push(w.clone());
        w
    }

    pub fn log_question_warning(&mut self, msg: &str) -> String {
        let w = format!(
            "[-] Question n° {} of Group n° {}: {}",
            self.current_question,
            self.current_group,
            msg,
        );
        println!("{}", w);
        self.warnings.push(w.clone());
        w
    }
}
