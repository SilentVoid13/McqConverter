use clap::{Arg, App};
use std::path::Path;
use std::process;
use std::error::Error;
use mcqconverter_core::{McqConverter, Format, McqConfig};

fn main() {
    let version     = "0.1";
    let author      = "SilentVoid <silentvoid13@protonmail.com>";
    let about       = "A converter from AMC-TXT to GIFT format";

    let matches = App::new("amctxt2gift")
        .version(version)
        .author(author)
        .about(about)
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Sets verbose output")
        )
        .arg(
            Arg::with_name("FILE")
                .index(1)
                .required(true)
                .help("Sets the input file to convert")
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("FILE_PATH")
                .help("Sets the output file name (default: [FILE].gift)")
        )
        .arg(
            Arg::with_name("comment")
                .short("c")
                .long("convert_comments")
                .help("Enables comments conversion (default: not enabled)")
        )
        .get_matches();

    let file = match matches.value_of("FILE") {
        Some(f) => {
            if !Path::new(f).exists() {
                eprintln!("[-] File does not exist");
                process::exit(1);
            }
            String::from(f)
        },
        None => {
            eprintln!("[-] Argument parsing error");
            process::exit(1);
        }
    };

    let output = match matches.value_of("output") {
        Some(o) => {
            String::from(o)
        }
        None => {
            if file.contains(".") {
                let split: Vec<&str> = file.split(".").collect();
                let mut filename = String::new();
                for p in split.iter().take(split.len()-1) {
                    filename.push_str(p);
                    filename.push('.');
                }
                filename += "gift";
                filename
            }
            else {
                file.clone() + ".gift"
            }
        }
    };

    let convert_comments = match matches.occurrences_of("comments") {
        0 => false,
        1 => true,
        _ => {
            eprintln!("[-] Only one '-c' is required");
            process::exit(1);
        }
    };

    let mcq_config = McqConfig {
        input_filename: file,
        convert_comments: convert_comments,
    };

    if let Err(e) = run(mcq_config, output) {
        eprintln!("[-] An error occured : {}", e);
    }
}

pub fn run(mcq_config: McqConfig, output: String) -> Result<(), Box<dyn Error>> {
    let mut converter = McqConverter::new(&mcq_config, Format::AmcTxt)?;
    converter.convert_and_write_to(&output, Format::Gift)?;

    println!("[+] Conversion success ! Converted file saved to {}", output);

    Ok(())
}
