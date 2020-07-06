#![windows_subsystem = "windows"]

use mcqconverter_core::{McqConverter, Format as McqFormat, McqConfig};

use druid::im::{vector,Vector};
use druid::widget::{Button, Flex, Label, RadioGroup, Scroll, List};
use druid::{commands, AppDelegate, AppLauncher, Command, Data, Lens, DelegateCtx, Env, FileDialogOptions, FileSpec, LocalizedString, Target, Widget, WindowDesc, WidgetExt, EventCtx, Color};

use std::fs;
use std::path::Path;
use std::ffi::OsString;

const DEFAULT_INPUT_PATH: &'static str = "No input file selected";

struct Delegate;

#[derive(Clone, Debug, PartialEq, Data)]
pub enum Format {
    AmcTxt,
    Gift,
}

#[derive(Clone, Debug, Data, Lens)]
struct McqConverterConfig {
    input_format: Format,
    output_format: Format,
    input_path: String,
    output_path: String,
    output_text: Vector<String>,
    converted_data: String,
}

impl AppDelegate<McqConverterConfig> for Delegate {
    fn command(
        &mut self,
        _ctx: &mut DelegateCtx,
        _target: Target,
        cmd: &Command,
        data: &mut McqConverterConfig,
        _env: &Env,
    ) -> bool {
        if let Some(Some(file_info)) = cmd.get(commands::SAVE_FILE) {
            let file_path = file_info.path().to_string_lossy().to_string();
            data.output_path = file_path;
            if let Err(e) = fs::write(file_info.path(), &data.converted_data[..]) {
                data.output_text.clear();
                data.output_text.push_back(format!("[-] An error occured: {}", e));
                return false;
            }
            data.output_text.push_back(format!("[+] Conversion success ! Converted file saved to {}", data.output_path));

            return true;
        }
        if let Some(file_info) = cmd.get(commands::OPEN_FILE) {
            let file_path = file_info.path().to_string_lossy().to_string();
            data.input_path = file_path;

            return true;
        }
        false
    }
}

fn convert_file(ctx: &mut EventCtx, data: &mut McqConverterConfig, _env: &Env) {
    data.output_text.clear();

    if data.input_path == DEFAULT_INPUT_PATH {
        data.output_text.push_back("[-] No input file !".to_string());
        return;
    }

    let mcq_config = McqConfig {
        input_filename: data.input_path.clone(),
        convert_comments: false,
    };

    let mcq_format_input = match data.input_format {
        Format::Gift => McqFormat::Gift,
        Format::AmcTxt => McqFormat::AmcTxt
    };
    let mcq_format_output = match data.output_format {
        Format::Gift => McqFormat::Gift,
        Format::AmcTxt => McqFormat::AmcTxt
    };
    if mcq_format_input == mcq_format_output {
        data.output_text.push_back("[-] Input format and Output format can't be the same".to_string());
        return;
    }

    let mut converter = match McqConverter::new(&mcq_config, mcq_format_input) {
        Ok(c) => c,
        Err(e) => {
            data.output_text.push_back(format!("[-] An error occured: {}", e));
            return;
        }
    };
    data.converted_data = match converter.convert_to(mcq_format_output) {
        Ok(d) => d,
        Err(e) => {
            data.output_text.push_back(format!("[-] An error occured: {}", e));
            return;
        }
    };
    for w in converter.get_warnings() {
        data.output_text.push_back(w);
    }

    // TODO: Try to add a default name for the saved file
    let txt = FileSpec::new("Output file", &["txt"]);
    ctx.submit_command(
        Command::new(
            druid::commands::SHOW_SAVE_PANEL,
            FileDialogOptions::new()
                .allowed_types(vec![txt])
                .default_type(txt)
        ),
        None,
    );
}

fn ui_builder() -> impl Widget<McqConverterConfig> {
    let txt = FileSpec::new("Source file", &["txt"]);
    let open_dialog_options = FileDialogOptions::new()
        .allowed_types(vec![txt])
        .default_type(txt);
    let open_dialog_options = open_dialog_options.clone();
    let open = Button::new("Open").on_click(move |ctx, _, _| {
        ctx.submit_command(
            Command::new(
                druid::commands::SHOW_OPEN_PANEL,
                open_dialog_options.clone(),
            ),
            None,
        )
    })
        .lens(McqConverterConfig::input_path);

    let open_filename = Label::new(|data: &String, _env: &Env| {
        let p = Path::new(data);
        let mut s = p.file_name().unwrap_or(&OsString::new()).to_str().unwrap_or("Filename").to_string();
        if s.len() > 22 {
            s.truncate(22usize);
            s + "..."
        }
        else {
            s
        }
    })
        .center()
        .lens(McqConverterConfig::input_path);

    let input_format_list = RadioGroup::new(
        vec![
            ("GIFT", Format::Gift),
            ("AMC-TXT", Format::AmcTxt),
        ]
    )
        .lens(McqConverterConfig::input_format);
    let input_format_text = Label::new("Input Format").center();

    let output_format_list = RadioGroup::new(
        vec![
            ("AMC-TXT", Format::AmcTxt),
            ("GIFT", Format::Gift)
        ]
    )
        .lens(McqConverterConfig::output_format);
    let output_format_text = Label::new("Output Format").center();

    let convert_btn = Button::new("Convert").on_click(convert_file);
    // TODO: Maybe add spinner (https://github.com/linebender/druid/blob/master/druid/examples/blocking_function.rs)
    //let spinner = Spinner::new();
    //let convert_spinner = Either::new()

    let output_text = Scroll::new(
        List::new(|| {
            Label::new(|data: &String, _env: &Env| {
                data.clone()
            })
        })
    )
        .expand()
        .lens(McqConverterConfig::output_text);

    // TODO: Add comment conversion possibility
    let mut col = Flex::column();
    col.add_flex_child(
        Flex::row()
            .with_flex_spacer(1.0)
            .with_child(
                Flex::column()
                    .with_child(open)
                    .with_spacer(10.0)
                    .with_child(open_filename)
            )
            .with_flex_spacer(1.0)
            .with_child(
                Flex::row()
                    .with_child(
                        Flex::column()
                            .with_child(input_format_text)
                            .with_spacer(10.0)
                            .with_child(input_format_list)
                    )
                    .with_spacer(20.0)
                    .with_child(
                        Flex::column()
                            .with_child(output_format_text)
                            .with_spacer(10.0)
                            .with_child(output_format_list)
                    )
            )
            .with_flex_spacer(1.0)
            .with_child(convert_btn)
            .with_flex_spacer(1.0)
            .center()
        ,
        1.0
    );
    col.add_flex_child(
            output_text
            .border(Color::grey(0.6), 2.0)
        ,
        1.0
    );

    col.center()
}

pub fn main() {
    let main_window = WindowDesc::new(ui_builder)
        .title(LocalizedString::new("McqConverter").with_placeholder("McqConverter"))
        .window_size((1000.0, 500.0));

    let mcqconverter_config = McqConverterConfig {
        input_format: Format::Gift,
        output_format: Format::AmcTxt,
        input_path: DEFAULT_INPUT_PATH.to_string(),
        output_path: "".to_string(),
        output_text: vector![],
        converted_data: "".to_string(),
    };
    AppLauncher::with_window(main_window)
        .delegate(Delegate)
        .launch(mcqconverter_config)
        .expect("launch failed");
}



