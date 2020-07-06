// TODO: Create custom error type
// https://github.com/Geal/nom/blob/master/src/error.rs#L79

/*
pub fn custom_gift_error<'a, E: McqConverterError<&'a str>>(i: &'a str, kind: GiftErrorType) -> IResult<&'a str, String, E> {
    Err(nom::Err::Error(GiftError::from_gift_error(i, kind)))
}

#[derive(Clone, Debug, PartialEq)]
pub enum GiftErrorType {
    NoQuestionText,
}

/// This error type accumulates errors and their position when backtracking
/// through a parse tree. With some post processing (cf `examples/json.rs`),
/// it can be used to display user friendly error messages
#[derive(Clone, Debug, PartialEq)]
pub struct GiftError<I> {
    /// List of errors accumulated by `GiftError`, containing the affected
    /// part of input data, and some context
    pub errors: nom::lib::std::vec::Vec<(I, GiftErrorKind)>,
}

#[derive(Clone, Debug, PartialEq)]
/// Error context for `GiftError`
pub enum GiftErrorKind {
    /// Static string added by the `context` function
    Context(&'static str),
    /// Indicates which character was expected by the `char` function
    Char(char),
    /// Error kind given by various nom parsers
    Nom(ErrorKind),
    Custom(GiftErrorType),
}

impl<I> McqConverterError<I> for GiftError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        GiftError {
            errors: vec![(input, GiftErrorKind::Nom(kind))],
        }
    }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push((input, GiftErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        GiftError {
            errors: vec![(input, GiftErrorKind::Char(c))],
        }
    }

    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, GiftErrorKind::Context(ctx)));
        other
    }
}

impl<I> GiftError<I> {
    pub fn from_gift_error(input: I, kind: GiftErrorType) -> Self {
        GiftError {
            errors: vec![(input, GiftErrorKind::Custom(kind))]
        }
    }
}

/// Replacement for the Offset trait
fn get_offset(first: &str, second: &str) -> usize {
    let fst = first.as_ptr();
    let snd = second.as_ptr();

    snd as usize - fst as usize
}

/// Transforms a `GiftError` into a trace with input position information
pub fn gift_convert_error(input: &str, e: GiftError<&str>) -> String {
    use std::fmt::Write;

    let mut result = String::new();

    for (i, (substring, kind)) in e.errors.iter().enumerate() {
        let offset = get_offset(input, substring);

        if input.is_empty() {
            match kind {
                GiftErrorKind::Char(c) => {
                    write!(&mut result, "{}: expected '{}', got empty input\n\n", i, c)
                }
                GiftErrorKind::Context(s) => write!(&mut result, "{}: in {}, got empty input\n\n", i, s),
                GiftErrorKind::Nom(e) => write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e),
            }
        } else {
            let prefix = &input.as_bytes()[..offset];

            // Count the number of newlines in the first `offset` bytes of input
            let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;

            // Find the line that includes the subslice:
            // Find the *last* newline before the substring starts
            let line_begin = prefix
                .iter()
                .rev()
                .position(|&b| b == b'\n')
                .map(|pos| offset - pos)
                .unwrap_or(0);

            // Find the full line after that newline
            let line = input[line_begin..]
                .lines()
                .next()
                .unwrap_or(&input[line_begin..])
                .trim_end();

            // The (1-indexed) column number is the offset of our substring into that line
            let column_number = get_offset(line, substring) + 1;

            match kind {
                GiftErrorKind::Char(c) => {
                    if let Some(actual) = substring.chars().next() {
                        write!(
                            &mut result,
                            "{i}: at line {line_number}:\n\
                            {line}\n\
                            {caret:>column$}\n\
                            expected '{expected}', found {actual}\n\n",
                            i = i,
                            line_number = line_number,
                            line = line,
                            caret = '^',
                            column = column_number,
                            expected = c,
                            actual = actual,
                        )
                    } else {
                        write!(
                            &mut result,
                            "{i}: at line {line_number}:\n\
                            {line}\n\
                            {caret:>column$}\n\
                            expected '{expected}', got end of input\n\n",
                            i = i,
                            line_number = line_number,
                            line = line,
                            caret = '^',
                            column = column_number,
                            expected = c,
                        )
                    }
                }
                GiftErrorKind::Context(s) => write!(
                    &mut result,
                    "{i}: at line {line_number}, in {context}:\n\
                    {line}\n\
                    {caret:>column$}\n\n",
                    i = i,
                    line_number = line_number,
                    context = s,
                    line = line,
                    caret = '^',
                    column = column_number,
                ),
                GiftErrorKind::Nom(e) => write!(
                    &mut result,
                    "{i}: at line {line_number}, in {nom_err:?}:\n\
                    {line}\n\
                    {caret:>column$}\n\n",
                    i = i,
                    line_number = line_number,
                    nom_err = e,
                    line = line,
                    caret = '^',
                    column = column_number,
                ),
                GiftErrorKind::Custom(e) => write!(
                    &mut result,
                    "{i}: at line {line_number}, in {nom_err:?}:\n\
                    {line}\n\
                    {caret:>column$}\n\n",
                    i = i,
                    line_number = line_number,
                    nom_err = e,
                    line = line,
                    caret = '^',
                    column = column_number,
                ),
            }
        }
            // Because `write!` to a `String` is infallible, this `unwrap` is fine.
            .unwrap();
    }

    result
}
 */
