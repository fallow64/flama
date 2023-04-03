use std::fs;

use colored::Colorize;

use crate::{lexer::token::Span, FlamaError};

pub fn report_error(error: FlamaError) {
    let span = error.span;
    let source_path = error.source_path;
    let error_type = error.error_type;
    let message = error.message;

    let source = fs::read_to_string(source_path.as_ref()).expect("Error while reading file.");
    let (start, _) = get_line_column(&source, span);

    // <error type>: <message>
    println!("{}: {}", error_type.to_string().red().bold(), message,);

    //    -> at <file_name>:<line>:<column>
    println!(
        "{} at {}:{}:{}",
        "    ->".red(),
        source_path.to_str().unwrap(),
        start.0,
        start.1
    );
}

fn get_line_column(source: &str, span: Span) -> ((usize, usize), (usize, usize)) {
    let mut start_line = 1;
    let mut start_column = 1;
    let mut end_line = 1;
    let mut end_column = 1;

    let mut current_line = 1;
    let mut current_column = 1;

    for (i, c) in source.char_indices() {
        if i == span.start {
            start_line = current_line;
            start_column = current_column;
        }

        if i == span.end {
            end_line = current_line;
            end_column = current_column;
        }

        if c == '\n' {
            current_line += 1;
            current_column = 1;
        } else {
            current_column += 1;
        }
    }

    ((start_line, start_column), (end_line, end_column))
}
