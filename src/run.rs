use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
    process,
    rc::Rc,
};

use flate2::{write::GzEncoder, Compression};

use crate::{
    check, compiler::compiler::Compiler, error::FlamaResults, lexer::Lexer, logger, parser::Parser,
};

/// Reads the contents of a file, then calls `run()`.
pub fn run_file(file_name: String) {
    let source = fs::read_to_string(&file_name).expect("Error while reading file.");
    let path_pointer = Rc::new(Path::new(&file_name).to_path_buf());

    run(source, path_pointer);
}

/// Runs the compiler. Does not return anything.
pub fn run(source: String, path_pointer: Rc<PathBuf>) {
    let lexer = Lexer::new(source, path_pointer.clone());
    let parser = Parser::new(lexer, path_pointer.clone());

    let program = Rc::new(unwrap_mul_or_exit(parser.parse_program()));
    unwrap_mul_or_exit(check::check(program.clone(), path_pointer));
    // unwrap_mul_or_exit(Printer::print(program.clone()));

    let templates = Compiler::compile_program(program);
    let encoded_template = templates
        .iter()
        .map(serde_json::to_string)
        .map(Result::unwrap)
        .map(encode_template)
        .collect::<Vec<_>>();

    for encoded_template in encoded_template {
        println!("{}", encoded_template);
    }
}

/// Unwraps a `FlamaResults` or exits the program with error code `1` while logging all errors.
fn unwrap_mul_or_exit<T>(result: FlamaResults<T>) -> T {
    match result {
        Ok(value) => value,
        Err(errs) => {
            for err in errs {
                logger::report_error(err);
            }
            process::exit(1);
        }
    }
}

fn encode_template(raw: String) -> String {
    // first gzip encode, then base64 encode.
    let mut e = GzEncoder::new(Vec::new(), Compression::default());
    e.write_all(raw.as_bytes()).unwrap();
    let compressed = e.finish().unwrap();
    let encoded = base64::encode(&compressed);
    encoded
}
