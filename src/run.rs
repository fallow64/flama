use std::{path::PathBuf, process, rc::Rc};

use crate::{
    check,
    lexer::Lexer,
    logger,
    parser::{ast_printer, Parser},
    FlamaError,
};

pub fn run(source: String, path_pointer: Rc<PathBuf>) {
    let lexer = Lexer::new(source, path_pointer.clone());
    let parser = Parser::new(lexer, path_pointer.clone());

    let program = Rc::new(unwrap_multiple_or_exit(parser.parse_program()));
    unwrap_or_exit(check::check(program.clone()));
    unwrap_or_exit(ast_printer::print(program.clone()));
}

/// Unwraps a `Result` or exits the program with error code `1` while logging an error.
fn unwrap_or_exit<T>(result: Result<T, FlamaError>) -> T {
    match result {
        Ok(value) => value,
        Err(err) => {
            logger::report_error(err);
            process::exit(1);
        }
    }
}

/// Unwraps a `Result` or exits the program with error code `1` while logging all errors.
fn unwrap_multiple_or_exit<T>(result: Result<T, Vec<FlamaError>>) -> T {
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
