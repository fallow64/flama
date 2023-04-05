use std::{path::PathBuf, process, rc::Rc};

use crate::{
    check,
    lexer::Lexer,
    logger,
    parser::{ast_printer, Parser},
    FlamaResults,
};

pub fn run(source: String, path_pointer: Rc<PathBuf>) {
    let lexer = Lexer::new(source, path_pointer.clone());
    let parser = Parser::new(lexer, path_pointer.clone());

    let program = Rc::new(unwrap_mul_or_exit(parser.parse_program()));
    unwrap_mul_or_exit(check::check(program.clone(), path_pointer.clone()));
    unwrap_mul_or_exit(ast_printer::print(program.clone()));
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
