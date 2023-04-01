use std::{process, rc::Rc};

use crate::{
    check,
    lexer::Lexer,
    logger,
    parser::{
        ast::{new_node_ptr, Declaration, NodePtr, Program},
        ast_printer, Parser,
    },
};

pub fn run(source: String, path_pointer: Rc<String>) {
    let lexer = Lexer::new(source, path_pointer.clone());
    let parser = Parser::new(lexer, path_pointer.clone());

    // Collect all declarations and errors.
    let mut declarations = vec![];
    let mut errors = vec![];
    for result in parser {
        match result {
            Ok(declaration) => declarations.push(declaration),
            Err(error) => errors.push(error),
        }
    }

    // If there are any errors, report them and exit.
    if errors.len() != 0 {
        for error in errors {
            logger::report_error(error);
        }
        process::exit(1);
    }

    let program: Program = Rc::new(
        declarations
            .into_iter()
            .map(new_node_ptr)
            .collect::<Vec<NodePtr<Declaration>>>(),
    );

    if let Err(err) = check::check(program.clone()) {
        logger::report_error(err);
        process::exit(1);
    }

    if let Err(err) = ast_printer::print(program.clone()) {
        logger::report_error(err);
        process::exit(1);
    }
}
