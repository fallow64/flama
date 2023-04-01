use std::{process, rc::Rc};

use crate::{
    check,
    lexer::Lexer,
    logger,
    parser::{
        ast::{new_node_ptr, Section, NodePtr, Program},
        ast_printer, Parser,
    },
};

pub fn run(source: String, path_pointer: Rc<String>) {
    let lexer = Lexer::new(source, path_pointer.clone());
    let parser = Parser::new(lexer, path_pointer.clone());

    // Collect all declarations and errors.
    let mut declarations = vec![];
    let mut errored = false;
    for result in parser {
        match result {
            Ok(declaration) => declarations.push(declaration),
            Err(error) => {
                logger::report_error(error);
                errored = true;
            },
        }
    }

    if errored {
        process::exit(1);
    }

    let program: Program = Rc::new(
        declarations
            .into_iter()
            .map(new_node_ptr)
            .collect::<Vec<NodePtr<Section>>>(),
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
