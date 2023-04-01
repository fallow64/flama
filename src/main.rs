use std::{env, fmt::Display, fs, process, rc::Rc};

use lexer::{token::Span, Lexer};
use parser::{
    ast::{new_node_ptr, Program},
    Parser,
};

mod lexer;
mod logger;
mod parser;

fn main() {
    let mut args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        run_file(args.remove(1));
    } else {
        println!("Usage: {} <file_name>", args[0]);
    }
}

fn run_file(file_name: String) {
    let source = fs::read_to_string(&file_name).expect("Error while reading file.");
    let path_pointer = Rc::new(file_name);

    run(source, path_pointer);
}

fn run(source: String, path_pointer: Rc<String>) {
    let lexer = Lexer::new(source, path_pointer.clone());
    let parser = Parser::new(lexer, path_pointer.clone());

    // collect the `parser` iterator into two vectors: one for the sections and one for the errors
    // let mut sections = vec![];
    // let mut errors = vec![];
    // for result in parser {
    //     match result {
    //         Ok(section) => sections.push(section),
    //         Err(error) => errors.push(error),
    //     }
    // }
    let (declarations, errors): (Vec<_>, Vec<_>) = parser.fold(
        (Vec::new(), Vec::new()),
        |(mut decls, mut errors), result| {
            result
                .map(|decl| decls.push(decl))
                .map_err(|error| errors.push(error))
                .unwrap_or_else(|_| ());
            (decls, errors)
        },
    );

    if errors.len() != 0 {
        for error in errors {
            logger::report_error(error);
        }
        process::exit(1);
    } else {
        let program = declarations.into_iter().map(new_node_ptr).collect::<Program>();
        println!("{:#?}", program);
    }
}

// etc

pub type FlamaResult<T> = Result<T, FlamaError>;

#[derive(Debug)]
pub enum ErrorType {
    Syntax,
    Parsing,
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::Syntax => write!(f, "syntax error"),
            ErrorType::Parsing => write!(f, "parsing error"),
        }
    }
}

#[derive(Debug)]
pub struct FlamaError {
    pub message: String,
    pub span: Span,
    pub error_type: ErrorType,
    pub source_path: Rc<String>,
}
