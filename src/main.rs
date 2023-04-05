use std::{
    env,
    fmt::Display,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use lexer::token::Span;

mod check;
mod lexer;
mod logger;
mod parser;
mod run;

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
    let path_pointer = Rc::new(Path::new(&file_name).to_path_buf());

    run::run(source, path_pointer);
}

// etc

pub type FlamaResult<T> = Result<T, FlamaError>;

pub type FlamaResults<T> = Result<T, Vec<FlamaError>>;

#[derive(Debug)]
pub enum ErrorType {
    Syntax,
    Parsing,
    Type,
    Name,
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::Syntax => write!(f, "syntax error"),
            ErrorType::Parsing => write!(f, "parsing error"),
            ErrorType::Type => write!(f, "type error"),
            ErrorType::Name => write!(f, "name error"),
        }
    }
}

#[derive(Debug)]
pub struct FlamaError {
    pub message: String,
    pub span: Span,
    pub error_type: ErrorType,
    pub source_path: Rc<PathBuf>,
}
