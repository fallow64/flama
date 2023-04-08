use std::{fmt::Display, path::PathBuf, rc::Rc};

use crate::lexer::token::Span;

pub type FlamaResult<T> = Result<T, FlamaError>;
pub type FlamaResults<T> = Result<T, Vec<FlamaError>>;

// pub struct SourceFileData {
//     pub source: String,
//     pub path: Rc<PathBuf>,
// }

#[derive(Debug)]
pub struct FlamaError {
    pub message: String,
    pub span: Span,
    pub error_type: ErrorType,
    pub source_path: Rc<PathBuf>,
}

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
