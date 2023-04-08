use std::{fmt::Display, path::PathBuf, rc::Rc};

use crate::lexer::token::Span;

/// Type alias for a `Result` with a `FlamaError` as the error type.
pub type FlamaResult<T> = Result<T, FlamaError>;
/// Type alias for a `Result` with a `Vec<FlamaError>` as the error type.
pub type FlamaResults<T> = Result<T, Vec<FlamaError>>;

/// FlamaError is the error type used by the compiler.
#[derive(Debug)]
pub struct FlamaError {
    pub message: String,
    pub span: Span,
    pub error_type: ErrorType,
    pub source_path: Rc<PathBuf>,
}

/// `ErrorType` is an enum that represents the type of error.
/// This is used to determine the error message.
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
