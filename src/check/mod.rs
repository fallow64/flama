use std::{path::PathBuf, rc::Rc};

use crate::{error::FlamaResults, parser::ast::Program};

use self::type_checker::TypeChecker;

pub mod environment;
pub mod type_checker;
pub mod types;

pub fn check(program: Rc<Program>, source_path: Rc<PathBuf>) -> FlamaResults<()> {
    TypeChecker::check(program, source_path)?;
    // no more errors after this point

    Ok(())
}
