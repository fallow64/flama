use std::{path::PathBuf, rc::Rc};

use crate::{parser::ast::Program, FlamaResults};

use self::type_checker::TypeChecker;

pub mod environment;
// pub mod test_layer;
pub mod type_checker;
pub mod types;

pub fn check(program: Rc<Program>, source_path: Rc<PathBuf>) -> FlamaResults<()> {
    // TestLayer::test(program.clone())?;
    TypeChecker::check(program.clone(), source_path)?;

    Ok(())
}
