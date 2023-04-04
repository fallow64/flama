use std::{path::PathBuf, rc::Rc};

use crate::{parser::ast::Program, FlamaResult};

use self::{test_layer::TestLayer, type_checker::TypeChecker};

pub mod environment;
pub mod test_layer;
pub mod type_checker;

pub fn check(program: Rc<Program>, source_path: Rc<PathBuf>) -> FlamaResult<()> {
    TestLayer::test(program.clone())?;
    TypeChecker::check(program.clone(), source_path)?;

    Ok(())
}
