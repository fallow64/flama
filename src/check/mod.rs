use std::rc::Rc;

use crate::{parser::ast::Program, FlamaResult};

pub mod test_layer;

pub fn check(program: Rc<Program>) -> FlamaResult<()> {
    test_layer::test(program.clone())?;

    Ok(())
}
