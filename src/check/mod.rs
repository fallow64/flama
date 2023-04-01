use crate::{parser::ast::Program, FlamaResult};

pub mod test_layer;

pub fn check(program: Program) -> FlamaResult<()> {
    test_layer::test(program.clone())?;

    Ok(())
}
