use std::fmt::Debug;

use crate::check::types::Type;

const BUILT_INS: [&dyn BuiltIn; 1] = [&PrintBuiltIn];

pub fn get_built_in(name: &str) -> Option<&dyn BuiltIn> {
    BUILT_INS.iter().find(|b| b.get_name() == name).copied()
}

/// A trait for built-in functions that can be used in the language.
pub trait BuiltIn: Debug {
    /// The name of the built-in function.
    fn get_name(&self) -> &str;

    /// Returns whether the built-in function can be called on the given type.
    /// e.g. `some_array.len()` is valid, but `some_number.len()` is not.
    fn can_act_on(&self, base_type: &Type) -> bool;

    /// Returns the type of the built-in function.
    fn get_return_type(&self, base_type: Option<&Type>) -> Type;

    /// Returns whether the given arguments are valid for the built-in function.
    /// e.g. `some_array.len()` is valid, but `some_array.len(1)` is not.
    /// Also handles arity checking.
    fn is_valid_args(&self, args: &[Type]) -> Result<(), String>;

    // fn compile();
}

#[derive(Debug)]
pub struct PrintBuiltIn;

impl BuiltIn for PrintBuiltIn {
    fn get_name(&self) -> &str {
        "p" // temporary because of print statement
    }

    fn can_act_on(&self, _base_type: &Type) -> bool {
        false
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Void
    }

    fn is_valid_args(&self, _args: &[Type]) -> Result<(), String> {
        Ok(()) // all args are valid for print
    }
}
