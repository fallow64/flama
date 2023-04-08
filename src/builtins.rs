use std::fmt::Debug;

use crate::check::types::Type;

pub const BUILT_INS: [&dyn BuiltIn; 2] = [&PrintBuiltIn, &LenBuiltIn];

pub fn get_built_in(name: &str) -> Option<&dyn BuiltIn> {
    BUILT_INS.iter().find(|b| b.get_name() == name).copied()
}

/// A trait for built-in functions that can be used in the language.
pub trait BuiltIn: Debug {
    /// The name of the built-in function.
    fn get_name(&self) -> &str;

    fn is_method(&self) -> bool;

    /// Returns whether the built-in function can be called on the given type.
    /// e.g. `some_array.len()` is valid, but `some_number.len()` is not.
    ///
    /// # Arguments
    ///
    /// * `base_type` - The type of the base of the call (e.g. `list` in `some_list.len()`). None if there is no base.
    fn can_act_on(&self, base_type: Option<&Type>) -> bool;

    /// Returns the type of the built-in function.
    ///
    /// # Arguments
    ///
    /// * `base_type` - The type of the base of the call (e.g. `list` in `some_list.len()`). None if there is no base.
    fn get_return_type(&self, base_type: Option<&Type>) -> Type;

    /// Returns whether the given arguments are valid for the built-in function.
    ///
    /// # Arguments
    ///
    /// * `args` - The arguments to the built-in function. Includes the base type if there is one.
    fn is_valid_args(&self, base_type: Option<&Type>, args: &[Type]) -> Result<(), String>;

    // fn compile();
}

#[derive(Debug)]
pub struct PrintBuiltIn;

impl BuiltIn for PrintBuiltIn {
    fn get_name(&self) -> &str {
        "p" // temporary because of print statement
    }

    fn is_method(&self) -> bool {
        false
    }

    fn can_act_on(&self, _base_type: Option<&Type>) -> bool {
        false
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Void
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, _args: &[Type]) -> Result<(), String> {
        Ok(()) // all args are valid for print
    }
}

#[derive(Debug)]
pub struct LenBuiltIn;

impl BuiltIn for LenBuiltIn {
    fn get_name(&self) -> &str {
        "len" // temporary because of print statement
    }

    fn is_method(&self) -> bool {
        true
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)) | Some(Type::String))
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Number
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        // base included in args
        if args.is_empty() {
            Ok(())
        } else {
            Err("builtin 'len' takes no arguments".to_string())
        }
    }
}
