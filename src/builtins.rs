use std::fmt::Debug;

use crate::check::types::Type;

pub static BUILT_INS: [&dyn BuiltIn; 2] = [&PrintBuiltIn, &LenBuiltIn];

pub fn get_built_in(name: &str) -> Option<&'static dyn BuiltIn> {
    BUILT_INS.iter().find(|b| b.get_name() == name).copied()
}

/// A trait for built-in functions that can be used in the language.
pub trait BuiltIn: Debug + Sync {
    /// The name of the built-in function.
    fn get_name(&self) -> &str;

    /// Returns whether the built-in function can be called on the given type.
    /// e.g. `some_array.len()` is valid, but `some_number.len()` is not.
    fn can_act_on(&self, base_type: Option<&Type>) -> bool;

    /// Returns the type of the built-in function.
    fn get_return_type(&self, base_type: Option<&Type>) -> Type;

    /// Returns whether the given arguments are valid for the built-in function.
    fn is_valid_args(&self, base_type: Option<&Type>, args: &[Type]) -> Result<(), String>;

    /// Returns whether the built-in function is a method.
    fn is_method(&self) -> bool {
        !self.can_act_on(None)
    }

    /// Returns whether the built-in function is a function.
    fn is_function(&self) -> bool {
        self.can_act_on(None)
    }
}

/// A built-in function that "prints" (whether that be console or `SendMessage`).
#[derive(Debug)]
pub struct PrintBuiltIn;

impl BuiltIn for PrintBuiltIn {
    fn get_name(&self) -> &str {
        "p" // temporary because of print statement
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        base_type.is_none()
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Void
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, _args: &[Type]) -> Result<(), String> {
        Ok(()) // all args are valid for print
    }
}

/// A built-in function that returns the length of a value.
#[derive(Debug)]
pub struct LenBuiltIn;

impl BuiltIn for LenBuiltIn {
    fn get_name(&self) -> &str {
        "len" // temporary because of print statement
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
