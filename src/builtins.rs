use crate::check::types::Type;

/// A trait for built-in functions that can be used in the language.
pub trait BuiltIn {
    /// Returns the name of the built-in function.
    fn get_name() -> &'static str;

    /// Returns whether the built-in function can be called on the given type.
    /// e.g. `some_array.len()` is valid, but `some_number.len()` is not.
    fn can_act_on(base_type: &Type) -> bool;

    /// Returns the type of the built-in function.
    fn get_return_type(base_type: Option<&Type>) -> Type;

    /// Returns whether the given arguments are valid for the built-in function.
    /// e.g. `some_array.len()` is valid, but `some_array.len(1)` is not.
    /// Also handles arity checking.
    fn is_valid_args(args: &[Type]) -> bool;

    // fn compile();
}
