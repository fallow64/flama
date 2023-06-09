use uuid::Uuid;

use super::builder::{CodeValue, VariableScope};

/// Mangles names to make them unique.
/// Currently not very complicated, but will be expanded in the future.
pub struct Namer;

impl Namer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get_rand_var(&self, base: &str) -> CodeValue {
        let uuid = Uuid::new_v4();
        CodeValue::Variable {
            name: format!("$temp_{}_{}", base, uuid),
            scope: VariableScope::default(),
        }
    }
}
