use uuid::Uuid;

use super::builder::{CodeValue, VariableScope};

pub struct Namer;

impl Namer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get_rand_var(&self, base: &str) -> CodeValue {
        let uuid = Uuid::new_v4();
        CodeValue::Variable {
            name: format!("$temp_{}_{}", base, uuid.to_string()),
            scope: VariableScope::default(),
        }
    }
}
