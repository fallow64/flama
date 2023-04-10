use core::fmt::Debug;

use crate::{
    check::types::Type,
    compiler::{
        builder::{Args, BlockInfo, CodeBlock, CodeItem, CodeValue, Info},
        compiler::Compiler,
    },
};

use self::lists::{
    LenBuiltIn, ListAppendBuiltIn, ListAppendListBuiltIn, ListGetBuiltIn, ListRandomizeBuiltIn,
    ListRandomizedBuiltIn, ListRemoveBuiltIn, ListTrimBuiltIn, ListTrimmedBuiltIn,
};

pub mod lists;

pub static BUILT_INS: [&dyn BuiltIn; 10] = [
    &PrintBuiltIn,
    &LenBuiltIn,
    &ListAppendBuiltIn,
    &ListAppendListBuiltIn,
    &ListRandomizeBuiltIn,
    &ListRandomizedBuiltIn,
    &ListGetBuiltIn,
    &ListRemoveBuiltIn,
    &ListTrimBuiltIn,
    &ListTrimmedBuiltIn,
];

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

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue>;

    /// Returns whether the built-in function is a method.
    fn is_method(&self) -> bool {
        !self.can_act_on(None)
    }

    /// Returns whether the built-in function is a function.
    fn is_function(&self) -> bool {
        self.can_act_on(None)
    }
}

// general builtins

/// A built-in function that "prints" (whether that be console or `SendMessage`).
#[derive(Debug)]
pub struct PrintBuiltIn;

impl BuiltIn for PrintBuiltIn {
    fn get_name(&self) -> &str {
        "print"
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

    fn compile(
        &self,
        compiler: &mut Compiler,
        _base: Option<(CodeValue, Type)>,
        args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let mut code_items = vec![];
        for (i, (arg, _)) in args.iter().enumerate() {
            code_items.push(arg.clone().as_item(i as i32));
        }

        code_items.append(&mut vec![
            CodeItem {
                item: CodeValue::Tag {
                    option: "Add spaces".to_string(),
                    tag: "Text Value Merging".to_string(),
                    action: "SendMessage".to_string(),
                    block: "player_action".to_string(),
                },
                slot: 25,
            },
            CodeItem {
                item: CodeValue::Tag {
                    option: "Regular".to_string(),
                    tag: "Alignment Mode".to_string(),
                    action: "SendMessage".to_string(),
                    block: "player_action".to_string(),
                },
                slot: 26,
            },
        ]);

        compiler.current_stack.push(CodeBlock {
            id: "block".to_string(),
            info: Info::BlockInfo(BlockInfo::PlayerAction {
                args: Args { items: code_items },
                action: "SendMessage".to_string(),
            }),
        });

        None
    }
}
