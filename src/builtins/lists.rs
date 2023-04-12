use std::{fmt::Debug, ops::Deref};

use crate::{
    check::types::Type,
    compiler::{
        builder::{Args, BlockInfo, CodeValue},
        compiler::Compiler,
    },
};

use super::BuiltIn;

/// A built-in function that returns the length of a value (strings and lists).
#[derive(Debug)]
pub struct LenBuiltIn;

impl BuiltIn for LenBuiltIn {
    fn get_name(&self) -> &str {
        "len"
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

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        _args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let (base, base_type) = base.unwrap();

        let result_var = compiler.namer.get_rand_var("len");
        let args = Args {
            items: vec![result_var.clone().as_item(0), base.as_item(1)],
        };
        match base_type {
            Type::List(_) => compiler.current_stack.push(
                BlockInfo::SetVariable {
                    args,
                    action: "ListLength".to_string(),
                }
                .into(),
            ),
            Type::String => compiler.current_stack.push(
                BlockInfo::SetVariable {
                    args,
                    action: "TextLength".to_string(),
                }
                .into(),
            ),
            _ => unreachable!(),
        }

        Some(result_var)
    }
}

/// A built-in function that appends a value to a list.
#[derive(Debug)]
pub struct ListAppendBuiltIn;

impl BuiltIn for ListAppendBuiltIn {
    fn get_name(&self) -> &str {
        "append"
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)))
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Void
    }

    fn is_valid_args(&self, base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        if args.len() != 1 {
            Err("builtin 'append' takes 1 argument: list element".to_string())
        } else if let Some(Type::List(base_type)) = base_type {
            if args[0] == base_type.deref().clone() {
                Ok(())
            } else {
                Err(format!(
                    "builtin 'append' takes 1 argument of type '{}'",
                    base_type
                ))
            }
        } else {
            unreachable!()
        }
    }

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let (base, _) = base.unwrap();
        let (arg, _) = args[0].clone();

        compiler.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![base.as_item(0), arg.as_item(1)].into(),
                action: "AppendValue".to_string(),
            }
            .into(),
        );

        None
    }
}

/// A built-in function that appends a list to a list.
#[derive(Debug)]
pub struct ListAppendListBuiltIn;

impl BuiltIn for ListAppendListBuiltIn {
    fn get_name(&self) -> &str {
        "append_list"
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)))
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Void
    }

    fn is_valid_args(&self, base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        if args.len() != 1 {
            Err("builtin 'append_list' takes 1 argument: list to append".to_string())
        } else if let Some(base_type) = base_type {
            if &args[0] == base_type {
                Ok(())
            } else {
                Err(format!(
                    "builtin 'append' takes 1 argument of type '{}'",
                    base_type
                ))
            }
        } else {
            unreachable!()
        }
    }

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let (base, _) = base.unwrap();
        let (arg, _) = args[0].clone();

        compiler.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![base.as_item(0), arg.as_item(1)].into(),
                action: "AppendList".to_string(),
            }
            .into(),
        );

        None
    }
}

/// A built-in function that randomized a list.
#[derive(Debug)]
pub struct ListRandomizeBuiltIn;

impl BuiltIn for ListRandomizeBuiltIn {
    fn get_name(&self) -> &str {
        "randomize"
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)))
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Void
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        if args.is_empty() {
            Ok(())
        } else {
            Err("builtin 'randomize' takes no arguments".to_string())
        }
    }

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        _args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let (base, _) = base.unwrap();

        compiler.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![base.as_item(0)].into(),
                action: "RandomizeList".to_string(),
            }
            .into(),
        );

        None
    }
}

/// A built-in function that returns the randomized version of a list.
#[derive(Debug)]
pub struct ListRandomizedBuiltIn;

impl BuiltIn for ListRandomizedBuiltIn {
    fn get_name(&self) -> &str {
        "randomized"
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)))
    }

    fn get_return_type(&self, base_type: Option<&Type>) -> Type {
        base_type.unwrap().clone()
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        if args.is_empty() {
            Ok(())
        } else {
            Err("builtin 'randomized' takes no arguments".to_string())
        }
    }

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        _args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let result = compiler.namer.get_rand_var("randomized");
        let (base, _) = base.unwrap();

        compiler.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![result.clone().as_item(0), base.as_item(1)].into(),
                action: "RandomizeList".to_string(),
            }
            .into(),
        );

        Some(result)
    }
}

/// A built-in function that gets the value of a list at a given index. Starts at 0.
#[derive(Debug)]
pub struct ListGetBuiltIn;

impl BuiltIn for ListGetBuiltIn {
    fn get_name(&self) -> &str {
        "get"
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)))
    }

    fn get_return_type(&self, base_type: Option<&Type>) -> Type {
        if let Some(Type::List(inner)) = base_type {
            inner.deref().clone()
        } else {
            unreachable!()
        }
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        if args.len() == 1 {
            Ok(())
        } else {
            Err("builtin 'get' takes 1 argument: index".to_string())
        }
    }

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let result = compiler.namer.get_rand_var("randomized");
        let (base, _) = base.unwrap();
        let (index, _) = args[0].clone();

        let index = into_index(index);

        compiler.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![result.clone().as_item(0), base.as_item(1), index.as_item(2)].into(),
                action: "GetListValue".to_string(),
            }
            .into(),
        );

        Some(result)
    }
}

/// A built-in function that removes the value of a list at a given index. Starts at 0.
#[derive(Debug)]
pub struct ListRemoveBuiltIn;

impl BuiltIn for ListRemoveBuiltIn {
    fn get_name(&self) -> &str {
        "remove"
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)))
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Void
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        if args.len() == 1 {
            Ok(())
        } else {
            Err("builtin 'get' takes 1 arguments: index".to_string())
        }
    }

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let (base, _) = base.unwrap();
        let (index, _) = args[0].clone();

        let index = into_index(index);

        compiler.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![base.as_item(0), index.as_item(1)].into(),
                action: "RemoveListIndex".to_string(),
            }
            .into(),
        );

        None
    }
}

/// A built-in function that trims a list.
#[derive(Debug)]
pub struct ListTrimBuiltIn;

impl BuiltIn for ListTrimBuiltIn {
    fn get_name(&self) -> &str {
        "trim"
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)))
    }

    fn get_return_type(&self, _base_type: Option<&Type>) -> Type {
        Type::Void
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        if args.len() == 2 {
            Ok(())
        } else {
            Err("builtin 'get' takes 2 arguments: start and end index".to_string())
        }
    }

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let (base, _) = base.unwrap();
        let (start, _) = args[0].clone();
        let (end, _) = args[1].clone();

        let start = into_index(start);
        let end = into_index(end);

        compiler.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![base.as_item(0), start.as_item(1), end.as_item(2)].into(),
                action: "TrimList".to_string(),
            }
            .into(),
        );

        None
    }
}

/// A built-in function that returns a trimmed version of a list.
#[derive(Debug)]
pub struct ListTrimmedBuiltIn;

impl BuiltIn for ListTrimmedBuiltIn {
    fn get_name(&self) -> &str {
        "trimmed"
    }

    fn can_act_on(&self, base_type: Option<&Type>) -> bool {
        matches!(base_type, Some(Type::List(_)))
    }

    fn get_return_type(&self, base_type: Option<&Type>) -> Type {
        base_type.unwrap().clone()
    }

    fn is_valid_args(&self, _base_type: Option<&Type>, args: &[Type]) -> Result<(), String> {
        if args.len() == 2 {
            Ok(())
        } else {
            Err("builtin 'trimmed' takes 2 arguments: start and end index".to_string())
        }
    }

    fn compile(
        &self,
        compiler: &mut Compiler,
        base: Option<(CodeValue, Type)>,
        args: Vec<(CodeValue, Type)>,
    ) -> Option<CodeValue> {
        let result = compiler.namer.get_rand_var("trimmed");
        let (base, _) = base.unwrap();
        let (start, _) = args[0].clone();
        let (end, _) = args[1].clone();

        let start = into_index(start);
        let end = into_index(end);

        compiler.current_stack.push(
            BlockInfo::SetVariable {
                args: vec![
                    result.clone().as_item(0),
                    base.as_item(1),
                    start.as_item(2),
                    end.as_item(3),
                ]
                .into(),
                action: "TrimList".to_string(),
            }
            .into(),
        );

        Some(result)
    }
}

fn into_index(code_value: CodeValue) -> CodeValue {
    CodeValue::Number {
        name: format!("%math({}+1)", code_value.as_string().unwrap()),
    }
}
