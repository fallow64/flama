use std::{collections::HashMap, fmt::Display};

use crate::parser::ast::{FunctionSignature, Identifier, LiteralKind};

/// The `Type` enum contains all of the possible types of types of expressions.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Type {
    Number,
    String,
    Boolean,
    List(Box<Type>),
    /// Intermediary between parser and type checker. Not to actually be used in the AST.   
    Identifier(Identifier),
    Function(Box<FunctionType>),
    Struct(String, HashMap<String, Type>),

    /// Used for list inference
    Any,
    #[default]
    /// Used solely for function returns. This is a hack to allow functions to return nothing.
    Void,
    /// Used as a temporary for when there is an error in type creation. TODO: delete?
    None,
}

impl From<LiteralKind> for Type {
    fn from(kind: LiteralKind) -> Self {
        match kind {
            LiteralKind::Number(_) => Type::Number,
            LiteralKind::String(_) => Type::String,
            LiteralKind::Boolean(_) => Type::Boolean,
        }
    }
}

impl From<Identifier> for Type {
    fn from(id: Identifier) -> Self {
        Type::Identifier(id)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Boolean => write!(f, "boolean"),
            Type::List(typ) => write!(f, "list<{}>", typ),
            Type::Identifier(id) => write!(f, "{}", id),
            Type::Function(sig) => {
                write!(
                    f,
                    "fn<({}) -> {}>",
                    sig.params
                        .iter()
                        .map(|typ| format!("{}", typ))
                        .collect::<Vec<String>>()
                        .join(", "),
                    sig.return_type.as_ref().unwrap_or(&Type::default())
                )
            }
            Type::Struct(name, _) => {
                write!(f, "<struct {}>", name)
            }
            Type::Any => write!(f, "any"),
            Type::Void => write!(f, "void"),
            Type::None => write!(f, "(error in type creation)"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub return_type: Option<Type>,
}

impl From<FunctionSignature> for FunctionType {
    fn from(value: FunctionSignature) -> Self {
        FunctionType {
            params: value
                .params
                .iter()
                .map(|p| p.1.typ.clone())
                .collect::<Vec<Type>>(),
            return_type: value.return_type.map(|te| te.typ).clone(),
        }
    }
}
