use std::{collections::BTreeMap, fmt::Display};

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
    Function(Box<FunctionSignature>),
    // BTreeMap used because it doesn't require a Hash impl
    Struct(String, BTreeMap<String, Type>),

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
                        .map(|p| format!("{}: {}", p.0, p.1))
                        .collect::<Vec<String>>()
                        .join(", "),
                    sig.return_type
                        .as_ref()
                        .map_or(&Type::default(), |te| &te.typ)
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
