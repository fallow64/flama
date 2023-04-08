use std::{collections::BTreeMap, fmt::Display, ops::Deref};

use crate::parser::ast::{FunctionSignature, Identifier, LiteralKind};

#[derive(Debug, Clone, Eq, Default)]
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

    /// Used for builtins
    ///
    /// The first element is the base type (e.g. `list` for `some_list.len()`).
    /// Note: `==` does not consider the base type.
    BuiltIn(Option<Box<Type>>, String),
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
            Type::BuiltIn(base, name) => write!(
                f,
                "builtin({}{})",
                base.as_ref()
                    .map(|s| s.deref().to_string() + ".")
                    .unwrap_or("".to_string()),
                name
            ),
            Type::Any => write!(f, "any"),
            Type::Void => write!(f, "void"),
            Type::None => write!(f, "(error in type creation)"),
        }
    }
}

// default partialeq except for BuiltIn, which doesn't consider the base type
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Number, Type::Number) => true,
            (Type::String, Type::String) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::List(a), Type::List(b)) => a == b,
            (Type::Identifier(a), Type::Identifier(idb)) => a == idb,
            (Type::Function(a), Type::Function(b)) => a == b,
            (Type::Struct(a1, b1), Type::Struct(a2, b2)) => a1 == a2 && b1 == b2,
            (Type::BuiltIn(_, name1), Type::BuiltIn(_, name2)) => name1 == name2,
            (Type::Any, Type::Any) => true,
            (Type::Void, Type::Void) => true,
            (Type::None, Type::None) => true,
            _ => false,
        }
    }
}

// default hash except for BuiltIn, which doesn't consider the base type
impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Type::Number => Type::Number.hash(state),
            Type::String => Type::String.hash(state),
            Type::Boolean => Type::Boolean.hash(state),
            Type::List(typ) => typ.hash(state),
            Type::Identifier(id) => id.hash(state),
            Type::Function(sig) => sig.hash(state),
            Type::Struct(name, fields) => {
                name.hash(state);
                fields.hash(state);
            }
            Type::BuiltIn(_, name) => name.hash(state),
            Type::Any => Type::Any.hash(state),
            Type::Void => Type::Void.hash(state),
            Type::None => Type::None.hash(state),
        }
    }
}
