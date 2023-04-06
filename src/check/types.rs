use std::fmt::Display;

use crate::parser::ast::{FunctionSignature, Identifier, LiteralKind};

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub enum Type {
    Number,
    String,
    Boolean,
    List(Box<Type>),
    Identifier(Identifier),
    Function(Box<FunctionSignature>),

    Any, // used for list inference
    #[default]
    Void,
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
                        .map(|p| p.type_annotation.typ.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    sig.return_type
                        .as_ref()
                        .map_or(&Type::default(), |te| &te.typ)
                )
            }
            Type::Any => write!(f, "any"),
            Type::Void => write!(f, "void"),
        }
    }
}
