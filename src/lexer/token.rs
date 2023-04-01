use core::fmt::{self, Display};

use crate::parser::ast::{BinaryOperator, UnaryOperator};

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // groupings
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // arithmetic operators
    Plus,
    Increment,
    Minus,
    Decrement,
    Star,
    Slash,
    Modulo,

    // boolean operators
    Not,
    Equals,
    NotEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Or,
    And,

    // etc
    Assign,
    Arrow,
    FatArrow,
    Dot,
    Comma,
    Colon,
    SemiColon,
    Ellipsis,

    // keywords
    Event,
    Function,
    Let,
    If,
    Else,
    For,
    While,
    Continue,
    Break,
    Return,
    True,
    False,
    Print,
    // types
    TypeNumber,
    TypeString,
    TypeBoolean,

    // special
    Identifier,
    String,
    Number,
}

impl TokenType {
    pub fn get_keyword(name: String) -> TokenType {
        match name.as_str() {
            "event" => TokenType::Event,
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "for" => TokenType::For,
            "while" => TokenType::While,
            "continue" => TokenType::Continue,
            "break" => TokenType::Break,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "print" => TokenType::Print,
            "num" => TokenType::TypeNumber,
            "string" => TokenType::TypeString,
            "bool" => TokenType::TypeBoolean,
            _ => TokenType::Identifier,
        }
    }

    pub fn get_operator_binary(&self) -> Option<BinaryOperator> {
        match self {
            TokenType::Plus => Some(BinaryOperator::Add),
            TokenType::Minus => Some(BinaryOperator::Subtract),
            TokenType::Star => Some(BinaryOperator::Multiply),
            TokenType::Slash => Some(BinaryOperator::Divide),
            TokenType::Modulo => Some(BinaryOperator::Modulo),
            TokenType::Equals => Some(BinaryOperator::Equals),
            TokenType::NotEq => Some(BinaryOperator::NotEq),
            TokenType::Greater => Some(BinaryOperator::Greater),
            TokenType::GreaterEq => Some(BinaryOperator::GreaterEq),
            TokenType::Less => Some(BinaryOperator::Less),
            TokenType::LessEq => Some(BinaryOperator::LessEq),
            TokenType::Or => Some(BinaryOperator::Or),
            TokenType::And => Some(BinaryOperator::And),
            TokenType::Assign => Some(BinaryOperator::Assign),
            _ => None,
        }
    }

    pub fn get_operator_unary(&self) -> Option<UnaryOperator> {
        match self {
            TokenType::Plus => Some(UnaryOperator::Identity),
            TokenType::Minus => Some(UnaryOperator::Negate),
            TokenType::Not => Some(UnaryOperator::Not),
            TokenType::Increment => Some(UnaryOperator::Increment),
            TokenType::Decrement => Some(UnaryOperator::Decrement),
            _ => None,
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::LParen => f.write_str("'('")?,
            TokenType::RParen => f.write_str("')'")?,
            TokenType::LBrace => f.write_str("'{'")?,
            TokenType::RBrace => f.write_str("'}'")?,
            TokenType::LBracket => f.write_str("'['")?,
            TokenType::RBracket => f.write_str("']'")?,
            TokenType::Plus => f.write_str("'+'")?,
            TokenType::Increment => f.write_str("'++'")?,
            TokenType::Minus => f.write_str("'-'")?,
            TokenType::Decrement => f.write_str("'--'")?,
            TokenType::Star => f.write_str("'*'")?,
            TokenType::Slash => f.write_str("'/'")?,
            TokenType::Modulo => f.write_str("'%'")?,
            TokenType::Not => f.write_str("'!'")?,
            TokenType::Equals => f.write_str("'=='")?,
            TokenType::NotEq => f.write_str("'!='")?,
            TokenType::Greater => f.write_str("'>'")?,
            TokenType::GreaterEq => f.write_str("'>='")?,
            TokenType::Less => f.write_str("'<'")?,
            TokenType::LessEq => f.write_str("'<='")?,
            TokenType::Or => f.write_str("'||'")?,
            TokenType::And => f.write_str("'&&'")?,
            TokenType::Assign => f.write_str("'='")?,
            TokenType::Arrow => f.write_str("'->'")?,
            TokenType::FatArrow => f.write_str("'=>'")?,
            TokenType::Dot => f.write_str("'.'")?,
            TokenType::Comma => f.write_str("','")?,
            TokenType::Colon => f.write_str("':'")?,
            TokenType::SemiColon => f.write_str("';'")?,
            TokenType::Ellipsis => f.write_str("'...'")?,
            TokenType::Event => f.write_str("'event'")?,
            TokenType::Function => f.write_str("'fn'")?,
            TokenType::Let => f.write_str("'let'")?,
            TokenType::If => f.write_str("'if'")?,
            TokenType::Else => f.write_str("'else'")?,
            TokenType::For => f.write_str("'for'")?,
            TokenType::While => f.write_str("'while'")?,
            TokenType::Continue => f.write_str("'continue'")?,
            TokenType::Break => f.write_str("'break'")?,
            TokenType::Return => f.write_str("'return'")?,
            TokenType::True => f.write_str("'true'")?,
            TokenType::False => f.write_str("'false'")?,
            TokenType::Print => f.write_str("'print'")?,
            TokenType::Identifier => f.write_str("identifier")?,
            TokenType::String => f.write_str("string")?,
            TokenType::Number => f.write_str("number")?,
            TokenType::TypeNumber => f.write_str("type number")?,
            TokenType::TypeString => f.write_str("type string")?,
            TokenType::TypeBoolean => f.write_str("type boolean")?,
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Default for Span {
    fn default() -> Self {
        Span { start: 0, end: 0 }
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}
