use core::fmt::{self, Display};

use crate::parser::ast::{BinaryOperator, UnaryOperator};

/// A token is a single unit of the language. It is a single word, number, or symbol.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub span: Span,
}

/// The `TokenType` enum contains all of the possible types of tokens.
/// The reason that `Token` is not just an enum with the different types
/// is because, while it would be more memory-efficient, it would be
/// mcuh more difficult to work with.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
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
    Minus,
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
    Save,
    Local,
    Game,
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
    Const,
    Struct,
    // types
    TypeNumber,
    TypeString,
    TypeBoolean,
    TypeList,

    // special
    Identifier,
    String,
    Number,
}

impl TokenType {
    /// Returns the matching keyword for the `name` string.
    /// If there are no corresponding keywords, then `TokenType::Identifier` is returned.
    pub fn get_keyword(name: String) -> TokenType {
        match name.as_str() {
            "event" => TokenType::Event,
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "local" => TokenType::Local,
            "save" => TokenType::Save,
            "game" => TokenType::Game,
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
            "const" => TokenType::Const,
            "struct" => TokenType::Struct,
            "num" => TokenType::TypeNumber,
            "string" => TokenType::TypeString,
            "bool" => TokenType::TypeBoolean,
            "list" => TokenType::TypeList,
            _ => TokenType::Identifier,
        }
    }

    /// Returns the matching binary operator for this token type, if any.
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

    /// Returns the matching unary operator for this token type, if any.
    pub fn get_operator_unary(&self) -> Option<UnaryOperator> {
        match self {
            TokenType::Plus => Some(UnaryOperator::Identity),
            TokenType::Minus => Some(UnaryOperator::Negate),
            TokenType::Not => Some(UnaryOperator::Not),
            _ => None,
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::LParen => f.write_str("LPAREN")?,
            TokenType::RParen => f.write_str("RPAREN")?,
            TokenType::LBrace => f.write_str("LBRACE")?,
            TokenType::RBrace => f.write_str("RBRACE")?,
            TokenType::LBracket => f.write_str("LBRACKET")?,
            TokenType::RBracket => f.write_str("RBRACKET")?,
            TokenType::Plus => f.write_str("PLUS")?,
            TokenType::Minus => f.write_str("MINUS")?,
            TokenType::Star => f.write_str("STAR")?,
            TokenType::Slash => f.write_str("SLASH")?,
            TokenType::Modulo => f.write_str("MODULO")?,
            TokenType::Not => f.write_str("NOT")?,
            TokenType::Equals => f.write_str("EQUALS")?,
            TokenType::NotEq => f.write_str("NOTEQUALS")?,
            TokenType::Greater => f.write_str("GREATER")?,
            TokenType::GreaterEq => f.write_str("GREATEREQUALS")?,
            TokenType::Less => f.write_str("LESS")?,
            TokenType::LessEq => f.write_str("LESSEQUALS")?,
            TokenType::Or => f.write_str("OR")?,
            TokenType::And => f.write_str("AND")?,
            TokenType::Assign => f.write_str("ASSIGN")?,
            TokenType::Arrow => f.write_str("ARROW")?,
            TokenType::FatArrow => f.write_str("FATARROW")?,
            TokenType::Dot => f.write_str("DOT")?,
            TokenType::Comma => f.write_str("COMMA")?,
            TokenType::Colon => f.write_str("COLON")?,
            TokenType::SemiColon => f.write_str("SEMICOLON")?,
            TokenType::Ellipsis => f.write_str("ELLIPSIS")?,
            TokenType::Event => f.write_str("EVENT")?,
            TokenType::Function => f.write_str("FUNCTION")?,
            TokenType::Let => f.write_str("LET")?,
            TokenType::Save => f.write_str("SAVE")?,
            TokenType::Local => f.write_str("LOCAL")?,
            TokenType::Game => f.write_str("GAME")?,
            TokenType::If => f.write_str("IF")?,
            TokenType::Else => f.write_str("ELSE")?,
            TokenType::For => f.write_str("FOR")?,
            TokenType::While => f.write_str("WHILE")?,
            TokenType::Continue => f.write_str("CONTINUE")?,
            TokenType::Break => f.write_str("BREAK")?,
            TokenType::Return => f.write_str("RETURN")?,
            TokenType::True => f.write_str("TRUE")?,
            TokenType::False => f.write_str("FALSE")?,
            TokenType::Print => f.write_str("PRINT")?,
            TokenType::Const => f.write_str("CONST")?,
            TokenType::Struct => f.write_str("STRUCT")?,
            TokenType::TypeNumber => f.write_str("TYPENUMBER")?,
            TokenType::TypeString => f.write_str("TYPESTRING")?,
            TokenType::TypeBoolean => f.write_str("TYPEBOOLEAN")?,
            TokenType::TypeList => f.write_str("TYPELIST")?,
            TokenType::Identifier => f.write_str("IDENTIFIER")?,
            TokenType::Number => f.write_str("NUMBER")?,
            TokenType::String => f.write_str("STRING")?,
        }
        Ok(())
    }
}

/// A span of text in the source code.
/// Note: this only contains the indices, not the line/column number.
/// Also, for representing EOF, use `Span { start: 0, end: 0 }`, or `Span::default()`.
#[derive(Debug, PartialEq, Eq, Default, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// A trait for structs that have a span.
pub trait Spanned {
    fn span(&self) -> Span;
}
