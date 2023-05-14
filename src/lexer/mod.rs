pub mod token;

use std::{path::PathBuf, rc::Rc};

use crate::error::{ErrorType, FlamaError, FlamaResult};

use self::token::{Span, Token, TokenType};

/// The lexer handles the conversion of source code into a stream of tokens.
pub struct Lexer {
    source: Vec<char>,
    start: usize,
    current: usize,
    source_path: Rc<PathBuf>,
}

impl Lexer {
    pub fn new(source: String, source_path: Rc<PathBuf>) -> Self {
        Lexer {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            source_path,
        }
    }

    /// Scans for the next available token. Does not perform whitespace skipping.
    fn scan_token(&mut self) -> FlamaResult<Token> {
        let c = self.advance();
        match c {
            // groupings
            '(' => Ok(self.make_spanned(TokenType::LParen)),
            ')' => Ok(self.make_spanned(TokenType::RParen)),
            '{' => Ok(self.make_spanned(TokenType::LBrace)),
            '}' => Ok(self.make_spanned(TokenType::RBrace)),
            '[' => Ok(self.make_spanned(TokenType::LBracket)),
            ']' => Ok(self.make_spanned(TokenType::RBracket)),
            // arithmetic
            '+' => Ok(self.make_spanned(TokenType::Plus)),
            '-' => Ok(self.if_char_else('>', TokenType::Arrow, TokenType::Minus)),
            '*' => Ok(self.make_spanned(TokenType::Star)),
            '/' => Ok(self.make_spanned(TokenType::Slash)),
            '%' => Ok(self.make_spanned(TokenType::Modulo)),
            // boolean
            '=' => Ok({
                if self.is_match('=') {
                    self.make_spanned(TokenType::Equals)
                } else if self.is_match('>') {
                    self.make_spanned(TokenType::FatArrow)
                } else {
                    self.make_spanned(TokenType::Assign)
                }
            }),
            '!' => Ok(self.if_char_else('=', TokenType::NotEq, TokenType::Not)),
            '>' => Ok(self.if_char_else('=', TokenType::GreaterEq, TokenType::Greater)),
            '<' => Ok(self.if_char_else('=', TokenType::LessEq, TokenType::Less)),
            '|' if self.is_match('|') => Ok(self.make_spanned(TokenType::Or)),
            '&' if self.is_match('&') => Ok(self.make_spanned(TokenType::And)),
            // etc (assign and arrow above)
            ':' => Ok(self.make_spanned(TokenType::Colon)),
            ';' => Ok(self.make_spanned(TokenType::SemiColon)),
            '.' => {
                if self.peek() == '.' && self.peek_next() == '.' {
                    self.advance();
                    self.advance();
                    Ok(self.make_spanned(TokenType::Ellipsis))
                } else {
                    Ok(self.make_spanned(TokenType::Dot))
                }
            }
            ',' => Ok(self.make_spanned(TokenType::Comma)),
            // special
            '"' => self.handle_string(),
            _ if c.is_ascii_digit() => Ok(self.handle_number()),
            _ if c.is_ascii_alphabetic() || c == '_' => Ok(self.handle_identifier()),
            _ => Err(self.make_error(format!("unknown character '{}'", c))),
        }
    }

    /// Handles a string literal. Assumes that the first " has already been consumed.
    fn handle_string(&mut self) -> FlamaResult<Token> {
        let mut escaped = false;
        while !self.is_at_end() {
            let consumed = self.advance();

            if consumed == '"' && !escaped {
                return Ok(self.make_spanned(TokenType::String));
            }

            // do not handle the unescaping here, do that in the parser
            escaped = consumed == '\\';
        }

        Err(self.make_error("unterminated string".to_string()))
    }

    /// Handles a number literal. Assumes that the first digit has already been consumed.
    fn handle_number(&mut self) -> Token {
        let mut chars: Vec<char> = vec![self.source[self.current - 1]]; // include the current character

        while self.peek().is_ascii_digit() {
            chars.push(self.advance());
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance(); // consume the .
            while self.peek().is_ascii_digit() {
                chars.push(self.advance());
            }
        }

        self.make_spanned(TokenType::Number)
    }

    /// Handles an identifier or keyword. Assumes that the first character has already been consumed.
    fn handle_identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let lexeme = self.get_lexeme();
        self.make_spanned(TokenType::get_keyword(lexeme))
    }

    /// Skips the whitespace until the next meaningful character. Comments are also counted as whitespace.
    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                '\n' | '\t' | '\r' | ' ' => {
                    self.advance();
                }
                '/' => match self.peek_next() {
                    '/' => {
                        while !self.is_at_end() && self.peek() != '\n' {
                            self.advance();
                        }
                    }
                    '*' => self.multi_line_comment(),
                    _ => return,
                },
                _ => return,
            }
        }
    }

    /// Skips a multi-line comment. Assumes that the initial /* has not been consumed.
    /// This is a seperate function because it is recursive, in order to allow for nesting.
    fn multi_line_comment(&mut self) {
        self.advance(); // consume initial / and *
        self.advance();

        while !(self.peek() == '*' && self.peek_next() == '/') {
            if self.peek() == '/' && self.peek_next() == '*' {
                self.multi_line_comment(); /* allow for /* nesting */ */
            } else {
                self.advance();
            }
        }

        self.advance();
        self.advance(); // consume the last * and /
    }

    // Utility functions

    /// Returns the lexeme of the current token.
    fn get_lexeme(&self) -> String {
        self.source[self.start..self.current]
            .iter()
            .cloned()
            .collect()
    }

    /// Returns the next character in the source code. If the end of the source code has been reached, returns '\0'.
    fn peek(&self) -> char {
        // to avoid a bunch of options, just return '\0' if we are at the end
        // since most of the time this is just used as a comparison
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    /// Returns 2 characters ahead of the current character. If it is beyond the end of the source code, returns '\0'.
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    /// If the next character is the given character, consume it and returns `cons`. Otherwise, returns `alt`.
    fn if_char_else(&mut self, condition: char, cons: TokenType, alt: TokenType) -> Token {
        let result = if self.is_match(condition) { cons } else { alt };
        self.make_spanned(result)
    }

    /// If the next character is the given character, consume it and returns `true`. Otherwise, returns `false`.
    /// 
    /// Tricky because the parser also has an `is_match` function, but that one doesn't consume.
    fn is_match(&mut self, condition: char) -> bool {
        if self.peek() != condition {
            false
        } else {
            self.advance();
            true
        }
    }

    /// Consumes the next character and returns it. Does not check if the end of the source code has been reached.
    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    /// Returns true if the end of the source code has been reached.
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Creates a token with the given type and default settings.
    fn make_spanned(&self, ttype: TokenType) -> Token {
        Token {
            ttype,
            lexeme: self.get_lexeme(),
            span: Span {
                start: self.start,
                end: self.current,
            },
        }
    }

    /// Creates an error with the given message and `ErrorType::Syntax`.
    fn make_error(&self, message: String) -> FlamaError {
        FlamaError {
            message,
            span: Span {
                start: self.start,
                end: self.current,
            },
            error_type: ErrorType::Syntax,
            source_path: self.source_path.clone(),
        }
    }
}

impl Iterator for Lexer {
    type Item = FlamaResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            None
        } else {
            Some(self.scan_token())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::*;

    #[test]
    fn test_all_tokens() {
        let source = r#"
            () {} [] + - * / % ! == != > >= < <= || && = -> => . , : ; ...
            event fn let save local game if else for while continue break return true false const
            num string bool this_is_an_identifier "this_is_a_string" 1234 3.1415926
        "#;

        let expected = vec![
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::LBracket,
            TokenType::RBracket,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Modulo,
            TokenType::Not,
            TokenType::Equals,
            TokenType::NotEq,
            TokenType::Greater,
            TokenType::GreaterEq,
            TokenType::Less,
            TokenType::LessEq,
            TokenType::Or,
            TokenType::And,
            TokenType::Assign,
            TokenType::Arrow,
            TokenType::FatArrow,
            TokenType::Dot,
            TokenType::Comma,
            TokenType::Colon,
            TokenType::SemiColon,
            TokenType::Ellipsis,
            TokenType::Event,
            TokenType::Function,
            TokenType::Let,
            TokenType::Save,
            TokenType::Local,
            TokenType::Game,
            TokenType::If,
            TokenType::Else,
            TokenType::For,
            TokenType::While,
            TokenType::Continue,
            TokenType::Break,
            TokenType::Return,
            TokenType::True,
            TokenType::False,
            TokenType::Const,
            TokenType::TypeNumber,
            TokenType::TypeString,
            TokenType::TypeBoolean,
            TokenType::Identifier,
            TokenType::String,
            TokenType::Number,
            TokenType::Number,
        ];

        let lexer = Lexer::new(source.to_string(), Rc::new("test".to_string().into()));
        for (i, token) in lexer.enumerate() {
            assert_eq!(token.unwrap().ttype, expected[i]);
        }
    }
}
