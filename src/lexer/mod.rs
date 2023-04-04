pub mod token;

use std::{path::PathBuf, rc::Rc};

use crate::{ErrorType, FlamaError, FlamaResult};

use self::token::{Span, Token, TokenType};

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
            '>' => Ok(self.if_char_else('=', TokenType::GreaterEq, TokenType::RArrow)),
            '<' => Ok(self.if_char_else('=', TokenType::LessEq, TokenType::LArrow)),
            '|' if self.is_match('|') => Ok(self.make_spanned(TokenType::Or)),
            '&' if self.is_match('&') => Ok(self.make_spanned(TokenType::And)),
            // etc (assign and arrow above)
            ':' => Ok(self.make_spanned(TokenType::Colon)),
            ';' => Ok(self.make_spanned(TokenType::SemiColon)),
            '.' => {
                // handle ellipsis
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
            _ if c.is_ascii_alphabetic() => Ok(self.handle_identifier()),
            _ => Err(self.make_error(format!("unknown character '{}'", c))),
        }
    }

    fn handle_string(&mut self) -> FlamaResult<Token> {
        while !self.is_at_end() {
            if self.peek() != '\\' && self.peek_next() == '"' {
                self.advance();
                self.advance(); // consume the "
                return Ok(self.make_spanned(TokenType::String));
            }
            self.advance();
        }

        Err(self.make_error("unterminated string".to_string()))
    }

    fn handle_number(&mut self) -> Token {
        let mut chars: Vec<char> = vec![self.source[self.current - 1]]; // include the current character

        // allow underscores for stuff like 1_000_000_000
        // we could do some type of check to prevent double underscores
        // or underscores before the decimal point, but this is fine for now
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

    fn handle_identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let lexeme = self.get_lexeme();
        self.make_spanned(TokenType::get_keyword(lexeme))
    }

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

    fn get_lexeme(&self) -> String {
        self.source[self.start..self.current]
            .iter()
            .cloned()
            .collect()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn if_char_else(&mut self, condition: char, cons: TokenType, alt: TokenType) -> Token {
        let result = if self.is_match(condition) { cons } else { alt };
        self.make_spanned(result)
    }

    fn is_match(&mut self, condition: char) -> bool {
        if self.peek() != condition {
            false
        } else {
            self.advance();
            true
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    // abcdef

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
        if self.is_at_end() {
            None
        } else {
            self.skip_whitespace();
            self.start = self.current;
            Some(self.scan_token())
        }
    }
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_tokens() {
        let expectations = vec![
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::LBracket,
            Token::RBracket,
            Token::Add,
            Token::Increment,
            Token::Minus,
            Token::Decrement,
            Token::Multiply,
            Token::Power,
            Token::Divide,
            Token::Modulo,
            Token::Negate,
            Token::Equals,
            Token::NotEq,
            Token::Greater,
            Token::GreaterEq,
            Token::Less,
            Token::LessEq,
            Token::Or,
            Token::And,
            Token::Assign,
            Token::Arrow,
            Token::Dot,
            Token::Comma,
            Token::Colon,
            Token::SemiColon,
            Token::Event,
            Token::Function,
            Token::Let,
            Token::If,
            Token::Else,
            Token::For,
            Token::While,
            Token::Continue,
            Token::Break,
            Token::Return,
            Token::True,
            Token::False,
            Token::Print,
        ];

        let src = r"
            ( ) { } [ ]
            + ++ - --
            * ** / %
            ! == != > >=
            < <= || &&
            = -> . , : ;

            event
            function
            let
            if
            else
            for
            while
            continue
            break
            return
            true
            false
            print
        ";

        let lexer = Lexer::new(src.to_string());
        for (got, expected) in lexer.zip(expectations.into_iter()) {
            let token = got.unwrap().val;

            assert_eq!(expected, token, "expected {expected} but got {token}");
        }
    }

    #[test]
    fn test_special() {
        let expectations = vec![Token::String("hello".to_string())];

        let src = r#"
            "hello"
        "#;

        let lexer = Lexer::new(src.to_string());
        for (got, expected) in lexer.zip(expectations.into_iter()) {
            let token = got.unwrap().val;

            assert_eq!(expected, token, "expected {expected} but got {token}");
        }
    }
}
*/
