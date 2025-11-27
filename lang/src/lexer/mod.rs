mod token;

#[cfg(test)]
mod tests;

pub use token::{Span, Token, TokenKind};

use std::str::Chars;

pub struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    position: usize,
    line: u32,
    column: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
    pub message: String,
    pub line: u32,
    pub column: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        let start_position = self.position;
        let start_line = self.line;
        let start_column = self.column;

        let kind = match self.peek() {
            None => TokenKind::Eof,
            Some(c) => match c {
                '/' if self.peek_next() == Some('/') => {
                    self.skip_comment();
                    return self.next_token();
                }

                '(' => {
                    self.advance();
                    TokenKind::LeftParen
                }
                ')' => {
                    self.advance();
                    TokenKind::RightParen
                }
                '[' => {
                    self.advance();
                    TokenKind::LeftBracket
                }
                ']' => {
                    self.advance();
                    TokenKind::RightBracket
                }
                '{' => {
                    self.advance();
                    TokenKind::LeftBrace
                }
                '}' => {
                    self.advance();
                    TokenKind::RightBrace
                }
                ',' => {
                    self.advance();
                    TokenKind::Comma
                }
                ':' => {
                    self.advance();
                    TokenKind::Colon
                }
                ';' => {
                    self.advance();
                    TokenKind::Semicolon
                }
                '`' => {
                    self.advance();
                    TokenKind::Backtick
                }

                '#' if self.peek_next() == Some('{') => {
                    self.advance();
                    self.advance();
                    TokenKind::HashBrace
                }

                '+' => {
                    self.advance();
                    TokenKind::Plus
                }
                '*' => {
                    self.advance();
                    TokenKind::Star
                }
                '/' => {
                    self.advance();
                    TokenKind::Slash
                }
                '%' => {
                    self.advance();
                    TokenKind::Percent
                }

                '-' => {
                    self.advance();
                    TokenKind::Minus
                }

                '.' if self.peek_next() == Some('.') => {
                    self.advance();
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::DotDotEqual
                    } else {
                        TokenKind::DotDot
                    }
                }

                '=' if self.peek_next() == Some('=') => {
                    self.advance();
                    self.advance();
                    TokenKind::EqualEqual
                }
                '=' => {
                    self.advance();
                    TokenKind::Equal
                }

                '!' if self.peek_next() == Some('=') => {
                    self.advance();
                    self.advance();
                    TokenKind::BangEqual
                }
                '!' => {
                    self.advance();
                    TokenKind::Bang
                }

                '<' if self.peek_next() == Some('=') => {
                    self.advance();
                    self.advance();
                    TokenKind::LessEqual
                }
                '<' => {
                    self.advance();
                    TokenKind::Less
                }

                '>' if self.peek_next() == Some('>') => {
                    self.advance();
                    self.advance();
                    TokenKind::GreaterGreater
                }
                '>' if self.peek_next() == Some('=') => {
                    self.advance();
                    self.advance();
                    TokenKind::GreaterEqual
                }
                '>' => {
                    self.advance();
                    TokenKind::Greater
                }

                '&' if self.peek_next() == Some('&') => {
                    self.advance();
                    self.advance();
                    TokenKind::AmpAmp
                }

                '|' if self.peek_next() == Some('>') => {
                    self.advance();
                    self.advance();
                    TokenKind::PipeGreater
                }
                '|' if self.peek_next() == Some('|') => {
                    self.advance();
                    self.advance();
                    TokenKind::PipePipe
                }
                '|' => {
                    self.advance();
                    TokenKind::Pipe
                }

                '_' if !self.is_identifier_continue(self.peek_next()) => {
                    self.advance();
                    TokenKind::Underscore
                }

                '"' => self.string()?,

                c if c.is_ascii_digit() => self.number()?,

                c if self.is_identifier_start(c) => self.identifier_or_keyword(),

                c => {
                    return Err(LexError {
                        message: format!("Unexpected character: '{c}'"),
                        line: start_line,
                        column: start_column,
                    });
                }
            },
        };

        let span = Span {
            start: start_position,
            end: self.position,
            line: start_line,
            column: start_column,
        };

        Ok(Token { kind, span })
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek_next(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.position += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(c)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        self.advance();
        self.advance();
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn is_identifier_start(&self, c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_identifier_continue(&self, c: Option<char>) -> bool {
        c.is_some_and(|c| c.is_ascii_alphanumeric() || c == '_' || c == '?')
    }

    fn identifier_or_keyword(&mut self) -> TokenKind {
        let start = self.position;

        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' || c == '?' {
                self.advance();
            } else {
                break;
            }
        }

        let text = &self.source[start..self.position];

        match text {
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "match" => TokenKind::Match,
            "return" => TokenKind::Return,
            "break" => TokenKind::Break,
            "nil" => TokenKind::Nil,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Identifier(text.to_string()),
        }
    }

    fn number(&mut self) -> Result<TokenKind, LexError> {
        let start = self.position;
        let start_line = self.line;
        let start_column = self.column;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();

            while let Some(c) = self.peek() {
                if c.is_ascii_digit() || c == '_' {
                    self.advance();
                } else {
                    break;
                }
            }

            let text = &self.source[start..self.position];
            let cleaned: String = text.chars().filter(|&c| c != '_').collect();
            let value: f64 = cleaned.parse().map_err(|_| LexError {
                message: format!("Invalid decimal literal: {text}"),
                line: start_line,
                column: start_column,
            })?;

            Ok(TokenKind::Decimal(value))
        } else {
            let text = &self.source[start..self.position];
            let cleaned: String = text.chars().filter(|&c| c != '_').collect();
            let value: i64 = cleaned.parse().map_err(|_| LexError {
                message: format!("Invalid integer literal: {text}"),
                line: start_line,
                column: start_column,
            })?;

            Ok(TokenKind::Integer(value))
        }
    }

    fn string(&mut self) -> Result<TokenKind, LexError> {
        let start_line = self.line;
        let start_column = self.column;

        self.advance();

        let mut value = String::new();

        loop {
            match self.peek() {
                None => {
                    return Err(LexError {
                        message: "Unterminated string literal".to_string(),
                        line: start_line,
                        column: start_column,
                    });
                }
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.peek() {
                        Some('n') => {
                            self.advance();
                            value.push('\n');
                        }
                        Some('t') => {
                            self.advance();
                            value.push('\t');
                        }
                        Some('\\') => {
                            self.advance();
                            value.push('\\');
                        }
                        Some('"') => {
                            self.advance();
                            value.push('"');
                        }
                        Some(c) => {
                            return Err(LexError {
                                message: format!("Invalid escape sequence: \\{c}"),
                                line: self.line,
                                column: self.column,
                            });
                        }
                        None => {
                            return Err(LexError {
                                message: "Unterminated string literal".to_string(),
                                line: start_line,
                                column: start_column,
                            });
                        }
                    }
                }
                Some(c) => {
                    self.advance();
                    value.push(c);
                }
            }
        }

        Ok(TokenKind::String(value))
    }
}
