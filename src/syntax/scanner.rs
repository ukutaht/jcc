use syntax::char::ESCharExt;
use syntax::span::{Span, Position};
use syntax::token::{Token, TokenValue};
use std::mem;

static KEYWORD_VAR: &'static str = "var";
static KEYWORD_FUNCTION: &'static str = "function";
static KEYWORD_IF: &'static str = "if";
static KEYWORD_ELSE: &'static str = "else";
static KEYWORD_NEW: &'static str = "new";

pub struct Scanner<'a> {
    source: &'a str,
    index: usize,
    line: u32,
    column: u32,
    pub lookahead: Token,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner {
        Scanner {
            source: source,
            index: 0,
            column: 0,
            line: 1,
            lookahead: Token {value: TokenValue::Eof, span: Span::initial() }
        }
    }

    pub fn position_at_start(&mut self) {
        self.lookahead = self.lex();
    }

    pub fn next_token(&mut self) -> Token {
        let token = mem::replace(&mut self.lookahead, Token {value: TokenValue::Eof, span: Span::initial() });
        self.lookahead = self.lex();
        token
    }

    pub fn back(&mut self, token: Token) {
        self.lookahead = token;
        self.index -= 1;
        self.column -= 1;
    }

    fn lex(&mut self) -> Token {
        self.skip_whitespace();
        let start = self.pos();
        let value = self.lex_value();
        Token { value:  value, span: Span { start: start, end: self.pos() } }
    }

    fn pos(&self) -> Position {
        Position { column: self.column, line: self.line }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.current_char() {
                Some('\n') => {
                    self.line += 1;
                    self.column = 0;
                    self.bump();
                }
                Some(c) if c.is_es_whitespace() => {
                    self.bump();
                },
                _ => break
            };
        }
    }

    fn lex_value(&mut self) -> TokenValue {
        if let None = self.current_char() {
            return TokenValue::Eof;
        }

        let character = self.expect_current_char();

        if character.is_es_identifier_start() {
            self.scan_identifier()
        } else if character.is_es_quote() {
            self.scan_string()
        } else if character.is_digit(10) {
            self.scan_number()
        } else if character == '=' {
            self.bump();
            match self.expect_current_char() {
                '=' => {
                    self.bump();
                    match self.expect_current_char() {
                        '=' => {
                            self.bump();
                            TokenValue::EqEqEq
                        },
                        _ => TokenValue::EqEq
                    }
                },
                _ => TokenValue::Eq
            }

        } else if character == '(' {
            self.bump();
            TokenValue::OpenParen
        } else if character == ')' {
            self.bump();
            TokenValue::CloseParen
        } else if character == '{' {
            self.bump();
            TokenValue::OpenCurly
        } else if character == '}' {
            self.bump();
            TokenValue::CloseCurly
        } else if character == '[' {
            self.bump();
            TokenValue::OpenSquare
        } else if character == ']' {
            self.bump();
            TokenValue::CloseSquare
        } else if character == '+' {
            self.bump();
            TokenValue::Plus
        } else if character == ',' {
            self.bump();
            TokenValue::Comma
        } else if character == '.' {
            self.bump();
            TokenValue::Dot
        } else if character == '!' {
            self.bump();
            match self.expect_current_char() {
                '=' => {
                    self.bump();
                    match self.expect_current_char() {
                        '=' => {
                            self.bump();
                            TokenValue::NotEqEq
                        },
                        _ => TokenValue::NotEq
                    }
                },
                _ => TokenValue::Bang
            }
        } else if character == '-' {
            self.bump();
            TokenValue::Minus
        } else if character == '&' {
            self.bump();
            match self.expect_current_char() {
                '&' => {
                    self.bump();
                    TokenValue::LogicalAnd
                },
                _ => panic!("Something with &")
            }
        } else {
            panic!("Unknown character: {}", character);
        }
    }

    fn scan_number(&mut self) -> TokenValue {
        let start = self.bump();

        while !self.is_eof() {
            let ch = self.current_char().unwrap();

            if ch.is_digit(10) || ch == '.' {
                self.bump();
            } else {
                break;
            }
        }

        let number_string = &self.source[start..self.index];
        let value: f64 = number_string.parse().unwrap();
        TokenValue::Number(value)
    }

    fn scan_string(&mut self) -> TokenValue {
        let start = self.bump();
        let quote = self.nth_char(start);

        while !self.is_eof() {
            let ch = self.current_char();
            self.bump();

            if ch == quote {
                break;
            }
        }

        TokenValue::String(self.source[start..self.index].to_string())
    }

    fn scan_identifier(&mut self) -> TokenValue {
        let value = self.get_identifier();

        if value == *KEYWORD_VAR {
            TokenValue::Var
        } else if value == *KEYWORD_FUNCTION {
            TokenValue::FunctionKeyword
        } else if value == *KEYWORD_IF {
            TokenValue::If
        } else if value == *KEYWORD_ELSE {
            TokenValue::Else
        } else if value == *KEYWORD_NEW {
            TokenValue::New
        } else {
            TokenValue::Ident(value)
        }
    }

    fn get_identifier(&mut self) -> String {
        let start_index = self.bump();

        while !self.is_eof() {
            let ch = self.current_char().unwrap();

            if ch.is_es_identifier_continue() {
                self.bump();
            } else {
                break;
            }
        }

        self.source[start_index..self.index].to_string()
    }

    fn bump(&mut self) -> usize {
        let current = self.index;
        self.index += 1;
        self.column += 1;
        current
    }

    fn expect_current_char(&self) -> char {
        self.source.chars().nth(self.index).expect("Unexpected end of input")
    }

    fn current_char(&self) -> Option<char> {
        self.source.chars().nth(self.index)
    }

    fn nth_char(&self, nth: usize) -> Option<char> {
        self.source.chars().nth(nth)
    }

    fn is_eof(&self) -> bool {
        self.current_char().is_none()
    }
}
