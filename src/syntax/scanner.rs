use syntax::char::ESCharExt;
use syntax::span::{Span, Position};
use syntax::token::{Token, TokenValue};
use std::str::Chars;
use std::mem;

static KEYWORD_VAR: &'static str = "var";
static KEYWORD_FUNCTION: &'static str = "function";
static KEYWORD_IF: &'static str = "if";
static KEYWORD_ELSE: &'static str = "else";
static KEYWORD_NEW: &'static str = "new";

pub struct Scanner<'a> {
    chars: Chars<'a>,
    current_char: Option<char>,
    line: u32,
    column: u32,
    pub lookahead: Token,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner {
        Scanner {
            chars: source.chars(),
            current_char: None,
            column: 0,
            line: 1,
            lookahead: Token {value: TokenValue::Eof, span: Span::initial() }
        }
    }

    pub fn position_at_start(&mut self) {
        self.current_char = self.chars.next();
        self.lookahead = self.lex();
    }

    pub fn next_token(&mut self) -> Token {
        let tok = self.lex();
        mem::replace(&mut self.lookahead, tok)
    }

    fn lex(&mut self) -> Token {
        let character;

        loop {
            match self.current_char() {
                Some('\n') => {
                    self.line += 1;
                    self.column = 0;
                    self.bump();
                }
                Some(c) if c.is_es_whitespace() => {
                    self.bump();
                }
                Some(c) => {
                    character = c;
                    break;
                }
                None => {
                    return Token { value: TokenValue::Eof, span: Span { start: self.pos(), end: self.pos() } }
                }
            };
        }

        let start = self.pos();
        let value = if character.is_es_identifier_start() {
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
        } else if character == '|' {
            self.bump();
            match self.expect_current_char() {
                '|' => {
                    self.bump();
                    TokenValue::LogicalOr
                },
                _ => panic!("Something with |")
            }
        } else {
            panic!("Unknown character: {}", character);
        };

        Token { value: value, span: Span { start: start, end: self.pos() } }
    }

    fn pos(&self) -> Position {
        Position { column: self.column, line: self.line }
    }

    fn scan_number(&mut self) -> TokenValue {
        let mut num = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_digit(10) || ch == '.' {
                num.push(ch);
                self.bump();
            } else {
                break;
            }
        }

        let value: f64 = num.parse().unwrap();
        TokenValue::Number(value)
    }

    fn scan_string(&mut self) -> TokenValue {
        let quote = self.expect_current_char();
        self.bump();
        let mut string = quote.to_string();

        while let Some(ch) = self.current_char {
            string.push(ch);
            self.bump();

            if ch == quote {
                break;
            }
        }

        TokenValue::String(string)
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
        let mut result = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_es_identifier_continue() {
                result.push(ch);
                self.bump();
            } else {
                break;
            }
        }

        result
    }

    fn bump(&mut self) -> Option<char> {
        self.current_char = self.chars.next();
        self.column += 1;
        self.current_char
    }

    fn expect_current_char(&self) -> char {
        self.current_char.expect("Unexpected end of input")
    }

    fn current_char(&self) -> Option<char> {
        self.current_char
    }
}
