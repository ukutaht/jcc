use syntax::char::ESCharExt;
use syntax::span::{Span, Position};
use syntax::token::{Token, TokenValue};
use std::str;
use std::mem;

static KEYWORD_VAR: &'static str = "var";
static KEYWORD_FUNCTION: &'static str = "function";
static KEYWORD_IF: &'static str = "if";
static KEYWORD_ELSE: &'static str = "else";
static KEYWORD_NEW: &'static str = "new";

pub struct Scanner<'a> {
    bytes: &'a [u8],
    index: usize,
    line: u32,
    column: u32,
    pub lookahead: Token,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner {
        Scanner {
            bytes: source.as_bytes(),
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
        let tok = self.lex();
        mem::replace(&mut self.lookahead, tok)
    }

    fn pos(&self) -> Position {
        Position { column: self.column, line: self.line }
    }

    fn lex(&mut self) -> Token {
        let character;

        loop {
            match self.current_byte() {
                Some(b'\n') => {
                    self.line += 1;
                    self.column = 0;
                    self.next_byte();
                }
                Some(b' ') => {
                    self.next_byte();
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
        let value = if character == b'\'' ||character == b'"' {
            self.scan_string(character)
        } else if (character as char).is_digit(10) {
            self.scan_number()
        } else if self.eat_byte(b'=') {
            if self.eat_byte(b'=') {
                if self.eat_byte(b'=') {
                    TokenValue::EqEqEq
                } else {
                    TokenValue::EqEq
                }
            } else {
                TokenValue::Eq
            }
        } else if self.eat_byte(b'(') {
            TokenValue::OpenParen
        } else if self.eat_byte(b')') {
            TokenValue::CloseParen
        } else if self.eat_byte(b'{') {
            TokenValue::OpenCurly
        } else if self.eat_byte(b'}') {
            TokenValue::CloseCurly
        } else if self.eat_byte(b'[') {
            TokenValue::OpenSquare
        } else if self.eat_byte(b']') {
            TokenValue::CloseSquare
        } else if self.eat_byte(b'+') {
            TokenValue::Plus
        } else if self.eat_byte(b'^') {
            TokenValue::BitXor
        } else if self.eat_byte(b'*') {
            TokenValue::Times
        } else if self.eat_byte(b'/') {
            TokenValue::Div
        } else if self.eat_byte(b'%') {
            TokenValue::Mod
        } else if self.eat_byte(b',') {
            TokenValue::Comma
        } else if character == b'.' {
            match self.peek_byte() {
                Some(ch) if (ch as char).is_digit(10) => {
                    self.scan_number()
                }
                _ => {
                    self.next_byte();
                    TokenValue::Dot
               }
            }
        } else if self.eat_byte(b'!') {
            if self.eat_byte(b'=') {
                if self.eat_byte(b'=') {
                    TokenValue::NotEqEq
                } else {
                    TokenValue::NotEq
                }
            } else {
                TokenValue::Bang
            }
        } else if self.eat_byte(b'-') {
            TokenValue::Minus
        } else if self.eat_byte(b'&') {
            if self.eat_byte(b'&') {
                TokenValue::LogicalAnd
            } else {
                panic!("Something with &")
            }
        } else if self.eat_byte(b'|') {
            if self.eat_byte(b'|') {
                TokenValue::LogicalOr
            } else {
                panic!("Something with |")
            }
        } else if self.current_char().unwrap().is_es_identifier_start() {
            self.scan_identifier()
        } else {
            panic!("Unknown character: {}", self.current_char().unwrap());
        };

        Token { value: value, span: Span { start: start, end: self.pos() } }
    }

    fn scan_number(&mut self) -> TokenValue {
        match (self.current_byte(), self.peek_byte()) {
            (Some(b'0'), Some(b'x')) | (Some(b'0'), Some(b'X')) => {
                self.scan_hex()
            }
            _ => self.scan_float()
        }

    }

    fn scan_hex(&mut self) -> TokenValue {
        self.next_byte();self.next_byte();
        let start = self.index;
        self.take_while(|c| (c as char).is_es_hex_digit());
        let hex = unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]) };

        let value = u32::from_str_radix(hex, 16).unwrap() as f64;
        TokenValue::Number(value)
    }

    fn scan_float(&mut self) -> TokenValue {
        let start = self.index;
        self.skip_digits();

        if self.eat_byte(b'.') {
            self.skip_digits()
        }

        if self.eat_byte(b'e') {
            self.eat_byte(b'+') || self.eat_byte(b'-');

            match self.current_byte() {
                Some(ch) if (ch as char).is_digit(10) => {
                    self.skip_digits();
                }
                _ => panic!("Invalid exponent")
            }
        }

        let float = unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]) }.parse().unwrap();
        TokenValue::Number(float)
    }

    fn skip_digits(&mut self) {
        self.take_while(|c| (c as char).is_digit(10));
    }

    fn scan_string(&mut self, quote: u8) -> TokenValue {
        let start = self.index;
        self.eat_byte(quote);
        self.take_while(|b| b != quote);
        self.eat_byte(quote);

        let string = unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]) }.to_string();
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
        let start = self.index;
        self.take_chars_while(|c| (c as char).is_es_identifier_continue());
        str::from_utf8(&self.bytes[start..self.index]).unwrap().to_string()
    }

    fn take_while<F>(&mut self, predicate: F) where F: Fn(u8) -> bool {
        while let Some(ch) = self.current_byte() {
            if predicate(ch) {
                self.next_byte();
            } else {
                break;
            }
        }
    }

    fn take_chars_while<F>(&mut self, predicate: F) where F: Fn(char) -> bool {
        while let Some(ch) = self.current_char() {
            if predicate(ch) {
                self.next_char();
            } else {
                break;
            }
        }
    }

    fn current_byte(&self) -> Option<u8> {
        self.get_byte(self.index)
    }

    fn peek_byte(&self) -> Option<u8> {
        self.get_byte(self.index + 1)
    }

    fn get_byte(&self, index: usize) -> Option<u8> {
      self.bytes.get(index).map(|b| *b)
    }

    fn current_char(&mut self) -> Option<char> {
      unsafe { str::from_utf8_unchecked(&self.bytes[self.index..]) }.chars().next()
    }

    fn next_byte(&mut self) -> Option<u8> {
        self.current_byte().map(|b| {
            debug_assert!(b < 128);
            self.index += 1;
            self.column += 1;
            b
        })
    }

    fn eat_byte(&mut self, byte: u8) -> bool {
        if let Some(b) = self.current_byte() {
            if b == byte {
                self.next_byte();
                return true
            }
            return false;
        }
        return false;
    }

    fn next_char(&mut self) -> Option<char> {
        unsafe { str::from_utf8_unchecked(&self.bytes[self.index..]) }.chars().next().map(|c| {
            self.index += c.len_utf8();
            self.column += 1;
            c
        })
    }
}
