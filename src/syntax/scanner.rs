use syntax::char::ESCharExt;
use syntax::token::Token;
use syntax::intern::{intern, Name};
use std::mem;

lazy_static! {
    static ref KEYWORD_VAR: Name = intern("var");
    static ref KEYWORD_FUNCTION: Name = intern("function");
    static ref KEYWORD_IF: Name = intern("if");
    static ref KEYWORD_ELSE: Name = intern("else");
}


pub struct Scanner<'a> {
    source: &'a str,
    index: usize,
    pub lookahead: Token,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner {
        Scanner {
            source: source,
            index: 0,
            lookahead: Token::Eof,
        }
    }

    pub fn position_at_start(&mut self) {
        self.lookahead = self.lex();
    }

    pub fn next_token(&mut self) -> Token {
        let token = mem::replace(&mut self.lookahead, Token::Eof);
        self.lookahead = self.lex();
        token
    }

    fn lex(&mut self) -> Token {
        let character;

        loop {
            match self.current_char() {
                None => return Token::Eof,
                Some('\n') => self.bump(),
                Some(c) if c.is_es_whitespace() => self.bump(),
                Some(c) => {
                    character = c;
                    break;
                }
            };
        }

        if character.is_es_identifier_start() {
            self.scan_identifier()
        } else if character.is_es_quote() {
            self.scan_string()
        } else if character.is_digit(10) {
            self.scan_number()
        } else if character == '=' {
            self.bump();
            Token::Equals
        } else if character == '(' {
            self.bump();
            Token::OpenParen
        } else if character == ')' {
            self.bump();
            Token::CloseParen
        } else if character == '{' {
            self.bump();
            Token::OpenCurly
        } else if character == '}' {
            self.bump();
            Token::CloseCurly
        } else if character == '[' {
            self.bump();
            Token::OpenSquare
        } else if character == ']' {
            self.bump();
            Token::CloseSquare
        } else if character == '+' {
            self.bump();
            Token::Plus
        } else if character == ',' {
            self.bump();
            Token::Comma
        } else if character == '.' {
            self.bump();
            Token::Dot
        } else if character == '!' {
            self.bump();
            Token::Bang
        } else if character == '-' {
            self.bump();
            Token::Minus
        } else {
            panic!("Unknown character: {}", character);
        }
    }

    fn scan_number(&mut self) -> Token {
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
        Token::Number(value)
    }

    fn scan_string(&mut self) -> Token {
        let start = self.bump();
        let quote = self.nth_char(start);

        while !self.is_eof() {
            let ch = self.current_char();
            self.bump();

            if ch == quote {
                break;
            }
        }

        Token::String(intern(&self.source[start..self.index]))
    }

    fn scan_identifier(&mut self) -> Token {
        let value = self.get_identifier();

        if value == *KEYWORD_VAR {
            Token::Var
        } else if value == *KEYWORD_FUNCTION {
            Token::FunctionKeyword
        } else if value == *KEYWORD_IF {
            Token::If
        } else if value == *KEYWORD_ELSE {
            Token::Else
        } else {
            Token::Ident(value)
        }
    }

    fn get_identifier(&mut self) -> Name {
        let start = self.bump();

        while !self.is_eof() {
            let ch = self.current_char().unwrap();

            if ch.is_es_identifier_continue() {
                self.bump();
            } else {
                break;
            }
        }

        intern(&self.source[start..self.index])
    }

    fn bump(&mut self) -> usize {
        let current = self.index;
        self.index += 1;
        current
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
