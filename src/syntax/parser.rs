use syntax::char::ESCharExt;
use syntax::ast::{Expression, AssignmentType, Literal};
use syntax::intern::{intern, Name};

#[derive(Debug, PartialEq)]
enum Token {
    Var,
    Ident(Name),
    Eof,
    Number(f64),
    Equals
}

pub struct Parser<'a> {
    source: &'a str,
    index: usize
}

lazy_static! {
    static ref KEYWORD_VAR: Name = intern("var");
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        Parser { source: source, index: 0 }
    }

    pub fn parse(&mut self) -> Expression {
        let tok = self.next_token();

        match tok {
            Token::Eof => panic!("END"),
            Token::Var => return self.parse_assignment(),
            Token::Ident(_) => panic!("Ident"),
            Token::Number(n) => Expression::Literal(Literal::Number(n)),
            Token::Equals => panic!("Equals")
        }
    }

    fn parse_assignment(&mut self) -> Expression {
        let ident = self.next_token();

        if let Token::Ident(n) = ident {
            let left = Box::new(Expression::Identifier(n));
            self.expect(Token::Equals);
            let right = Box::new(self.parse());
            Expression::Assign(AssignmentType::Var, left, right)
        } else {
            panic!("Wat");
        }
    }

    fn expect(&mut self, expected: Token) {
        let next = self.next_token();

        if next != expected {
            panic!("Expected {:?}, got {:?}", expected, next);
        }
    }

    fn next_token(&mut self) -> Token {
        if self.is_eof() {
            return Token::Eof;
        }
        let mut character = self.current_char().unwrap();

        while character.is_es_whitespace() {
            self.index += 1;
            character = self.current_char().unwrap();
        }

        if character.is_es_identifier_start() {
            self.scan_identifier()
        } else if character.is_digit(10) {
            self.scan_number()
        } else if character == '=' {
            self.index += 1;
            return Token::Equals
        } else {
            panic!("Unknown character: {}", character);
        }
    }

    fn scan_number(&mut self) -> Token {
        let start = self.index;
        self.index += 1;

        while !self.is_eof() {
            let ch = self.current_char().unwrap();

            if ch.is_digit(10) {
                self.index += 1;
            } else {
                break;
            }
        }

        let number_string = &self.source[start..self.index];
        let value: f64 = number_string.parse().unwrap();
        self.index += 1;
        Token::Number(value)
    }

    fn scan_identifier(&mut self) -> Token {
        let value = self.get_identifier();

        if value == *KEYWORD_VAR {
            Token::Var
        } else {
           Token::Ident(value)
        }
    }

    fn get_identifier(&mut self) -> Name {
        let start = self.index;
        self.index += 1;

        while !self.is_eof() {
            let ch = self.current_char().unwrap();

            if ch.is_es_identifier_continue() {
                self.index += 1;
            } else {
                break;
            }
        }

        let result = intern(&self.source[start..self.index]);
        self.index += 1;
        result
    }

    fn current_char(&mut self) -> Option<char> {
        self.source.chars().nth(self.index)
    }

    fn is_eof(&mut self) -> bool {
        self.current_char().is_none()
    }
}
