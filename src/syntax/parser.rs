use syntax::char::ESCharExt;
use syntax::ast::*;
use syntax::intern::{intern, Name};
use std::mem;

#[derive(Debug, PartialEq)]
enum Token {
    Var,
    Ident(Name),
    String(Name),
    Eof,
    Number(f64),
    Equals,
    FunctionKeyword,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
}

pub struct Parser<'a> {
    source: &'a str,
    index: usize,
    lookahead: Token
}

lazy_static! {
    static ref KEYWORD_VAR: Name = intern("var");
    static ref KEYWORD_FUNCTION: Name = intern("function");
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        let mut parser = Parser { source: source, index: 0, lookahead: Token::Eof };
        parser.position_at_start();
        parser
    }

    fn position_at_start(&mut self) {
        self.lookahead = self.lex();
    }

    pub fn parse(&mut self) -> Program {
        let mut body = Vec::new();

        while self.lookahead != Token::Eof {
            body.push(self.parse_statement_list_item());
        }

        Program(body)
    }

    pub fn parse_expression(&mut self) -> Expression {
        let tok = self.next_token();

        match tok {
            Token::Eof => panic!("END"),
            Token::Var => return self.parse_assignment(),
            Token::FunctionKeyword => return self.parse_function(),
            Token::Ident(_) => panic!("Ident"),
            Token::Number(n) => Expression::Literal(Literal::Number(n)),
            Token::String(s) => Expression::Literal(Literal::String(s)),
            Token::Equals => panic!("Equals"),
            Token::OpenParen => panic!("OpenParen"),
            Token::CloseParen => panic!("CloseParen"),
            Token::OpenCurly => panic!("OpenCurly"),
            Token::CloseCurly => panic!("CloseCurly"),
        }
    }

    fn parse_variable_statement(&mut self) -> Statement {
        let ident = self.next_token();

        if let Token::Ident(name) = ident {
            self.expect(Token::Equals);
            let init = self.parse_expression();
            Statement::VariableDeclaration(VariableDeclaration {
                kind: VariableDeclarationKind::Var,
                declarations: vec![VariableDeclarator{id: name, init: Some(init)}]
            })
        } else {
            panic!("Wat");
        }
    }

    fn parse_function_declaration(&mut self) -> Statement {
        let next = self.next_token();

        if let Token::Ident(name) = next {
            self.expect(Token::OpenParen);
            self.expect(Token::CloseParen);

            let block = self.parse_block();

            Statement::FunctionDeclaration(FunctionDeclaration{id: Some(name), body: block})
        } else {
            panic!("Function needs a name!");
        }
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_statement_list_item(&mut self) -> StatementListItem {
        match self.next_token() {
            Token::Var => StatementListItem::Statement(self.parse_variable_statement()),
            Token::FunctionKeyword => StatementListItem::Statement(self.parse_function_declaration()),
            token => panic!("Could not parse statement list item. Got {:?}", token)
        }
    }

    fn parse_block(&mut self) -> Block {
      self.expect(Token::OpenCurly);
      let mut statements = Vec::new();

      while self.lookahead != Token::CloseCurly {
        statements.push(self.parse_statement_list_item());
      }

      self.expect(Token::CloseCurly);
      Block(statements)
    }

    fn parse_function(&mut self) -> Expression {
        let next = self.next_token();

        if let Token::Ident(name) = next {
            self.expect(Token::OpenParen);
            self.expect(Token::CloseParen);

            let block = self.parse_block();

            Expression::Function(Some(name), block)
        } else {
            panic!("Function needs a name!");
        }
    }

    fn parse_assignment(&mut self) -> Expression {
        let ident = self.next_token();

        if let Token::Ident(n) = ident {
            let left = Box::new(Expression::Identifier(n));
            self.expect(Token::Equals);
            let right = Box::new(self.parse_expression());
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
        let token = mem::replace(&mut self.lookahead, Token::Eof);
        self.lookahead = self.lex();
        token
    }

    fn lex(&mut self) -> Token {
        if self.is_eof() {
            return Token::Eof;
        }
        let mut character = self.current_char().unwrap();

        while character.is_es_whitespace() {
            self.bump();
            character = self.current_char().unwrap();
        }

        if character.is_es_identifier_start() {
            self.scan_identifier()
        } else if character.is_es_quote() {
            self.scan_string()
        } else if character.is_digit(10) {
            self.scan_number()
        } else if character == '=' {
            self.bump();
            return Token::Equals
        } else if character == '(' {
            self.bump();
            return Token::OpenParen
        } else if character == ')' {
            self.bump();
            return Token::CloseParen
        } else if character == '{' {
            self.bump();
            return Token::OpenCurly
        } else if character == '}' {
            self.bump();
            return Token::CloseCurly
        } else {
            panic!("Unknown character: {}", character);
        }
    }

    fn scan_number(&mut self) -> Token {
        let start = self.bump();

        while !self.is_eof() {
            let ch = self.current_char().unwrap();

            if ch.is_digit(10) {
                self.bump();
            } else if ch == '.' {
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

            if ch == quote {
                break;
            } else {
                self.bump();
            }
        }

        let start_without_quote = start + 1;
        Token::String(intern(&self.source[start_without_quote..self.index]))
    }

    fn scan_identifier(&mut self) -> Token {
        let value = self.get_identifier();

        if value == *KEYWORD_VAR {
           Token::Var
        } else if value == *KEYWORD_FUNCTION {
           Token::FunctionKeyword
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

        let result = intern(&self.source[start..self.index]);
        result
    }

    fn bump(&mut self) -> usize {
        let current = self.index;
        self.index += 1;
        current
    }

    fn current_char(&mut self) -> Option<char> {
        self.source.chars().nth(self.index)
    }

    fn nth_char(&mut self, nth: usize) -> Option<char> {
        self.source.chars().nth(nth)
    }

    fn is_eof(&mut self) -> bool {
        self.current_char().is_none()
    }
}
