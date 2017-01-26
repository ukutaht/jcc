use syntax::char::ESCharExt;
use syntax::ast::*;
use syntax::intern::{intern, Name};
use syntax::token::Token;
use syntax::scanner::Scanner;
use std::mem;

lazy_static! {
    static ref KEYWORD_VAR: Name = intern("var");
    static ref KEYWORD_FUNCTION: Name = intern("function");
}


pub struct Parser<'a> {
    scanner: Scanner<'a>
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        let mut scanner = Scanner::new(source);
        scanner.position_at_start();
        Parser { scanner: scanner }
    }

    pub fn parse(&mut self) -> Program {
        let mut body = Vec::new();

        while self.scanner.lookahead != Token::Eof {
            body.push(self.parse_statement_list_item());
        }

        Program(body)
    }

    pub fn parse_expression(&mut self) -> Expression {
        let tok = self.scanner.next_token();

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
        let ident = self.scanner.next_token();

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
        let next = self.scanner.next_token();

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
        match self.scanner.next_token() {
            Token::Var => StatementListItem::Statement(self.parse_variable_statement()),
            Token::FunctionKeyword => StatementListItem::Statement(self.parse_function_declaration()),
            token => panic!("Could not parse statement list item. Got {:?}", token)
        }
    }

    fn parse_block(&mut self) -> Block {
      self.expect(Token::OpenCurly);
      let mut statements = Vec::new();

      while self.scanner.lookahead != Token::CloseCurly {
        statements.push(self.parse_statement_list_item());
      }

      self.expect(Token::CloseCurly);
      Block(statements)
    }

    fn parse_function(&mut self) -> Expression {
        let next = self.scanner.next_token();

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
        let ident = self.scanner.next_token();

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
        let next = self.scanner.next_token();

        if next != expected {
            panic!("Expected {:?}, got {:?}", expected, next);
        }
    }
}
