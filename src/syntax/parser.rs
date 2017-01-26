use syntax::ast::*;
use syntax::token::Token;
use syntax::scanner::Scanner;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
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
            Token::Number(n) => Expression::Literal(Literal::Number(n)),
            Token::String(s) => Expression::Literal(Literal::String(s)),
            Token::Eof => panic!("END"),
            Token::Var => panic!("VAR"),
            Token::FunctionKeyword => panic!("FUNCTION"),
            Token::Ident(_) => panic!("Ident"),
            Token::Equals => panic!("Equals"),
            Token::OpenParen => panic!("OpenParen"),
            Token::CloseParen => panic!("CloseParen"),
            Token::OpenCurly => panic!("OpenCurly"),
            Token::CloseCurly => panic!("CloseCurly"),
        }
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_variable_statement(&mut self) -> Statement {
        self.expect(Token::Var);
        let ident = self.scanner.next_token();

        if let Token::Ident(name) = ident {
            self.expect(Token::Equals);
            let init = self.parse_expression();
            Statement::VariableDeclaration(VariableDeclaration {
                kind: VariableDeclarationKind::Var,
                declarations: vec![VariableDeclarator {
                                       id: name,
                                       init: Some(init),
                                   }],
            })
        } else {
            panic!("Wat");
        }
    }

    // https://tc39.github.io/ecma262/#sec-function-definitions
    fn parse_function_declaration(&mut self) -> Statement {
        self.expect(Token::FunctionKeyword);
        let next = self.scanner.next_token();

        if let Token::Ident(name) = next {
            self.expect(Token::OpenParen);
            self.expect(Token::CloseParen);

            let block = self.parse_block();

            Statement::FunctionDeclaration(FunctionDeclaration {
                id: Some(name),
                body: block,
            })
        } else {
            panic!("Function needs a name!");
        }
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expr = self.parse_expression();
        Statement::Expression(expr)
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_statement_list_item(&mut self) -> StatementListItem {
        match self.scanner.lookahead {
            Token::Var => StatementListItem::Statement(self.parse_variable_statement()),
            Token::FunctionKeyword => {
                StatementListItem::Statement(self.parse_function_declaration())
            }
            Token::Number(_) | Token::String(_) => {
                StatementListItem::Statement(self.parse_expression_statement())
            }
            token => panic!("Could not parse statement list item. Got {:?}", token),
        }
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_block(&mut self) -> Block {
        self.expect(Token::OpenCurly);
        let mut statements = Vec::new();

        while self.scanner.lookahead != Token::CloseCurly {
            statements.push(self.parse_statement_list_item());
        }

        self.expect(Token::CloseCurly);
        Block(statements)
    }

    fn expect(&mut self, expected: Token) {
        let next = self.scanner.next_token();

        if next != expected {
            panic!("Expected {:?}, got {:?}", expected, next);
        }
    }
}
