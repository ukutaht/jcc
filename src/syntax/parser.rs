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

    pub fn parse_primary_expression(&mut self) -> Expression {
        let tok = self.scanner.next_token();

        match tok {
            Token::Number(n) => Expression::Literal(Literal::Number(n)),
            Token::String(s) => Expression::Literal(Literal::String(s)),
            Token::Ident(n) => Expression::Identifier(n),
            Token::OpenSquare => self.parse_array_initializer(),
            t => panic!("Bad token to start expression: {:?}", t),
        }
    }

    fn parse_arguments(&mut self, base: Expression) -> Expression {
        self.expect(Token::OpenParen);
        let mut arguments = Vec::new();

        loop {
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            } else {
                let argument = ArgumentListElement::Expression(self.parse_assignment_expression());
                arguments.push(argument);

                if self.scanner.lookahead != Token::CloseParen {
                    self.expect(Token::Comma);
                }
            }
        }

        self.expect(Token::CloseParen);
        Expression::Call(Box::new(base), arguments)
    }

    fn parse_call_expression(&mut self) -> Expression {
        let base = self.parse_primary_expression();
        match self.scanner.lookahead {
            Token::OpenParen => self.parse_arguments(base),
            _ => base,
        }
    }

    // https://tc39.github.io/ecma262/#sec-left-hand-side-expressions
    fn parse_lhs_expression(&mut self) -> Expression {
        self.parse_call_expression()
    }

    // https://tc39.github.io/ecma262/#sec-unary-operators
    fn parse_unary_expression(&mut self) -> Expression {
        self.parse_lhs_expression()
    }

    // https://tc39.github.io/ecma262/#sec-conditional-operator
    fn parse_conditional_expression(&mut self) -> Expression {
        self.parse_unary_expression()
    }

    // https://tc39.github.io/ecma262/#sec-assignment-operators
    fn parse_assignment_expression(&mut self) -> Expression {
        self.parse_conditional_expression()
    }

    // https://tc39.github.io/ecma262/#sec-array-initializer
    fn parse_array_initializer(&mut self) -> Expression {
        let mut elements = Vec::new();

        loop {
            if let Token::CloseSquare = self.scanner.lookahead {
                break;
            } else {
                elements.push(self.parse_assignment_expression());

                if self.scanner.lookahead != Token::CloseSquare {
                    self.expect(Token::Comma);
                }
            }
        }

        self.expect(Token::CloseSquare);
        Expression::Array(elements)
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_variable_statement(&mut self) -> Statement {
        self.expect(Token::Var);
        let ident = self.scanner.next_token();

        if let Token::Ident(name) = ident {
            self.expect(Token::Equals);
            let init = self.parse_primary_expression();
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

    fn parse_function_parameter(&mut self) -> FunctionParameter {
        let token = self.scanner.next_token();

        if let Token::Ident(name) = token {
            FunctionParameter::Binding(Binding::Identifier(name))
        } else {
            panic!("ONLY IDENTIFIERS IN PARAMETERS PLZ")
        }
    }

    fn parse_function_parameters(&mut self) -> Vec<FunctionParameter> {
        self.expect(Token::OpenParen);
        let mut parameters = Vec::new();

        loop {
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            }
            parameters.push(self.parse_function_parameter());
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            }
            self.expect(Token::Comma);
        }

        self.expect(Token::CloseParen);

        parameters
    }

    // https://tc39.github.io/ecma262/#sec-function-definitions
    fn parse_function_declaration(&mut self) -> Statement {
        self.expect(Token::FunctionKeyword);

        let id = match self.scanner.lookahead {
            Token::Ident(name) => {
                self.scanner.next_token();
                Some(name)
            }
            Token::OpenParen => None,
            _ => panic!("Unexpected token"),
        };

        let parameters = self.parse_function_parameters();
        let block = self.parse_block();

        Statement::FunctionDeclaration(FunctionDeclaration {
            id: id,
            body: block,
            parameters: parameters,
        })
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_assignment_expression()
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expr = self.parse_assignment_expression();
        Statement::Expression(expr)
    }

    fn parse_statement(&mut self) -> Statement {
        match self.scanner.lookahead {
            Token::Var => self.parse_variable_statement(),
            Token::FunctionKeyword => self.parse_function_declaration(),
            Token::If => self.parse_if_statement(),
            Token::Number(_) | Token::String(_) | Token::OpenSquare | Token::Ident(_) => {
                self.parse_expression_statement()
            },
            Token::OpenCurly => Statement::Block(self.parse_block()),
            token => panic!("Could not parse statement. Got {:?}", token),
        }
    }

    fn parse_if_statement(&mut self) -> Statement {
        self.expect(Token::If);
        self.expect(Token::OpenParen);
        let test = self.parse_expression();
        self.expect(Token::CloseParen);
        let then = self.parse_statement();
        let alternate = match self.scanner.lookahead {
            Token::Else => {
                self.scanner.next_token();
                Some(Box::new(self.parse_statement()))
            },
            _ => None
        };

        Statement::If(test, Box::new(then), alternate)
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_statement_list_item(&mut self) -> StatementListItem {
        StatementListItem::Statement(self.parse_statement())
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
