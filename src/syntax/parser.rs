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

    fn parse_static_member_property(&mut self, base: Expression) -> Expression {
        self.expect(Token::Dot);
        match self.scanner.next_token() {
            Token::Ident(name) => {
                Expression::StaticMember(Box::new(base), name)
            },
            _ => panic!("Unexpected thing in member property")
        }
    }

    fn parse_call_expression(&mut self) -> Expression {
        let mut result = self.parse_primary_expression();

        loop {
            match self.scanner.lookahead {
                Token::OpenParen => result = self.parse_arguments(result),
                Token::Dot       => result = self.parse_static_member_property(result),
                _ => break,
            }
        }

        result
    }

    // https://tc39.github.io/ecma262/#sec-left-hand-side-expressions
    fn parse_lhs_expression(&mut self) -> Expression {
        self.parse_call_expression()
    }

    // https://tc39.github.io/ecma262/#sec-unary-operators
    fn parse_unary_expression(&mut self) -> Expression {
        let mut prefixes = Vec::new();

        while let Some(prefix) = self.match_unary_operator() {
            prefixes.push(prefix);
        }

        let mut expr = self.parse_lhs_expression();

        for prefix in prefixes.into_iter().rev() {
            expr = Expression::Unary(prefix, Box::new(expr))
        }

        return expr;
    }

    fn match_unary_operator(&mut self) -> Option<UnOp> {
        match self.scanner.lookahead {
            Token::Bang => {
                self.scanner.next_token();
                Some(UnOp::Not)
            },
            Token::Minus => {
                self.scanner.next_token();
                Some(UnOp::Minus)
            },
            _ => None
        }
    }

    fn parse_more_infix_expressions(&mut self, left: Expression) -> Expression {
        if let Some(op) = self.match_infix() {
            let right = self.parse_unary_expression();
            match op {
                InfixOp::BinOp(bin_op) => Expression::Binary(bin_op, Box::new(left), Box::new(right)),
                InfixOp::LogOp(log_op) => Expression::Logical(log_op, Box::new(left), Box::new(right)),
            }
        } else {
            left
        }
    }

    fn match_infix(&mut self) -> Option<InfixOp> {
        match self.scanner.lookahead {
            Token::Plus => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::Plus))
            }
            Token::EqEq => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::EqEq))
            }
            Token::EqEqEq => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::EqEqEq))
            }
            Token::LogicalAnd => {
                self.scanner.next_token();
                Some(InfixOp::LogOp(LogOp::AndAnd))
            }
            _ => None
        }
    }

    // https://tc39.github.io/ecma262/#sec-conditional-operator
    fn parse_conditional_expression(&mut self) -> Expression {
        let left = self.parse_unary_expression();
        self.parse_more_infix_expressions(left)
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
            self.expect(Token::Eq);
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

    fn parse_function_parameter(&mut self) -> Pattern {
        let token = self.scanner.next_token();

        if let Token::Ident(name) = token {
            Pattern::Identifier(name)
        } else {
            panic!("ONLY IDENTIFIERS IN PARAMETERS PLZ")
        }
    }

    fn parse_function_parameters(&mut self) -> Vec<Pattern> {
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
        let expr = self.parse_expression();
        Statement::Expression(expr)
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

    fn parse_statement(&mut self) -> Statement {
        match self.scanner.lookahead {
            Token::Var => self.parse_variable_statement(),
            Token::FunctionKeyword => self.parse_function_declaration(),
            Token::If => self.parse_if_statement(),
            Token::OpenCurly => Statement::Block(self.parse_block()),
            _ => self.parse_expression_statement(),
        }
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
