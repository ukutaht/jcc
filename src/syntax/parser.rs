use errors::CompileError;
use syntax::ast::*;
use syntax::span::Tracking;
use syntax::token::{TokenValue};
use syntax::scanner::Scanner;
use std;

pub type Result<T> = std::result::Result<T, CompileError>;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        let mut scanner = Scanner::new(source);
        scanner.position_at_start();
        Parser { scanner: scanner }
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut body = Vec::new();

        while self.scanner.lookahead.value != TokenValue::Eof {
            body.push(self.parse_statement_list_item()?);
        }

        Ok(Program(body))
    }

    pub fn parse_primary_expression(&mut self) -> Result<Expression> {
        let token = self.scanner.next_token();
        match token.value {
            TokenValue::Number(n) => {
                Ok(Expression::Literal(token.span.clone(), Literal::Number(n)))
            }
            TokenValue::String(ref s) => {
                Ok(Expression::Literal(token.span.clone(), Literal::String(s.clone())))
            }
            TokenValue::Ident(n) => {
                Ok(Expression::Identifier(token.span.clone(), n))
            }
            TokenValue::OpenSquare => {
                self.scanner.back(token);
                self.parse_array_initializer()
            },
            TokenValue::FunctionKeyword => {
                self.scanner.back(token);
                self.parse_function().map(Expression::Function)
            },
            _ => Err(CompileError::UnexpectedToken(token.clone()))
        }
    }

    fn parse_arguments(&mut self) -> Vec<ArgumentListElement> {
        self.expect(TokenValue::OpenParen);
        let mut arguments = Vec::new();

        loop {
            if let TokenValue::CloseParen = self.scanner.lookahead.value {
                break;
            } else {
                let argument = ArgumentListElement::Expression(self.parse_assignment_expression().unwrap());
                arguments.push(argument);

                if self.scanner.lookahead.value != TokenValue::CloseParen {
                    self.expect(TokenValue::Comma);
                }
            }
        }

        self.expect(TokenValue::CloseParen);
        arguments
    }

    fn parse_static_member_property(&mut self, base: Expression) -> Expression {
        self.expect(TokenValue::Dot);
        match self.scanner.next_token().value {
            TokenValue::Ident(name) => {
                Expression::StaticMember(Box::new(base), name)
            },
            _ => panic!("Unexpected thing in member property")
        }
    }

    fn parse_call_expression(&mut self) -> Result<Expression> {
        let mut result = self.parse_primary_expression()?;

        loop {
            match self.scanner.lookahead.value {
                TokenValue::OpenParen => {
                    let args = self.parse_arguments();
                    result = Expression::Call(Box::new(result), args);
                },
                TokenValue::Dot => result = self.parse_static_member_property(result),
                _ => break,
            }
        }

        Ok(result)
    }

    fn parse_new_expression(&mut self) -> Result<Expression> {
        self.expect(TokenValue::New);
        let base = self.parse_primary_expression()?;
        let args = if self.scanner.lookahead.value == TokenValue::OpenParen {
            self.parse_arguments()
        } else {
            Vec::new()
        };

        Ok(Expression::New(Box::new(base), args))
    }

    // https://tc39.github.io/ecma262/#sec-left-hand-side-expressions
    fn parse_lhs_expression(&mut self) -> Result<Expression> {
        if self.scanner.lookahead.value == TokenValue::New {
            self.parse_new_expression()
        } else {
            self.parse_call_expression()
        }
    }

    // https://tc39.github.io/ecma262/#sec-unary-operators
    fn parse_unary_expression(&mut self) -> Result<Expression> {
        let mut prefixes = Vec::new();

        while let Some(prefix) = self.match_unary_operator() {
            prefixes.push(prefix);
        }

        let mut expr = self.parse_lhs_expression()?;

        for prefix in prefixes.into_iter().rev() {
            expr = Expression::Unary(prefix, Box::new(expr))
        }

        Ok(expr)
    }

    fn match_unary_operator(&mut self) -> Option<UnOp> {
        match self.scanner.lookahead.value {
            TokenValue::Bang => {
                self.scanner.next_token();
                Some(UnOp::Not)
            },
            TokenValue::Minus => {
                self.scanner.next_token();
                Some(UnOp::Minus)
            },
            _ => None
        }
    }

    fn parse_more_infix_expressions(&mut self, left: Expression) -> Result<Expression> {
        if let Some(op) = self.match_infix() {
            let right = self.parse_unary_expression()?;
            match op {
                InfixOp::BinOp(bin_op) => {
                    let span = left.span().to(right.span());
                    Ok(Expression::Binary(span, bin_op, Box::new(left), Box::new(right)))
                }
                InfixOp::LogOp(log_op) => Ok(Expression::Logical(log_op, Box::new(left), Box::new(right))),
            }
        } else {
            Ok(left)
        }
    }

    fn match_infix(&mut self) -> Option<InfixOp> {
        match self.scanner.lookahead.value {
            TokenValue::Plus => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::Plus))
            }
            TokenValue::EqEq => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::EqEq))
            }
            TokenValue::EqEqEq => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::EqEqEq))
            }
            TokenValue::LogicalAnd => {
                self.scanner.next_token();
                Some(InfixOp::LogOp(LogOp::AndAnd))
            }
            _ => None
        }
    }

    // https://tc39.github.io/ecma262/#sec-conditional-operator
    fn parse_conditional_expression(&mut self) -> Result<Expression> {
        let left = self.parse_unary_expression()?;
        self.parse_more_infix_expressions(left)
    }

    // https://tc39.github.io/ecma262/#sec-assignment-operators
    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        self.parse_conditional_expression()
    }

    // https://tc39.github.io/ecma262/#sec-array-initializer
    fn parse_array_initializer(&mut self) -> Result<Expression> {
        self.expect(TokenValue::OpenSquare);
        let mut elements = Vec::new();

        loop {
            if let TokenValue::CloseSquare = self.scanner.lookahead.value {
                break;
            } else {
                elements.push(self.parse_assignment_expression()?);

                if self.scanner.lookahead.value != TokenValue::CloseSquare {
                    self.expect(TokenValue::Comma);
                }
            }
        }

        self.expect(TokenValue::CloseSquare);
        Ok(Expression::Array(elements))
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_variable_statement(&mut self) -> Result<Statement> {
        self.expect(TokenValue::Var);
        if let TokenValue::Ident(name) = self.scanner.next_token().value {
            self.expect(TokenValue::Eq);
            let init = self.parse_primary_expression()?;
            Ok(Statement::VariableDeclaration(VariableDeclaration {
                kind: VariableDeclarationKind::Var,
                declarations: vec![VariableDeclarator {
                                       id: name,
                                       init: Some(init),
                                   }],
            }))
        } else {
            panic!("Wat");
        }
    }

    fn parse_function_parameter(&mut self) -> Pattern {
        if let TokenValue::Ident(name) = self.scanner.next_token().value {
            Pattern::Identifier(name)
        } else {
            panic!("ONLY IDENTIFIERS IN PARAMETERS PLZ")
        }
    }

    fn parse_function_parameters(&mut self) -> Vec<Pattern> {
        self.expect(TokenValue::OpenParen);
        let mut parameters = Vec::new();

        loop {
            if let TokenValue::CloseParen = self.scanner.lookahead.value {
                break;
            }
            parameters.push(self.parse_function_parameter());
            if let TokenValue::CloseParen = self.scanner.lookahead.value {
                break;
            }
            self.expect(TokenValue::Comma);
        }

        self.expect(TokenValue::CloseParen);

        parameters
    }

    // https://tc39.github.io/ecma262/#sec-function-definitions
    fn parse_function(&mut self) -> Result<Function> {
        self.expect(TokenValue::FunctionKeyword);

        let token = self.scanner.next_token();
        let id = match token.value {
            TokenValue::Ident(name) => Some(name),
            TokenValue::OpenParen => {
                self.scanner.back(token);
                None
            }
            _ => panic!("Unexpected token"),
        };

        let parameters = self.parse_function_parameters();
        let block = self.parse_block()?;

        Ok(Function { id: id, body: block, parameters: parameters })
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_assignment_expression()
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expr = self.parse_expression()?;
        Ok(Statement::Expression(expr))
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        self.expect(TokenValue::If);
        self.expect(TokenValue::OpenParen);
        let test = self.parse_expression()?;
        self.expect(TokenValue::CloseParen);
        let then = self.parse_statement()?;
        let alternate = match self.scanner.lookahead.value {
            TokenValue::Else => {
                self.scanner.next_token();
                Some(Box::new(self.parse_statement()?))
            },
            _ => None
        };

        Ok(Statement::If(test, Box::new(then), alternate))
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.scanner.lookahead.value {
            TokenValue::Var => self.parse_variable_statement(),
            TokenValue::FunctionKeyword => {
                self.parse_function().map(Statement::FunctionDeclaration)
            },
            TokenValue::If => self.parse_if_statement(),
            TokenValue::OpenCurly => self.parse_block().map(Statement::Block),
            _ => self.parse_expression_statement(),
        }
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_statement_list_item(&mut self) -> Result<StatementListItem> {
        self.parse_statement().map(StatementListItem::Statement)
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_block(&mut self) -> Result<Block> {
        self.expect(TokenValue::OpenCurly);
        let mut statements = Vec::new();

        while self.scanner.lookahead.value != TokenValue::CloseCurly {
            statements.push(self.parse_statement_list_item()?);
        }

        self.expect(TokenValue::CloseCurly);
        Ok(Block(statements))
    }

    fn expect(&mut self, expected: TokenValue) {
        let next = self.scanner.next_token();

        if next.value != expected {
            panic!("Expected {:?}, got {:?}", expected, next);
        }
    }
}
