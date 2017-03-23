use errors::CompileError;
use syntax::ast::*;
use syntax::span::{Tracking, Span, Position};
use syntax::token::Token;
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

        while self.scanner.lookahead != Token::Eof {
            body.push(self.parse_statement_list_item()?);
        }

        Ok(Program(body))
    }

    fn finalize(&self, start: Position) -> Span {
        Span {
            start: start,
            end: self.scanner.last_pos.clone()
        }
    }

    pub fn parse_primary_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        let token = self.scanner.lookahead.clone();
        match token {
            Token::Number(n) => {
                self.scanner.next_token();
                Ok(Expression::Literal(self.finalize(start), Literal::Number(n)))
            }
            Token::String(ref s) => {
                self.scanner.next_token();
                Ok(Expression::Literal(self.finalize(start), Literal::String(s.clone())))
            }
            Token::Ident(n) => {
                self.scanner.next_token();
                Ok(Expression::Identifier(self.finalize(start), n))
            }
            Token::OpenSquare => {
                self.parse_array_initializer()
            },
            Token::OpenParen => {
                self.parse_group_expression()
            },
            Token::FunctionKeyword => {
                self.parse_function().map(Expression::Function)
            },
            _ => Err(CompileError::UnexpectedToken(token.clone()))
        }
    }

    fn parse_group_expression(&mut self) -> Result<Expression> {
        self.expect(Token::OpenParen);
        let result = self.parse_assignment_expression()?;
        self.expect(Token::CloseParen);
        Ok(result)
    }

    fn parse_arguments(&mut self) -> Vec<ArgumentListElement> {
        self.expect(Token::OpenParen);
        let mut arguments = Vec::new();

        loop {
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            } else {
                let argument = ArgumentListElement::Expression(self.parse_assignment_expression().unwrap());
                arguments.push(argument);

                if self.scanner.lookahead != Token::CloseParen {
                    self.expect(Token::Comma);
                }
            }
        }

        self.expect(Token::CloseParen);
        arguments
    }

    fn parse_new_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::New);
        let base = self.parse_lhs_expression(false)?;
        let args = if self.scanner.lookahead == Token::OpenParen {
            self.parse_arguments()
        } else {
            Vec::new()
        };
        Ok(Expression::New(self.finalize(start), Box::new(base), args))
    }

    // https://tc39.github.io/ecma262/#sec-left-hand-side-expressions
    fn parse_lhs_expression(&mut self, allow_call: bool) -> Result<Expression> {
        let start = self.scanner.lookahead_start;

        let mut result = if self.scanner.lookahead == Token::New {
            self.parse_new_expression()?
        } else {
            self.parse_primary_expression()?
        };

        loop {
            match self.scanner.lookahead {
                Token::OpenParen => {
                    if allow_call {
                        let args = self.parse_arguments();
                        let span = self.finalize(start);
                        result = Expression::Call(span, Box::new(result), args);
                    } else {
                        break;
                    }
                },
                Token::OpenSquare => {
                    self.expect(Token::OpenSquare);
                    let expr = self.parse_expression()?;
                    self.expect(Token::CloseSquare);
                    let span = self.finalize(start);
                    result = Expression::ComputedMember(span, Box::new(result), Box::new(expr));
                },
                Token::Dot => {
                    self.scanner.next_token();
                    let token = self.scanner.next_token();
                    let identifier_name = match token {
                        Token::Ident(name) => name,
                        Token::If => "if".to_string(),
                        Token::Else => "else".to_string(),
                        _ => return Err(CompileError::UnexpectedToken(token))
                    };

                    result = Expression::StaticMember(self.finalize(start), Box::new(result), identifier_name)

                }
                _ => break,
            }
        }

        Ok(result)
    }

    // https://tc39.github.io/ecma262/#sec-unary-operators
    fn parse_unary_expression(&mut self) -> Result<Expression> {
        let mut prefixes = Vec::new();

        while let Some(prefix) = self.match_unary_operator() {
            let start = self.scanner.lookahead_start;
            self.scanner.next_token();
            prefixes.push((prefix, self.finalize(start)));
        }

        let mut expr = self.parse_lhs_expression(true)?;

        for (prefix, span) in prefixes.into_iter().rev() {
            expr = Expression::Unary(span.merge(expr.span()), prefix, Box::new(expr))
        }

        Ok(expr)
    }

    fn match_unary_operator(&mut self) -> Option<UnOp> {
        match self.scanner.lookahead {
            Token::Bang => Some(UnOp::Not),
            Token::Minus => Some(UnOp::Minus),
            Token::Plus => Some(UnOp::Plus),
            _ => None
        }
    }

    fn match_infix(&mut self) -> Option<InfixOp> {
        match self.scanner.lookahead {
            Token::Plus => {
                Some(InfixOp::BinOp(BinOp::Plus))
            }
            Token::BitXor => {
                Some(InfixOp::BinOp(BinOp::BitXor))
            }
            Token::BitAnd => {
                Some(InfixOp::BinOp(BinOp::BitAnd))
            }
            Token::BitOr => {
                Some(InfixOp::BinOp(BinOp::BitOr))
            }
            Token::LShift => {
                Some(InfixOp::BinOp(BinOp::LShift))
            }
            Token::RShift => {
                Some(InfixOp::BinOp(BinOp::RShift))
            }
            Token::URShift => {
                Some(InfixOp::BinOp(BinOp::URShift))
            }
            Token::Times => {
                Some(InfixOp::BinOp(BinOp::Times))
            }
            Token::Div => {
                Some(InfixOp::BinOp(BinOp::Div))
            }
            Token::Mod => {
                Some(InfixOp::BinOp(BinOp::Mod))
            }
            Token::Minus => {
                Some(InfixOp::BinOp(BinOp::Minus))
            }
            Token::EqEq => {
                Some(InfixOp::BinOp(BinOp::EqEq))
            }
            Token::NotEq => {
                Some(InfixOp::BinOp(BinOp::NotEq))
            }
            Token::NotEqEq => {
                Some(InfixOp::BinOp(BinOp::NotEqEq))
            }
            Token::EqEqEq => {
                Some(InfixOp::BinOp(BinOp::EqEqEq))
            }
            Token::LogicalAnd => {
                Some(InfixOp::LogOp(LogOp::AndAnd))
            }
            Token::LogicalOr => {
                Some(InfixOp::LogOp(LogOp::OrOr))
            }
            _ => None
        }
    }

    fn combine_binary(&self, operator: InfixOp, left: Expression, right: Expression, start: Position) -> Expression {
        let span = Span {
            start: start,
            end: self.scanner.last_pos.clone()
        };

        match operator {
            InfixOp::BinOp(op) => Expression::Binary(span, op, Box::new(left), Box::new(right)),
            InfixOp::LogOp(op) => Expression::Logical(span, op, Box::new(left), Box::new(right)),
        }
    }

    fn parse_binary_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;

        let mut expr = self.parse_unary_expression()?;

        if let Some(first_op) = self.match_infix() {
            self.scanner.next_token();
            let mut markers = vec![start, self.scanner.lookahead_start];
            let mut right = self.parse_unary_expression()?;
            let mut expressions = vec![expr, right];
            let mut operators = vec![first_op];

            while let Some(op) = self.match_infix() {
                while expressions.len() > 1 && operators.last().unwrap().precedence() >= op.precedence() {
                    right = expressions.pop().unwrap();
                    let operator = operators.pop().unwrap();
                    expr = expressions.pop().unwrap();
                    markers.pop();
                    let marker = markers.last().unwrap().clone();
                    expressions.push(self.combine_binary(operator, expr, right, marker))
                }
                self.scanner.next_token();
                operators.push(op);
                markers.push(self.scanner.lookahead_start);
                expressions.push(self.parse_unary_expression()?)
            }

            expr = expressions.pop().unwrap();
            markers.pop();

            while expressions.len() > 0 {
                let operator = operators.pop().unwrap();
                let left = expressions.pop().unwrap();
                expr = self.combine_binary(operator, left , expr, markers.pop().unwrap());
            }
        }

        Ok(expr)
    }

    // https://tc39.github.io/ecma262/#sec-conditional-operator
    fn parse_conditional_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression()
    }

    // https://tc39.github.io/ecma262/#sec-assignment-operators
    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        let left = self.parse_conditional_expression()?;
        match self.scanner.lookahead {
            Token::Eq => {
                self.scanner.next_token();
                match left {
                    Expression::Identifier(_, _) => {
                        let right = self.parse_assignment_expression()?;
                        let span = left.span().merge(right.span());
                        Ok(Expression::Assignment(span, AssignOp::Eq, Box::new(left), Box::new(right)))
                    }
                    _ => panic!("Invalid assignment target")
                }
            }
            _ => Ok(left)
        }
    }

    // https://tc39.github.io/ecma262/#sec-array-initializer
    fn parse_array_initializer(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::OpenSquare);
        let mut elements = Vec::new();

        loop {
            if let Token::CloseSquare = self.scanner.lookahead {
                break;
            }
            if let Token::Comma = self.scanner.lookahead {
                self.scanner.next_token();
                elements.push(None);
                continue;
            } else {
                elements.push(Some(self.parse_assignment_expression()?));
            }

            if self.scanner.lookahead != Token::CloseSquare {
                self.expect(Token::Comma);
            }
        }

        self.expect(Token::CloseSquare);
        Ok(Expression::Array(self.finalize(start), elements))
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_variable_statement(&mut self) -> Result<Statement> {
        self.expect(Token::Var);
        if let Token::Ident(name) = self.scanner.next_token() {
            self.expect(Token::Eq);
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
        if let Token::Ident(name) = self.scanner.next_token() {
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
    fn parse_function(&mut self) -> Result<Function> {
        self.expect(Token::FunctionKeyword);

        let id = match self.scanner.lookahead.clone() {
            Token::Ident(name) => {
                self.scanner.next_token();
                Some(name)
            }
            Token::OpenParen => {
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
        Ok(Statement::Expression(self.parse_expression()?))
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        self.expect(Token::If);
        self.expect(Token::OpenParen);
        let test = self.parse_expression()?;
        self.expect(Token::CloseParen);
        let then = self.parse_statement()?;
        let alternate = match self.scanner.lookahead {
            Token::Else => {
                self.scanner.next_token();
                Some(Box::new(self.parse_statement()?))
            },
            _ => None
        };

        Ok(Statement::If(test, Box::new(then), alternate))
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.scanner.lookahead {
            Token::Var => self.parse_variable_statement(),
            Token::FunctionKeyword => {
                self.parse_function().map(Statement::FunctionDeclaration)
            },
            Token::If => self.parse_if_statement(),
            Token::OpenCurly => self.parse_block().map(Statement::Block),
            _ => self.parse_expression_statement(),
        }
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_statement_list_item(&mut self) -> Result<StatementListItem> {
        self.parse_statement().map(StatementListItem::Statement)
    }

    // https://tc39.github.io/ecma262/#sec-block
    fn parse_block(&mut self) -> Result<Block> {
        self.expect(Token::OpenCurly);
        let mut statements = Vec::new();

        while self.scanner.lookahead != Token::CloseCurly {
            statements.push(self.parse_statement_list_item()?);
        }

        self.expect(Token::CloseCurly);
        Ok(Block(statements))
    }

    fn expect(&mut self, expected: Token) -> Token {
        let next = self.scanner.next_token();

        if next != expected {
            panic!("Expected {:?}, got {:?}", expected, next);
        }

        next
    }
}
