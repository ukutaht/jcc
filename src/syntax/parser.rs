use errors::CompileError;
use syntax::ast::*;
use syntax::span::Tracking;
use syntax::token::{Token, TokenValue};
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
        let token = self.scanner.lookahead.clone();
        match token.value {
            TokenValue::Number(n) => {
                self.scanner.next_token();
                Ok(Expression::Literal(token.span.clone(), Literal::Number(n)))
            }
            TokenValue::String(ref s) => {
                self.scanner.next_token();
                Ok(Expression::Literal(token.span.clone(), Literal::String(s.clone())))
            }
            TokenValue::Ident(n) => {
                self.scanner.next_token();
                Ok(Expression::Identifier(token.span.clone(), n))
            }
            TokenValue::OpenSquare => {
                self.parse_array_initializer()
            },
            TokenValue::FunctionKeyword => {
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

    fn parse_static_member_property(&mut self, base: Expression) -> Result<Expression> {
        self.expect(TokenValue::Dot);
        let token = self.scanner.next_token();
        let identifier_name = match token.value {
            TokenValue::Ident(name) => name,
            TokenValue::If => "if".to_string(),
            TokenValue::Else => "else".to_string(),
            _ => return Err(CompileError::UnexpectedToken(token))
        };

        Ok(Expression::StaticMember(base.span().merge(&token.span), Box::new(base), identifier_name))
    }

    fn parse_new_expression(&mut self) -> Result<Expression> {
        let start = self.expect(TokenValue::New);
        let base = self.parse_lhs_expression(false)?;
        let args = if self.scanner.lookahead.value == TokenValue::OpenParen {
            self.parse_arguments()
        } else {
            Vec::new()
        };
        let span = start.span.to(&self.scanner.lookahead.span);

        Ok(Expression::New(span, Box::new(base), args))
    }

    // https://tc39.github.io/ecma262/#sec-left-hand-side-expressions
    fn parse_lhs_expression(&mut self, allow_call: bool) -> Result<Expression> {
        let mut result = if self.scanner.lookahead.value == TokenValue::New {
            self.parse_new_expression()?
        } else {
            self.parse_primary_expression()?
        };

        loop {
            match self.scanner.lookahead.value {
                TokenValue::OpenParen => {
                    if allow_call {
                        let args = self.parse_arguments();
                        let span = result.span().to(&self.scanner.lookahead.span);
                        result = Expression::Call(span, Box::new(result), args);
                    } else {
                        break;
                    }
                },
                TokenValue::OpenSquare => {
                    self.expect(TokenValue::OpenSquare);
                    let expr = self.parse_expression()?;
                    let end = self.expect(TokenValue::CloseSquare);
                    let span = result.span().merge(&end.span);
                    result = Expression::ComputedMember(span, Box::new(result), Box::new(expr));
                },
                TokenValue::Dot => result = self.parse_static_member_property(result)?,
                _ => break,
            }
        }

        Ok(result)
    }

    // https://tc39.github.io/ecma262/#sec-unary-operators
    fn parse_unary_expression(&mut self) -> Result<Expression> {
        let mut prefixes = Vec::new();

        while let Some(prefix) = self.match_unary_operator() {
            prefixes.push(prefix);
        }

        let mut expr = self.parse_lhs_expression(true)?;

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

    fn match_infix(&mut self) -> Option<InfixOp> {
        match self.scanner.lookahead.value {
            TokenValue::Plus => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::Plus))
            }
            TokenValue::BitXor => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::BitXor))
            }
            TokenValue::BitAnd => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::BitAnd))
            }
            TokenValue::BitOr => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::BitOr))
            }
            TokenValue::LShift => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::LShift))
            }
            TokenValue::RShift => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::RShift))
            }
            TokenValue::Times => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::Times))
            }
            TokenValue::Div => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::Div))
            }
            TokenValue::Mod => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::Mod))
            }
            TokenValue::Minus => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::Minus))
            }
            TokenValue::EqEq => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::EqEq))
            }
            TokenValue::NotEq => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::NotEq))
            }
            TokenValue::NotEqEq => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::NotEqEq))
            }
            TokenValue::EqEqEq => {
                self.scanner.next_token();
                Some(InfixOp::BinOp(BinOp::EqEqEq))
            }
            TokenValue::LogicalAnd => {
                self.scanner.next_token();
                Some(InfixOp::LogOp(LogOp::AndAnd))
            }
            TokenValue::LogicalOr => {
                self.scanner.next_token();
                Some(InfixOp::LogOp(LogOp::OrOr))
            }
            _ => None
        }
    }

    fn combine_binary(&self, frame: (Expression, InfixOp), right: Expression) -> Expression {
        let span = frame.0.span().merge(right.span());

        match frame.1 {
            InfixOp::BinOp(op) => Expression::Binary(span, op, Box::new(frame.0), Box::new(right)),
            InfixOp::LogOp(op) => Expression::Logical(span, op, Box::new(frame.0), Box::new(right)),
        }
    }

    fn parse_binary_expression(&mut self) -> Result<Expression> {
        let mut stack: Vec<(Expression, InfixOp)> = Vec::new();
        let mut left = self.parse_unary_expression()?;

        while let Some(op) = self.match_infix() {
            while stack.len() > 0 && stack.last().unwrap().1.precedence() >= op.precedence() {
                left = self.combine_binary(stack.pop().unwrap(), left);
            }
            stack.push((left, op));
            left = self.parse_unary_expression()?;
        }

        while stack.len() > 0 {
            left = self.combine_binary(stack.pop().unwrap(), left);
        }
        Ok(left)
    }

    // https://tc39.github.io/ecma262/#sec-conditional-operator
    fn parse_conditional_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression()
    }

    // https://tc39.github.io/ecma262/#sec-assignment-operators
    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        let left = self.parse_conditional_expression()?;
        match self.scanner.lookahead.value {
            TokenValue::Eq => {
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
        let start = self.expect(TokenValue::OpenSquare);
        let mut elements = Vec::new();

        loop {
            if let TokenValue::CloseSquare = self.scanner.lookahead.value {
                break;
            }
            if let TokenValue::Comma = self.scanner.lookahead.value {
                self.scanner.next_token();
                elements.push(None);
                continue;
            } else {
                elements.push(Some(self.parse_assignment_expression()?));
            }

            if self.scanner.lookahead.value != TokenValue::CloseSquare {
                self.expect(TokenValue::Comma);
            }
        }

        let end = self.expect(TokenValue::CloseSquare);
        Ok(Expression::Array(start.span.merge(&end.span), elements))
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

        let id = match self.scanner.lookahead.clone().value {
            TokenValue::Ident(name) => {
                self.scanner.next_token();
                Some(name)
            }
            TokenValue::OpenParen => {
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

    fn expect(&mut self, expected: TokenValue) -> Token {
        let next = self.scanner.next_token();

        if next.value != expected {
            panic!("Expected {:?}, got {:?}", expected, next);
        }

        next
    }
}
