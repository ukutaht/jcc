use errors::{CompileError, ErrorCause, Result};
use syntax::ast::*;
use syntax::span::{Span, Position};
use syntax::token::Token;
use syntax::scanner::Scanner;
use syntax::ops::AsOperator;
use std::collections::HashSet;
use std;

struct ParseContext {
    allow_in: bool,
    in_iteration: bool,
    in_switch: bool,
    in_function_body: bool,
    labels: HashSet<String>
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    context: ParseContext,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        Parser {
            scanner: Scanner::new(source),
            context: ParseContext {
                allow_in: true,
                in_iteration: false,
                in_switch: false,
                in_function_body: false,
                labels: HashSet::new()
            }
        }
    }

    fn directive_opt(&mut self, expr: &Expression) -> Option<String> {
        if let Expression::Literal(_, Literal::String(ref val)) = *expr {
            let len = val.len();
            Some(val.as_str()[1..len - 1].to_owned())
        } else {
            None
        }
    }

    fn parse_directive(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;

        let expr = self.parse_expression()?;
        match self.directive_opt(&expr) {
            Some(dir) => Ok(Statement::Directive(self.consume_semicolon(start)?, expr, dir)),
            None => Ok(Statement::Expression(self.consume_semicolon(start)?, expr))
        }
    }

    fn parse_directive_prologues(&mut self) -> Result<Vec<StatementListItem>> {
        let mut directives = Vec::new();

        while let Token::String(_) = self.scanner.lookahead {
            let dir = self.parse_directive()?;
            directives.push(StatementListItem::Statement(dir))
        }

        Ok(directives)
    }

    pub fn parse(&mut self) -> Result<Program> {
        self.scanner.position_at_start()?;
        let mut body = self.parse_directive_prologues()?;

        while self.scanner.lookahead != Token::Eof {
            body.push(self.parse_statement_list_item()?);
        }

        Ok(Program(body))
    }

    fn allow_in<F, T>(&mut self, allow_in: bool, parse_fn: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T> {
        let allow_in = std::mem::replace(&mut self.context.allow_in, allow_in);
        let result = parse_fn(self);
        std::mem::replace(&mut self.context.allow_in, allow_in);
        result
    }

    fn in_switch<F, T>(&mut self, in_switch: bool, parse_fn: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T> {
        let in_switch = std::mem::replace(&mut self.context.in_switch, in_switch);
        let result = parse_fn(self);
        std::mem::replace(&mut self.context.in_switch, in_switch);
        result
    }

    fn in_iteration<F, T>(&mut self, in_iteration: bool, parse_fn: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T> {
        let in_iteration = std::mem::replace(&mut self.context.in_iteration, in_iteration);
        let result = parse_fn(self);
        std::mem::replace(&mut self.context.in_iteration, in_iteration);
        result
    }

    fn matches(&self, tok: Token) -> bool {
        self.scanner.lookahead == tok
    }

    fn eat(&mut self, tok: Token) -> Result<bool> {
        if self.matches(tok) {
            self.scanner.next_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn finalize(&self, start: Position) -> Span {
        Span {
            start: start,
            end: self.scanner.last_pos
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        let token = self.scanner.lookahead.clone();
        match token {
            Token::Number(n) => {
                self.scanner.next_token()?;
                Ok(Expression::Literal(self.finalize(start), Literal::Number(n)))
            }
            Token::BoolTrue => {
                self.scanner.next_token()?;
                Ok(Expression::Literal(self.finalize(start), Literal::True))
            }
            Token::BoolFalse => {
                self.scanner.next_token()?;
                Ok(Expression::Literal(self.finalize(start), Literal::False))
            }
            Token::String(ref s) => {
                self.scanner.next_token()?;
                Ok(Expression::Literal(self.finalize(start), Literal::String(s.clone())))
            }
            Token::Ident(n) => {
                self.scanner.next_token()?;
                Ok(Expression::Identifier(self.finalize(start), n))
            }
            Token::OpenSquare => self.parse_array_initializer(),
            Token::OpenCurly => self.parse_object_initializer(),
            Token::OpenParen => self.parse_group_expression(),
            Token::FunctionKeyword => {
                let fun = self.parse_function()?;
                Ok(Expression::Function(self.finalize(start), fun))
            },
            Token::ThisKeyword => {
                self.scanner.next_token()?;
                Ok(Expression::This(self.finalize(start)))
            },
            Token::Null => {
                self.scanner.next_token()?;
                Ok(Expression::Literal(self.finalize(start), Literal::Null))
            },
            t => {
                Err(self.unexpected_token(t.clone()))
            }
        }
    }

    fn parse_group_expression(&mut self) -> Result<Expression> {
        self.expect(Token::OpenParen)?;
        let result = self.parse_assignment_expression()?;
        self.expect(Token::CloseParen)?;
        Ok(result)
    }

    fn parse_arguments(&mut self) -> Result<Vec<ArgumentListElement>> {
        self.expect(Token::OpenParen)?;
        let mut arguments = Vec::new();

        loop {
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            } else {
                let argument = ArgumentListElement::Expression(self.parse_assignment_expression()?);
                arguments.push(argument);

                if self.scanner.lookahead != Token::CloseParen {
                    self.expect(Token::Comma)?;
                }
            }
        }

        self.expect(Token::CloseParen)?;
        Ok(arguments)
    }

    fn parse_new_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::New)?;
        let base = self.allow_in(true, Parser::parse_lhs_expression)?;
        let args = if self.matches(Token::OpenParen) {
            self.parse_arguments()?
        } else {
            Vec::new()
        };
        Ok(Expression::New(self.finalize(start), Box::new(base), args))
    }

    fn expect_identifier_name(&mut self) -> Result<String> {
        match self.match_identifier_name() {
            Some(ident) => {
                self.scanner.next_token()?;
                Ok(ident)
            }
            None => Err(self.error(ErrorCause::UnexpectedToken(self.scanner.lookahead.clone())))
        }
    }

    fn match_identifier_name(&mut self) -> Option<String> {
        match self.scanner.lookahead.clone() {
            Token::Ident(name) => Some(name),
            Token::If => Some("if".to_string()),
            Token::Else => Some("else".to_string()),
            Token::Null => Some("null".to_string()),
            Token::BoolTrue => Some("true".to_string()),
            Token::BoolFalse => Some("false".to_string()),
            Token::In => Some("in".to_string()),
            _ => None
        }
    }

    fn parse_lhs_expression(&mut self) -> Result<Expression> {
        self.parse_lhs_expression_opt(false)
    }

    fn parse_lhs_expression_allow_call(&mut self) -> Result<Expression> {
        self.parse_lhs_expression_opt(true)
    }

    fn parse_lhs_expression_opt(&mut self, allow_call: bool) -> Result<Expression> {
        let start = self.scanner.lookahead_start;

        let mut result = if self.matches(Token::New) {
            self.parse_new_expression()?
        } else {
            self.parse_primary_expression()?
        };

        loop {
            match self.scanner.lookahead {
                Token::OpenParen => {
                    if allow_call {
                        let args = self.parse_arguments()?;
                        let span = self.finalize(start);
                        result = Expression::Call(span, Box::new(result), args);
                    } else {
                        break;
                    }
                },
                Token::OpenSquare => {
                    self.expect(Token::OpenSquare)?;
                    let expr = self.parse_expression()?;
                    self.expect(Token::CloseSquare)?;
                    let span = self.finalize(start);
                    result = Expression::ComputedMember(span, Box::new(result), Box::new(expr));
                },
                Token::Dot => {
                    self.scanner.next_token()?;
                    let identifier_name = self.expect_identifier_name()?;
                    result = Expression::StaticMember(self.finalize(start), Box::new(result), identifier_name)

                }
                _ => break,
            }
        }

        Ok(result)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression> {
        if let Some(prefix) = self.scanner.lookahead.as_unary_op() {
            let start = self.scanner.lookahead_start;
            self.scanner.next_token()?;
            let expr = self.parse_unary_expression()?;
            Ok(Expression::Unary(self.finalize(start), prefix, Box::new(expr)))
        } else {
            self.parse_update_expression()
        }
    }

    fn parse_update_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        if let Some(op) = self.scanner.lookahead.as_update_op() {
            self.scanner.next_token()?;
            let expr = self.parse_unary_expression()?;
            Ok(Expression::Update(self.finalize(start), op, Box::new(expr), true))
        } else {
            let expr = self.allow_in(true, Parser::parse_lhs_expression_allow_call)?;

            if self.scanner.at_newline() {
                return Ok(expr);
            };

            if let Some(op) = self.scanner.lookahead.as_update_op() {
                self.scanner.next_token()?;
                Ok(Expression::Update(self.finalize(start), op, Box::new(expr), false))
            } else {
                Ok(expr)
            }
        }
    }

    fn combine_binary(&self, operator: InfixOp, left: Expression, right: Expression, start: Position) -> Expression {
        let span = Span {
            start: start,
            end: self.scanner.last_pos
        };

        match operator {
            InfixOp::BinOp(op) => Expression::Binary(span, op, Box::new(left), Box::new(right)),
            InfixOp::LogOp(op) => Expression::Logical(span, op, Box::new(left), Box::new(right)),
        }
    }

    fn parse_binary_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;

        let mut expr = self.parse_unary_expression()?;

        if let Some(first_op) = self.scanner.lookahead.as_infix_op(self.context.allow_in) {
            self.scanner.next_token()?;
            let mut markers = vec![start, self.scanner.lookahead_start];
            let mut right = self.parse_unary_expression()?;
            let mut expressions = vec![expr, right];
            let mut operators = vec![first_op];

            while let Some(op) = self.scanner.lookahead.as_infix_op(self.context.allow_in) {
                while expressions.len() > 1 && operators.last().unwrap().precedence() >= op.precedence() {
                    right = expressions.pop().unwrap();
                    let operator = operators.pop().unwrap();
                    expr = expressions.pop().unwrap();
                    markers.pop();
                    let marker = markers.last().unwrap();
                    expressions.push(self.combine_binary(operator, expr, right, *marker))
                }
                self.scanner.next_token()?;
                operators.push(op);
                markers.push(self.scanner.lookahead_start);
                expressions.push(self.parse_unary_expression()?)
            }

            expr = expressions.pop().unwrap();
            markers.pop();

            while !expressions.is_empty() {
                let operator = operators.pop().unwrap();
                let left = expressions.pop().unwrap();
                expr = self.combine_binary(operator, left , expr, markers.pop().unwrap());
            }
        }

        Ok(expr)
    }

    fn parse_conditional_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        let expr = self.parse_binary_expression()?;

        if self.eat(Token::QuestionMark)? {
            let consequent = self.parse_assignment_expression()?;
            self.expect(Token::Colon)?;
            let alternate = self.parse_assignment_expression()?;
            let span = self.finalize(start);
            Ok(Expression::Conditional(span, Box::new(expr), Box::new(consequent), Box::new(alternate)))
        } else {
            Ok(expr)
        }
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        let left = self.parse_conditional_expression()?;
        match self.scanner.lookahead.as_assign_op() {
            Some(op) => {
                match left {
                    Expression::Identifier(_, _) => {
                        self.scanner.next_token()?;
                        let right = self.parse_assignment_expression()?;
                        let span = self.finalize(start);
                        Ok(Expression::Assignment(span, op, Box::new(left), Box::new(right)))
                    }
                    _ => Err(self.error(ErrorCause::InvalidLHSAssignment))
                }
            }
            None => Ok(left)
        }
    }

    fn parse_array_initializer(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::OpenSquare)?;
        let mut elements = Vec::new();

        loop {
            if let Token::CloseSquare = self.scanner.lookahead {
                break;
            }
            if let Token::Comma = self.scanner.lookahead {
                self.scanner.next_token()?;
                elements.push(None);
                continue;
            } else {
                elements.push(Some(self.parse_assignment_expression()?));
            }

            if self.scanner.lookahead != Token::CloseSquare {
                self.expect(Token::Comma)?;
            }
        }

        self.expect(Token::CloseSquare)?;
        Ok(Expression::Array(self.finalize(start), elements))
    }

    fn match_object_property_key(&mut self) -> Result<Option<PropKey>> {
        let start = self.scanner.lookahead_start;

        let token = self.scanner.lookahead.clone();
        match token {
            Token::String(ref s) => {
                self.scanner.next_token()?;
                Ok(Some(PropKey::String(self.finalize(start), s.to_string())))
            }
            Token::Number(n) => {
                self.scanner.next_token()?;
                Ok(Some(PropKey::Number(self.finalize(start), n)))
            },
            _ => {
                if let Some(name) = self.match_identifier_name() {
                    self.scanner.next_token()?;
                    Ok(Some(PropKey::Identifier(self.finalize(start), name)))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn parse_prop_init(&mut self, start: Position, key: PropKey) -> Result<Prop> {
        self.expect(Token::Colon)?;
        let value = self.parse_assignment_expression()?;
        Ok(Prop::Init(self.finalize(start), key, value))
    }

    fn parse_function_source_elements(&mut self) -> Result<Block> {
        let old_in_function_body = std::mem::replace(&mut self.context.in_function_body, true);
        let old_labels = std::mem::replace(&mut self.context.labels, HashSet::new());
        let old_in_switch = std::mem::replace(&mut self.context.in_switch, false);
        let old_in_iteration = std::mem::replace(&mut self.context.in_iteration, false);

        self.expect(Token::OpenCurly)?;
        let mut statements = self.parse_directive_prologues()?;

        while self.scanner.lookahead != Token::CloseCurly {
            statements.push(self.parse_statement_list_item()?);
        }

        self.expect(Token::CloseCurly)?;
        std::mem::replace(&mut self.context.in_function_body, old_in_function_body);
        std::mem::replace(&mut self.context.labels, old_labels);
        std::mem::replace(&mut self.context.in_switch, old_in_switch);
        std::mem::replace(&mut self.context.in_iteration, old_in_iteration);
        Ok(Block(statements))
    }

    fn parse_object_property(&mut self) -> Result<Prop> {
        let start = self.scanner.lookahead_start;
        let token = self.scanner.lookahead.clone();

        if token == Token::Ident("get".to_string()) {
            self.scanner.next_token()?;
            if let Some(key) = self.match_object_property_key()? {
                let parameters = self.parse_function_parameters()?;
                let block = self.parse_function_source_elements()?;
                let value = Function { id: None, body: block, parameters: parameters };
                Ok(Prop::Get(self.finalize(start), key, value))
            } else {
                let span = self.finalize(start);
                self.parse_prop_init(start, PropKey::Identifier(span, "get".to_string()))
            }
        } else if token == Token::Ident("set".to_string()) {
            self.scanner.next_token()?;
            if let Some(key) = self.match_object_property_key()? {
                let parameters = self.parse_function_parameters()?;
                let block = self.parse_function_source_elements()?;
                let value = Function { id: None, body: block, parameters: parameters };
                Ok(Prop::Set(self.finalize(start), key, value))
            } else {
                let span = self.finalize(start);
                self.parse_prop_init(start, PropKey::Identifier(span, "set".to_string()))
            }
        } else if let Some(key) = self.match_object_property_key()? {
            self.parse_prop_init(start, key)
        } else {
            Err(self.unexpected_token(token))
        }
    }

    fn parse_object_initializer(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::OpenCurly)?;

        let mut properties = Vec::new();

        while self.scanner.lookahead != Token::CloseCurly {
            properties.push(self.parse_object_property()?);

            if self.scanner.lookahead != Token::CloseCurly {
                self.expect(Token::Comma)?;
            }
        };

        self.expect(Token::CloseCurly)?;
        Ok(Expression::Object(self.finalize(start), properties))
    }

    fn parse_variable_declarator(&mut self) -> Result<VariableDeclarator> {
        match self.scanner.lookahead.clone() {
            Token::Ident(name) => {
                self.scanner.next_token()?;
                let init = match self.scanner.lookahead {
                    Token::Eq => {
                        self.scanner.next_token()?;
                        Some(self.parse_assignment_expression()?)
                    }
                    _ => None
                };
                Ok(VariableDeclarator { id: name, init: init })
            }
            t => {
                Err(self.unexpected_token(t))
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration> {
        let mut declarators = Vec::new();
        declarators.push(self.parse_variable_declarator()?);
        while self.eat(Token::Comma)? {
            declarators.push(self.parse_variable_declarator()?)
        }
        Ok(VariableDeclaration {
            kind: VariableDeclarationKind::Var,
            declarations: declarators
        })
    }

    fn parse_variable_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Var)?;
        let declaration = self.parse_variable_declaration()?;
        Ok(Statement::VariableDeclaration(self.consume_semicolon(start)?, declaration))
    }

    fn parse_function_parameter(&mut self) -> Result<Pattern> {
        if let Token::Ident(name) = self.scanner.lookahead.clone() {
            self.scanner.next_token()?;
            Ok(Pattern::Identifier(name))
        } else {
            Err(self.unexpected_token(self.scanner.lookahead.clone()))
        }
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Pattern>> {
        self.expect(Token::OpenParen)?;
        let mut parameters = Vec::new();

        loop {
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            }
            parameters.push(self.parse_function_parameter()?);
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            }
            self.expect(Token::Comma)?;
        }

        self.expect(Token::CloseParen)?;

        Ok(parameters)
    }

    fn parse_function(&mut self) -> Result<Function> {
        self.expect(Token::FunctionKeyword)?;

        let id = match self.scanner.lookahead.clone() {
            Token::Ident(name) => {
                self.scanner.next_token()?;
                Some(name)
            }
            Token::OpenParen => {
                None
            }
            t => {
                return Err(self.unexpected_token(t));
            }
        };

        let parameters = self.parse_function_parameters()?;
        let block = self.parse_function_source_elements()?;

        Ok(Function { id: id, body: block, parameters: parameters })
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        let expr = self.parse_assignment_expression()?;

        if self.matches(Token::Comma) {
            let mut expressions = vec![expr];
            while !self.matches(Token::Eof) {
                if self.eat(Token::Comma)? {
                    expressions.push(self.parse_assignment_expression()?);
                } else {
                    break;
                }
            }
            Ok(Expression::Sequence(self.finalize(start), expressions))
        } else {
            Ok(expr)
        }
    }

    fn consume_semicolon(&mut self, start: Position) -> Result<Span> {
        if self.eat(Token::Semi)? || self.scanner.at_newline() {
            Ok(self.finalize(start))
        } else if self.matches(Token::Eof) || self.matches(Token::CloseCurly) {
            Ok(Span {
                start: start,
                end: self.scanner.lookahead_start
            })
        } else {
            Err(self.unexpected_token(self.scanner.lookahead.clone()))
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        let expr = self.parse_expression()?;
        Ok(Statement::Expression(self.consume_semicolon(start)?, expr))
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        self.expect(Token::If)?;
        self.expect(Token::OpenParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        let then = self.parse_statement()?;
        let alternate = match self.scanner.lookahead {
            Token::Else => {
                self.scanner.next_token()?;
                Some(Box::new(self.parse_statement()?))
            },
            _ => None
        };

        Ok(Statement::If(test, Box::new(then), alternate))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Return)?;

        if !self.context.in_function_body {
            return Err(self.error(ErrorCause::IllegalReturn))
        };

        let mut argument = None;

        let has_argument = self.scanner.lookahead != Token::Semi
            && self.scanner.lookahead != Token::CloseCurly
            && self.scanner.lookahead != Token::Eof
            && !self.scanner.at_newline();


        if has_argument {
            argument = Some(self.parse_expression()?);
        }

        let span = self.consume_semicolon(start)?;
        Ok(Statement::Return(span, argument))
    }

    fn parse_throw_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::ThrowKeyword)?;
        if self.scanner.at_newline() {
            return Err(self.error(ErrorCause::NewLineAfterThrow))
        }
        let argument = self.parse_expression()?;
        Ok(Statement::Throw(self.consume_semicolon(start)?, argument))
    }

    fn parse_catch_clause(&mut self) -> Result<CatchClause> {
        self.expect(Token::CatchKeyword)?;
        self.expect(Token::OpenParen)?;
        let param = match self.scanner.lookahead.clone() {
            Token::Ident(s) => {
                self.scanner.next_token()?;
                s
            },
            t => return Err(self.unexpected_token(t))
        };
        self.expect(Token::CloseParen)?;
        let body = self.parse_block()?;
        Ok(CatchClause { param: param, body: body })
    }

    fn parse_try_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::TryKeyword)?;
        let block = self.parse_block()?;
        let handler = if self.matches(Token::CatchKeyword) {
            Some(self.parse_catch_clause()?)
        } else {
            None
        };
        let finalizer = if self.eat(Token::FinallyKeyword)? {
            Some(self.parse_block()?)
        } else {
            None
        };

        if handler.is_none() && finalizer.is_none() {
            return Err(self.error(ErrorCause::MissingCatchOrFinally))
        }

        Ok(Statement::Try(self.consume_semicolon(start)?, block, handler, finalizer))
    }

    fn parse_switch_case(&mut self) -> Result<SwitchCase> {
        let test = if self.eat(Token::DefaultKeyword)? {
            None
        } else {
            self.expect(Token::CaseKeyword)?;
            Some(self.parse_expression()?)
        };
        self.expect(Token::Colon)?;

        let mut consequent = Vec::new();
        loop {
            if self.matches(Token::CloseCurly)
                || self.matches(Token::DefaultKeyword)
                    || self.matches(Token::CaseKeyword) {
                        break;
                    };
            consequent.push(self.parse_statement_list_item()?);
        };

        Ok(SwitchCase { test: test, consequent: Block(consequent) })
    }

    fn parse_switch_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::SwitchKeyword)?;

        self.expect(Token::OpenParen)?;
        let discriminant = self.parse_expression()?;
        self.expect(Token::CloseParen)?;

        let mut cases = Vec::new();
        let mut default_found = false;
        self.expect(Token::OpenCurly)?;
        loop {
            if self.matches(Token::CloseCurly) {
                break;
            };

            let case = self.in_switch(true, Parser::parse_switch_case)?;
            if case.test.is_none() {
                if default_found {
                    return Err(self.error(ErrorCause::MultipleDefaultsInSwitch))
                }
                default_found = true;
            }

            cases.push(case);
        };
        self.expect(Token::CloseCurly)?;

        Ok(Statement::Switch(self.consume_semicolon(start)?, discriminant, cases))
    }

    fn parse_do_while_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::DoKeyword)?;
        let body = self.in_iteration(true, Parser::parse_statement)?;
        self.expect(Token::WhileKeyword)?;
        self.expect(Token::OpenParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        self.eat(Token::Semi)?;
        Ok(Statement::DoWhile(self.finalize(start), Box::new(body), test))
    }

    fn parse_while_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::WhileKeyword)?;
        self.expect(Token::OpenParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        let body = self.in_iteration(true, Parser::parse_statement)?;
        Ok(Statement::While(self.finalize(start), test, Box::new(body)))
    }

    fn parse_for_in_statement(&mut self, start: Position, left: ForInit) -> Result<Statement> {
        let right = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        let body = self.in_iteration(true, Parser::parse_statement)?;
        Ok(Statement::ForIn(self.finalize(start), Box::new(ForInStatement {
            left: left,
            right: right,
            body: body
        })))
    }

    fn parse_for_iter_statement(&mut self, start: Position, init: Option<ForInit>) -> Result<Statement> {
        let test = if self.matches(Token::Semi) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect(Token::Semi)?;

        let update = if self.matches(Token::CloseParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect(Token::CloseParen)?;

        let body = self.in_iteration(true, Parser::parse_statement)?;
        Ok(Statement::For(self.finalize(start), Box::new(ForStatement {
            init: init,
            test: test,
            update: update,
            body: body
        })))
    }

    fn parse_for_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::ForKeyword)?;
        self.expect(Token::OpenParen)?;
        if self.eat(Token::Semi)? {
            self.parse_for_iter_statement(start, None)
        } else if self.eat(Token::Var)? {
            let decl = self.allow_in(false, Parser::parse_variable_declaration)?;
            if decl.declarations.len() == 1 && self.eat(Token::In)? {
                self.parse_for_in_statement(start, ForInit::VarDecl(decl))

            } else {
                self.expect(Token::Semi)?;
                self.parse_for_iter_statement(start, Some(ForInit::VarDecl(decl)))
            }
        } else {
            let init = ForInit::Expression(self.allow_in(false, Parser::parse_expression)?);
            if self.eat(Token::In)? {
                self.parse_for_in_statement(start, init)
            } else {
                self.expect(Token::Semi)?;
                self.parse_for_iter_statement(start, Some(init))
            }
        }
    }

    fn parse_with_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::WithKeyword)?;
        self.expect(Token::OpenParen)?;
        let object = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        let body = self.parse_statement()?;
        Ok(Statement::With(self.finalize(start), object, Box::new(body)))
    }

    fn parse_labeled_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        let expr = self.parse_expression()?;
        let id_opt = self.as_id(&expr);
        if id_opt.is_some() && self.eat(Token::Colon)? {
            let id = id_opt.unwrap();
            if self.context.labels.contains(&id.1) {
                return Err(self.error(ErrorCause::DuplicateLabel(id.1.clone())))
            }
            self.context.labels.insert(id.1.clone());
            let body = self.parse_statement()?;
            self.context.labels.remove(&id.1);
            Ok(Statement::Labeled(self.finalize(start), id, Box::new(body)))
        } else {
            Ok(Statement::Expression(self.consume_semicolon(start)?, expr))
        }
    }

    fn parse_break_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::BreakKeyword)?;
        if self.match_ident() && !self.scanner.at_newline() {
            let id = self.parse_id()?;
            if !self.context.labels.contains(&id.1) {
                return Err(self.error(ErrorCause::UndefinedLabel(id.1.clone())))
            };
            Ok(Statement::Break(self.consume_semicolon(start)?, Some(id)))
        } else if self.context.in_switch || self.context.in_iteration {
            Ok(Statement::Break(self.consume_semicolon(start)?, None))
        } else {
            self.consume_semicolon(start)?;
            Err(self.error(ErrorCause::IllegalBreak))
        }
    }

    fn parse_continue_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::ContinueKeyword)?;
        if self.match_ident() && !self.scanner.at_newline() {
            let id = self.parse_id()?;
            if !self.context.labels.contains(&id.1) {
                return Err(self.error(ErrorCause::UndefinedLabel(id.1.clone())))
            };
            Ok(Statement::Continue(self.consume_semicolon(start)?, Some(id)))
        } else if self.context.in_iteration {
            Ok(Statement::Continue(self.consume_semicolon(start)?, None))
        } else {
            self.consume_semicolon(start)?;
            Err(self.error(ErrorCause::IllegalContinue))
        }
    }

    fn match_ident(&self) -> bool {
        match self.scanner.lookahead {
            Token::Ident(_) => true,
            _ => false
        }
    }

    fn parse_id(&mut self) -> Result<Id> {
        let start = self.scanner.lookahead_start;
        match self.scanner.next_token()? {
            Token::Ident(s) => {
                Ok(Id(self.finalize(start), s))
            },
            t => Err(self.error(ErrorCause::UnexpectedToken(t)))
        }
    }

    fn as_id(&self, expr: &Expression) -> Option<Id> {
        match *expr {
            Expression::Identifier(ref sp, ref id) => Some(Id(sp.clone(), id.clone())),
            _ => None
        }
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;

        match self.scanner.lookahead {
            Token::Var => self.parse_variable_statement(),
            Token::FunctionKeyword => {
                self.parse_function().map(Statement::FunctionDeclaration)
            },
            Token::If => self.parse_if_statement(),
            Token::OpenCurly => {
                let block = self.parse_block()?;
                Ok(Statement::Block(self.finalize(start), block))
            },
            Token::Semi => {
                self.scanner.next_token()?;
                Ok(Statement::Empty(self.finalize(start)))
            }
            Token::DebuggerKeyword => {
                self.scanner.next_token()?;
                Ok(Statement::Debugger(self.consume_semicolon(start)?))
            }
            Token::BreakKeyword => self.parse_break_statement(),
            Token::ContinueKeyword => self.parse_continue_statement(),
            Token::ThrowKeyword => self.parse_throw_statement(),
            Token::TryKeyword => self.parse_try_statement(),
            Token::Return => self.parse_return_statement(),
            Token::SwitchKeyword => self.parse_switch_statement(),
            Token::DoKeyword => self.parse_do_while_statement(),
            Token::WhileKeyword => self.parse_while_statement(),
            Token::ForKeyword => self.parse_for_statement(),
            Token::WithKeyword => self.parse_with_statement(),
            Token::Ident(_) => self.parse_labeled_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_statement_list_item(&mut self) -> Result<StatementListItem> {
        self.parse_statement().map(StatementListItem::Statement)
    }

    fn parse_block(&mut self) -> Result<Block> {
        self.expect(Token::OpenCurly)?;
        let mut statements = Vec::new();

        while self.scanner.lookahead != Token::CloseCurly {
            statements.push(self.parse_statement_list_item()?);
        }

        self.expect(Token::CloseCurly)?;
        Ok(Block(statements))
    }

    fn expect(&mut self, expected: Token) -> Result<Token> {
        let next = self.scanner.lookahead.clone();

        if next == expected {
            self.scanner.next_token()?;
            Ok(next)
        } else {
            Err(self.unexpected_token(next))
        }
    }

    fn unexpected_token(&self, token: Token) -> CompileError {
        CompileError {
            pos: self.scanner.lookahead_start.one_indexed(),
            cause: ErrorCause::UnexpectedToken(token)
        }
    }

    fn error(&self, cause: ErrorCause) -> CompileError {
        CompileError {
            pos: self.scanner.last_pos.one_indexed(),
            cause: cause
        }
    }
}
