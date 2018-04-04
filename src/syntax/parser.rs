use errors::{CompileError, ErrorCause, Result};
use syntax::ast::*;
use syntax::span::{Span, Position};
use syntax::token::Token;
use syntax::scanner::Scanner;
use syntax::ops::AsOperator;
use syntax::word::EsWord;
use interner::{self, Symbol};

use std::collections::HashSet;
use std;

struct ParseContext {
    allow_in: bool,
    allow_yield: bool,
    in_iteration: bool,
    in_switch: bool,
    in_function_body: bool,
    strict: bool,
    is_assignment_target: bool,
    is_binding_element: bool,
    first_cover_initialized_name_error: Option<(Token, Position)>,
    labels: HashSet<Symbol>
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    context: ParseContext,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser<'a> {
        Parser {
            scanner: Scanner::new(source),
            context: ParseContext {
                allow_in: true,
                allow_yield: false,
                in_iteration: false,
                in_switch: false,
                in_function_body: false,
                strict: false,
                is_assignment_target: false,
                is_binding_element: false,
                first_cover_initialized_name_error: None,
                labels: HashSet::new()
            }
        }
    }

    fn directive_opt(&mut self, expr: &Expression) -> Option<String> {
        if let Expression::Literal(_, Literal::String(ref lit)) = *expr {
            let string = interner::resolve(lit.raw);
            Some(string[1..string.len() -1].to_owned())
        } else {
            None
        }
    }

    fn parse_directive(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;

        let expr = self.parse_expression()?;
        match self.directive_opt(&expr) {
            Some(dir) => {
                if dir == "use strict" {
                    self.context.strict = true;
                }
                Ok(Statement::Directive(self.consume_semicolon(start)?, expr, dir))
            },
            None => Ok(Statement::Expression(self.consume_semicolon(start)?, expr))
        }
    }

    fn parse_directive_prologues(&mut self) -> Result<Vec<StatementListItem>> {
        let mut directives = Vec::new();

        while let Token::String(_, _) = self.scanner.lookahead {
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

    fn allow_yield<F, T>(&mut self, allow_yield: bool, parse_fn: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T> {
        let allow_yield = std::mem::replace(&mut self.context.allow_yield, allow_yield);
        let result = parse_fn(self);
        std::mem::replace(&mut self.context.allow_yield, allow_yield);
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

    fn isolate_cover_grammar<F, T>(&mut self, parse_fn: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T> {
        let is_assignment_target = std::mem::replace(&mut self.context.is_assignment_target, true);
        let is_binding_element = std::mem::replace(&mut self.context.is_binding_element, true);
        let first_cover_initialized_name_error = std::mem::replace(&mut self.context.first_cover_initialized_name_error, None);
        let result = parse_fn(self);

        match self.context.first_cover_initialized_name_error {
            Some((tok, pos)) => Err(CompileError::new(pos, ErrorCause::UnexpectedToken(tok))),
            None => {
                std::mem::replace(&mut self.context.is_assignment_target, is_assignment_target);
                std::mem::replace(&mut self.context.is_binding_element, is_binding_element);
                std::mem::replace(&mut self.context.first_cover_initialized_name_error, first_cover_initialized_name_error);
                result
            }
        }
    }

    fn inherit_cover_grammar<F, T>(&mut self, parse_fn: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T> {
        let is_assignment_target = std::mem::replace(&mut self.context.is_assignment_target, true);
        let is_binding_element = std::mem::replace(&mut self.context.is_binding_element, true);
        let first_cover_initialized_name_error = std::mem::replace(&mut self.context.first_cover_initialized_name_error, None);

        let result = parse_fn(self);

        if self.context.is_assignment_target && is_assignment_target {
            std::mem::replace(&mut self.context.is_assignment_target, true);
        }
        if self.context.is_binding_element && is_binding_element {
            std::mem::replace(&mut self.context.is_binding_element, true);
        }
        let new_cover_initialized_name_error = first_cover_initialized_name_error.or(self.context.first_cover_initialized_name_error);
        std::mem::replace(&mut self.context.first_cover_initialized_name_error, new_cover_initialized_name_error);
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
        match self.scanner.lookahead {
            Token::Number(n) => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let lit = NumberLiteral { span: self.finalize(start), value: n };
                Ok(Expression::Literal(self.finalize(start), Literal::Number(lit)))
            }
            Token::BoolTrue => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                Ok(Expression::Literal(self.finalize(start), Literal::True))
            }
            Token::BoolFalse => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                Ok(Expression::Literal(self.finalize(start), Literal::False))
            }
            Token::String(raw, value) => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let lit = StringLiteral {
                    span: self.finalize(start),
                    raw,
                    value
                };
                Ok(Expression::Literal(self.finalize(start), Literal::String(lit)))
            }
            Token::Ident(n) => {
                self.scanner.next_token()?;
                Ok(Expression::Identifier(Id(self.finalize(start), n)))
            }
            Token::OpenSquare => self.inherit_cover_grammar(Parser::parse_array_initializer),
            Token::OpenCurly => self.inherit_cover_grammar(Parser::parse_object_initializer),
            Token::OpenParen => {
                self.context.is_binding_element = false;
                self.inherit_cover_grammar(Parser::parse_group_expression)
            }
            Token::FunctionKeyword => {
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let fun = self.parse_function(false)?;
                Ok(Expression::Function(fun))
            },
            Token::ClassKeyword => {
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let class = self.parse_class(true).map(Box::new)?;
                Ok(Expression::Class(self.finalize(start), class))
            },
            Token::ThisKeyword => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                Ok(Expression::This(self.finalize(start)))
            },
            Token::Null => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                Ok(Expression::Literal(self.finalize(start), Literal::Null))
            },
            Token::Div | Token::DivEq => {
                let regex = self.scanner.regex_token()?;
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                Ok(Expression::Literal(self.finalize(start), regex))
            },
            Token::YieldKeyword => {
                if !self.context.strict && !self.context.allow_yield {
                    self.scanner.next_token()?;
                    Ok(Expression::Identifier(Id(self.finalize(start), interner::KEYWORD_YIELD)))
                } else {
                    Err(self.unexpected_token(Token::YieldKeyword))
                }
            },
            t => {
                Err(self.unexpected_token(t))
            }
        }
    }

    fn parse_group_expression(&mut self) -> Result<Expression> {
        let paren_start = self.scanner.lookahead_start;
        self.expect(Token::OpenParen)?;

        if self.scanner.lookahead == Token::CloseParen {
            self.expect(Token::CloseParen)?;

            if self.scanner.lookahead != Token::Arrow {
                self.expect(Token::Arrow)?;
            }
            return self.parse_arrow_function(paren_start, Vec::new())
        }

        if self.scanner.lookahead == Token::Ellipsis {
            let rest = self.parse_rest_element()?;
            self.expect_because(Token::CloseParen, ErrorCause::RestParamMustBeLast)?;

            if self.scanner.lookahead != Token::Arrow {
                self.expect(Token::Arrow)?;
            }

            return self.parse_arrow_function(paren_start, vec![rest])
        }

        let start = self.scanner.lookahead_start;
        self.context.is_binding_element = true;
        let mut result = self.inherit_cover_grammar(Parser::parse_assignment_expression)?;

        if self.scanner.lookahead == Token::Comma {
            self.context.is_assignment_target = false;
            let mut sequence = vec![result];

            while self.scanner.lookahead != Token::Eof {
                if self.scanner.lookahead != Token::Comma {
                    break;
                }
                self.scanner.next_token()?;

                if self.scanner.lookahead == Token::Ellipsis {
                    if !self.context.is_binding_element {
                        return Err(self.unexpected_token(self.scanner.lookahead))
                    }
                    let rest = self.parse_rest_element()?;
                    self.expect_because(Token::CloseParen, ErrorCause::RestParamMustBeLast)?;
                    let mut args = self.reinterpret_as_arguments(Expression::Sequence(self.finalize(start), sequence)).unwrap();
                    args.push(rest);
                    return self.parse_arrow_function(paren_start, args)
                }

                sequence.push(self.inherit_cover_grammar(Parser::parse_assignment_expression)?)
            }

            result = Expression::Sequence(self.finalize(start), sequence);
        }

        self.expect(Token::CloseParen)?;

        if self.scanner.lookahead == Token::Arrow {
            if !self.context.is_binding_element {
                return Err(self.unexpected_token(self.scanner.lookahead))
            } else {
                match self.reinterpret_as_arguments(result) {
                    Some(args) => {
                        return self.parse_arrow_function(paren_start, args)
                    }
                    None => return Err(self.unexpected_token(self.scanner.lookahead))
                }
            }
        }

        self.context.is_binding_element = false;

        Ok(result)
    }

    fn parse_argument_list_item(&mut self) -> Result<ArgumentListElement> {
        let start = self.scanner.lookahead_start;

        if self.eat(Token::Ellipsis)? {
            let expr = self.inherit_cover_grammar(Parser::parse_assignment_expression)?;
            Ok(ArgumentListElement::SpreadElement(self.finalize(start), expr))
        } else {
            let expr = self.inherit_cover_grammar(Parser::parse_assignment_expression)?;
            Ok(ArgumentListElement::Expression(expr))
        }
    }

    fn parse_arguments(&mut self) -> Result<Vec<ArgumentListElement>> {
        self.expect(Token::OpenParen)?;
        let mut arguments = Vec::new();

        loop {
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            } else {
                arguments.push(self.parse_argument_list_item()?);

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

    fn parse_identifier_name(&mut self) -> Result<Id> {
        let start = self.scanner.lookahead_start;
        match self.match_identifier_name() {
            Some(ident) => {
                self.scanner.next_token()?;
                Ok(Id(self.finalize(start), ident))
            }
            None => Err(self.unexpected_token(self.scanner.lookahead))
        }
    }

    fn match_identifier_name(&mut self) -> Option<Symbol> {
        match self.scanner.lookahead {
            Token::Ident(name) => Some(name),
            Token::If => Some(interner::KEYWORD_IF),
            Token::Else => Some(interner::KEYWORD_ELSE),
            Token::Null => Some(interner::KEYWORD_NULL),
            Token::BoolTrue => Some(interner::KEYWORD_TRUE),
            Token::BoolFalse => Some(interner::KEYWORD_FALSE),
            Token::In => Some(interner::KEYWORD_IN),
            Token::YieldKeyword => Some(interner::KEYWORD_YIELD),
            Token::TryKeyword => Some(interner::KEYWORD_TRY),
            Token::DefaultKeyword => Some(interner::KEYWORD_DEFAULT),
            _ => None
        }
    }

    fn parse_lhs_expression(&mut self) -> Result<Expression> {
        self.parse_lhs_expression_opt(false)
    }

    fn parse_lhs_expression_allow_call(&mut self) -> Result<Expression> {
        let allow_in = std::mem::replace(&mut self.context.allow_in, true);
        let result = self.parse_lhs_expression_opt(true);
        std::mem::replace(&mut self.context.allow_in, allow_in);
        result
    }

    fn parse_super(&mut self) -> Result<Callee> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::SuperKeyword)?;
        match self.scanner.lookahead {
            Token::OpenParen | Token::OpenSquare | Token::Dot => Ok(Callee::Super(self.finalize(start))),
            t => Err(self.unexpected_token(t))
        }
    }

    fn parse_lhs_expression_opt(&mut self, allow_call: bool) -> Result<Expression> {
        let start = self.scanner.lookahead_start;

        let mut result = if self.matches(Token::SuperKeyword) && self.context.in_function_body {
            self.parse_super()?
        } else if self.matches(Token::New) {
            Callee::Expression(self.inherit_cover_grammar(Parser::parse_new_expression)?)
        } else {
            Callee::Expression(self.inherit_cover_grammar(Parser::parse_primary_expression)?)
        };

        loop {
            match self.scanner.lookahead {
                Token::OpenParen if allow_call => {
                    self.context.is_binding_element = false;
                    self.context.is_assignment_target = false;
                    let args = self.parse_arguments()?;
                    let span = self.finalize(start);
                    result = Callee::Expression(Expression::Call(span, Box::new(result), args));
                },
                Token::OpenSquare => {
                    self.context.is_binding_element = false;
                    self.context.is_assignment_target = true;
                    self.expect(Token::OpenSquare)?;
                    let expr = self.isolate_cover_grammar(Parser::parse_expression)?;
                    self.expect(Token::CloseSquare)?;
                    let span = self.finalize(start);
                    result = Callee::Expression(Expression::Member(Box::new(Member {
                        span,
                        object: result,
                        property: expr,
                        computed: true
                    })));
                },
                Token::Dot => {
                    self.context.is_binding_element = false;
                    self.context.is_assignment_target = true;
                    self.scanner.next_token()?;
                    let id = self.parse_identifier_name()?;
                    let prop = Expression::Identifier(id);
                    let span = self.finalize(start);
                    result = Callee::Expression(Expression::Member(Box::new(Member {
                        span,
                        object: result,
                        property: prop,
                        computed: false
                    })));

                }
                _ => break,
            }
        }

        match result {
            Callee::Expression(e) => Ok(e),
            Callee::Super(ref sp) => Err(self.unexpected_token_at(sp.start, Token::SuperKeyword))
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Expression> {
        if let Some(prefix) = self.scanner.lookahead.as_unary_op() {
            let start = self.scanner.lookahead_start;
            self.scanner.next_token()?;
            let expr = self.inherit_cover_grammar(Parser::parse_unary_expression)?;
            if self.context.strict && prefix == UnOp::Delete {
                if let Expression::Identifier(_) = expr {
                    return Err(self.error(ErrorCause::UnqualifiedDelete))
                }
            }
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            Ok(Expression::Unary(self.finalize(start), prefix, Box::new(expr)))
        } else {
            self.parse_update_expression()
        }
    }

    fn check_reserved_expr_at(&self, expr: &Expression, pos: Option<Position>, cause: ErrorCause) -> Result<()> {
        if let Expression::Identifier(ref id) = *expr {
            self.check_reserved_at(id.1, pos.unwrap_or(id.0.start), cause)?;
        }
        Ok(())
    }

    fn check_reserved_pat_at(&self, pat: &Pattern<Id>, pos: Position, cause: ErrorCause) -> Result<()> {
        match *pat {
            Pattern::Simple(ref id) => self.check_reserved_at(id.1, pos, cause),
            Pattern::Assignment(_, ref left, ref right) => {
                self.check_reserved_pat_at(&*left, pos, cause.clone())?;
                self.check_reserved_expr_at(&*right, None, cause)
            }
            Pattern::Array(_, ref elements) => {
                for elem in elements {
                    if let Some(ref binding_pattern) = *elem {
                        self.check_reserved_pat_at(binding_pattern, pos, cause.clone())?;
                    }
                };
                Ok(())
            }
            Pattern::RestElement(_, ref arg) => self.check_reserved_pat_at(&*arg, pos, cause),
            Pattern::Object(_, ref props) => {
                for prop in props {
                    self.check_reserved_pat_at(&prop.value, pos, cause.clone())?;
                }
                Ok(())
            }
        }
    }

    fn check_reserved_at(&self, word: Symbol, pos: Position, cause: ErrorCause) -> Result<()> {
        if self.context.strict {
            if word.is_strict_mode_reserved_word() {
                return Err(CompileError::new(pos, ErrorCause::StrictReservedWord))
            }
            if word.is_restricted_word() {
                return Err(CompileError::new(pos, cause))
            }
        }
        Ok(())
    }

    fn check_assignment_allowed(&mut self) -> Result<()> {
        if !self.context.is_assignment_target {
            return Err(self.error(ErrorCause::InvalidLHSAssignment));
        }
        Ok(())
    }

    fn parse_update_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        if let Some(op) = self.scanner.lookahead.as_update_op() {
            self.scanner.next_token()?;
            let expr = self.inherit_cover_grammar(Parser::parse_unary_expression)?;
            self.check_reserved_expr_at(&expr, Some(self.scanner.last_pos), ErrorCause::RestrictedVarNameInPrefix)?;
            self.check_assignment_allowed()?;
            Ok(Expression::Update(self.finalize(start), op, Box::new(expr), true))
        } else {
            let expr = self.inherit_cover_grammar(Parser::parse_lhs_expression_allow_call)?;

            if self.scanner.at_newline() {
                return Ok(expr);
            };

            if let Some(op) = self.scanner.lookahead.as_update_op() {
                self.check_reserved_expr_at(&expr, Some(self.scanner.last_pos), ErrorCause::RestrictedVarNameInPostfix)?;
                self.check_assignment_allowed()?;
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
        let expr = self.inherit_cover_grammar(Parser::parse_binary_expression)?;

        if self.eat(Token::QuestionMark)? {
            let consequent = self.isolate_cover_grammar(Parser::parse_assignment_expression)?;
            self.expect(Token::Colon)?;
            let alternate = self.isolate_cover_grammar(Parser::parse_assignment_expression)?;
            let span = self.finalize(start);
            Ok(Expression::Conditional(span, Box::new(expr), Box::new(consequent), Box::new(alternate)))
        } else {
            Ok(expr)
        }
    }

    fn reinterpret_as_pattern(&self, expr: Expression) -> Option<Pattern<Id>> {
        match expr {
            Expression::Identifier(id) => Some(Pattern::Simple(id)),
            Expression::Assignment(sp, _, left, right) => {
                match *left {
                    Pattern::Simple(AssignTarget::Id(id)) => {
                        Some(Pattern::Assignment(sp, Box::new(Pattern::Simple(id)), *right))
                    }
                    _ => unimplemented!()
                }
            }
            Expression::Array(sp, exprs) => {
                let mut result = Vec::new();
                for elem in exprs {
                    let pat = match elem {
                        Some(ArgumentListElement::Expression(e)) => {
                            self.reinterpret_as_pattern(e)
                        }
                        Some(ArgumentListElement::SpreadElement(sp, e)) => {
                            self.reinterpret_as_pattern(e).map(|p| Pattern::RestElement(sp, Box::new(p)))
                        }
                        _ => None
                    };
                    result.push(pat)
                }

                Some(Pattern::Array(sp, result))
            }
            Expression::Object(sp, props) => {
                let mut result = Vec::new();
                for prop in props {
                    let prop_def = match prop {
                        Prop::Init(sp, key, val) => {
                            match self.reinterpret_as_pattern(val) {
                                Some(val_pattern) => {
                                    PropPattern { span: sp, key: key, value: val_pattern, shorthand: false }
                                }
                                None => return None
                            }
                        }
                        Prop::CoverInitializedName(sp, key, val) => {
                            match self.reinterpret_as_pattern(val) {
                                Some(val_pattern) => {
                                    PropPattern { span: sp, key: key, value: val_pattern, shorthand: true }
                                }
                                None => return None
                            }
                        }
                        Prop::Shorthand(sp, id) => {
                            let val_pattern = Pattern::Simple(id.clone());
                            let key = PropKey::Identifier(id);
                            PropPattern { span: sp, key: key, value: val_pattern, shorthand: true }
                        }
                        _ => return None
                    };
                    result.push(prop_def)
                }

                Some(Pattern::Object(sp, result))
            }
            _ => None
        }
    }

    fn reinterpret_as_assign_target(&self, expr: Expression) -> Option<Pattern<AssignTarget>> {
        match expr {
            Expression::Member(member) => Some(Pattern::Simple(AssignTarget::Member(*member))),
            Expression::Identifier(id) => Some(Pattern::Simple(AssignTarget::Id(id))),
            Expression::Assignment(sp, _op, left, right) => Some(Pattern::Assignment(sp, left, *right)),
            Expression::Array(sp, exprs) => {
                let mut result = Vec::new();
                for elem in exprs {
                    let pat = match elem {
                        Some(ArgumentListElement::Expression(e)) => {
                            self.reinterpret_as_assign_target(e)
                        }
                        Some(ArgumentListElement::SpreadElement(sp, e)) => {
                            self.reinterpret_as_assign_target(e).map(|p| Pattern::RestElement(sp, Box::new(p)))
                        }
                        _ => None
                    };
                    result.push(pat)
                }

                Some(Pattern::Array(sp, result))
            }
            Expression::Object(sp, props) => {
                let mut result = Vec::new();
                for prop in props {
                    let pat = match prop {
                        Prop::Init(sp, key, value) => {
                            match self.reinterpret_as_assign_target(value) {
                                Some(target) => {
                                    PropPattern {span: sp, key: key, value: target, shorthand: false}
                                }
                                None => return None
                            }
                        }
                        Prop::CoverInitializedName(sp, key, val) => {
                            match self.reinterpret_as_assign_target(val) {
                                Some(val_pattern) => {
                                    PropPattern { span: sp, key: key, value: val_pattern, shorthand: true }
                                }
                                None => return None
                            }
                        }
                        Prop::Shorthand(sp, id) => {
                            let val_pattern = Pattern::Simple(AssignTarget::Id(id.clone()));
                            let key = PropKey::Identifier(id);
                            PropPattern { span: sp, key: key, value: val_pattern, shorthand: true }
                        }
                        _ => return None
                    };
                    result.push(pat)
                }

                Some(Pattern::Object(sp, result))
            }
            _ => None
        }
    }

    fn reinterpret_as_arguments(&self, expr: Expression) -> Option<Vec<Pattern<Id>>> {
        let exprs = match expr {
            Expression::Sequence(_, sequence) => sequence,
            e => vec![e]
        };

        let mut res = Vec::new();
        for e in exprs {
            match self.reinterpret_as_pattern(e) {
                Some(Pattern::Assignment(sp, op, Expression::Yield(id_sp, arg, _))) => {
                    if arg.is_some() {
                        return None
                    };
                    res.push(Pattern::Assignment(sp, op, Expression::Identifier(Id(id_sp, interner::KEYWORD_YIELD))))
                }
                Some(p) => res.push(p),
                None => return None
            }
        }
        Some(res)
    }

    fn parse_arrow_function(&mut self, start: Position, params: Vec<Pattern<Id>>) -> Result<Expression> {
        let previous_allow_yield = std::mem::replace(&mut self.context.allow_yield, false);
        self.context.first_cover_initialized_name_error = None;

        if self.scanner.at_newline() {
            self.scanner.next_token()?;
            return Err(CompileError::new(self.scanner.lookahead_start, ErrorCause::UnexpectedToken(Token::Arrow)));
        }
        self.expect(Token::Arrow)?;
        self.context.is_assignment_target = false;

        let body = if self.scanner.lookahead == Token::OpenCurly {
            ArrowFunctionBody::Block(self.parse_function_source_elements()?)
        } else {
            ArrowFunctionBody::Expression(Box::new(self.isolate_cover_grammar(Parser::parse_assignment_expression)?))
        };

        self.validate_params(&params, Some(self.scanner.last_pos))?;
        self.context.allow_yield = previous_allow_yield;

        Ok(Expression::ArrowFunction(ArrowFunction {
            span: self.finalize(start),
            body: body,
            parameters: params
        }))
    }

    fn is_start_of_expression(&self) -> bool {
        match self.scanner.lookahead {
            Token::CloseCurly | Token::CloseParen | Token::CloseSquare => false,
            Token::Dot | Token::Ellipsis => false,
            Token::Semi | Token::Comma => false,
            Token::Lt | Token::Lte => false,
            Token::Gt | Token::Gte => false,
            Token::Eq | Token::EqEq | Token::EqEqEq => false,
            Token::PlusEq | Token::MinusEq => false,
            Token::Star | Token::TimesEq => false,
            Token::Mod | Token::ModEq => false,
            Token::LShift | Token::LShiftEq => false,
            Token::RShift | Token::RShiftEq => false,
            Token::URShift | Token::URShiftEq => false,
            Token::BitAnd | Token::BitAndEq => false,
            Token::BitOr | Token::BitOrEq => false,
            Token::BitXor | Token::BitXorEq => false,
            Token::LogicalAnd | Token::LogicalOr => false,
            Token::QuestionMark | Token::Colon => false,
            Token::Arrow => false,
            Token::TryKeyword | Token::CatchKeyword | Token::CaseKeyword => false,
            Token::ContinueKeyword | Token::BreakKeyword => false,
            Token::DoKeyword | Token::WhileKeyword => false,
            Token::If | Token::Else => false,
            Token::Var | Token::Const => false,
            Token::WithKeyword | Token::ThrowKeyword => false,
            Token::FinallyKeyword => false,
            Token::Return => false,
            Token::In => false,
            Token::ForKeyword => false,
            _ => true
        }
    }

    fn parse_yield_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        let mut delegate = false;
        self.expect(Token::YieldKeyword)?;

        let argument = if self.scanner.at_newline() {
            None
        } else if self.eat(Token::Star)? {
            delegate = true;
            Some(self.allow_yield(true, Parser::parse_assignment_expression)?)
        } else if self.is_start_of_expression() {
            Some(self.allow_yield(true, Parser::parse_assignment_expression)?)
        } else {
            None
        };

        Ok(Expression::Yield(self.finalize(start), Box::new(argument), delegate))
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        if self.context.allow_yield && self.matches(Token::YieldKeyword) {
            return self.parse_yield_expression()
        }

        let start = self.scanner.lookahead_start;
        let left = self.parse_conditional_expression()?;

        if self.scanner.lookahead == Token::Arrow {
            // Arrow function without parantheses e.g.
            // a => { something(a) }
            if let Expression::Identifier(_) = left {
                let params = vec![self.reinterpret_as_pattern(left).unwrap()];
                self.parse_arrow_function(start, params)
            } else {
                return Err(self.unexpected_token(Token::Arrow))
            }
        } else {
            match self.scanner.lookahead.as_assign_op() {
                Some(op) => {
                    self.check_reserved_expr_at(&left, Some(start), ErrorCause::RestrictedVarNameInAssignment)?;
                    self.check_assignment_allowed()?;
                    match self.reinterpret_as_assign_target(left) {
                        Some(target) => {
                            self.scanner.next_token()?;
                            self.context.first_cover_initialized_name_error = None;
                            let right = self.isolate_cover_grammar(Parser::parse_assignment_expression)?;
                            let span = self.finalize(start);
                            Ok(Expression::Assignment(span, op, Box::new(target), Box::new(right)))
                        },
                         _ =>  Err(self.error(ErrorCause::InvalidLHSAssignment))

                    }
                }
                None => Ok(left)
            }
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
            }

            let arg = self.parse_argument_list_item()?;
            elements.push(Some(arg));

            if self.scanner.lookahead != Token::CloseSquare {
                if let Some(ArgumentListElement::SpreadElement(_, _)) = *elements.last().unwrap() {
                    self.context.is_assignment_target = false;
                    self.context.is_binding_element = false;
                };
                self.expect(Token::Comma)?;
            }
        }

        self.expect(Token::CloseSquare)?;
        Ok(Expression::Array(self.finalize(start), elements))
    }

    fn match_object_property_key(&mut self) -> Result<Option<PropKey>> {
        let start = self.scanner.lookahead_start;

        match self.scanner.lookahead {
            Token::String(raw, value) => {
                self.scanner.next_token()?;
                let lit = StringLiteral {
                    span: self.finalize(start),
                    raw,
                    value
                };
                Ok(Some(PropKey::String(lit)))
            }
            Token::Number(value) => {
                self.scanner.next_token()?;
                let lit = NumberLiteral {span: self.finalize(start), value};
                Ok(Some(PropKey::Number(lit)))
            },
            Token::OpenSquare => {
                self.scanner.next_token()?;
                let expr = self.isolate_cover_grammar(Parser::parse_assignment_expression)?;
                self.expect(Token::CloseSquare)?;
                Ok(Some(PropKey::Computed(expr)))
            },
            _ => {
                let id_start = self.scanner.lookahead_start;
                if let Some(name) = self.match_identifier_name() {
                    self.scanner.next_token()?;
                    Ok(Some(PropKey::Identifier(Id(self.finalize(id_start), name))))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn parse_property_method(&mut self, start: Position, key: PropKey, generator: bool) -> Result<Prop> {
        let function_start = self.scanner.lookahead_start;
        let params = self.parse_function_parameters()?;
        self.context.is_assignment_target = false;
        self.context.is_binding_element = false;

        let previous_allow_yield = std::mem::replace(&mut self.context.allow_yield, generator);
        let previous_strict = self.context.strict;
        self.validate_params(&params, None)?;
        let block = self.parse_function_source_elements()?;
        let value = Function {
            id: None,
            span: self.finalize(function_start),
            body: block,
            parameters: params,
            generator };
        self.context.strict = previous_strict;
        self.context.allow_yield = previous_allow_yield;
        Ok(Prop::Method(self.finalize(start), key, value))
    }

    fn parse_prop_init(&mut self, start: Position, key: PropKey, generator: bool) -> Result<Prop> {
        if self.eat(Token::Colon)? {
            let value = self.parse_assignment_expression()?;
            Ok(Prop::Init(self.finalize(start), key, value))
        } else if self.matches(Token::OpenParen) {
            self.parse_property_method(start, key, generator)
        } else if let PropKey::Identifier(ref id) = key {
            if self.scanner.lookahead == Token::Eq {
                self.context.first_cover_initialized_name_error = Some((self.scanner.lookahead, self.scanner.lookahead_start));
                self.scanner.next_token()?;
                let key = PropKey::Identifier(id.clone());
                let init = self.isolate_cover_grammar(Parser::parse_assignment_expression)?;
                let pat = Pattern::Simple(AssignTarget::Id(id.clone()));
                let value = Expression::Assignment(self.finalize(start), AssignOp::Eq, Box::new(pat), Box::new(init));
                Ok(Prop::CoverInitializedName(self.finalize(start), key, value))
            } else {
                Ok(Prop::Shorthand(self.finalize(start), id.clone()))
            }
        } else {
            Err(self.unexpected_token(self.scanner.lookahead))
        }
    }

    fn parse_function_source_elements(&mut self) -> Result<Block> {
        let start = self.scanner.lookahead_start;
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
        Ok(Block(self.finalize(start), statements))
    }

    fn parse_object_property(&mut self) -> Result<Prop> {
        let start = self.scanner.lookahead_start;

        if self.scanner.lookahead == Token::Ident(interner::KEYWORD_GET) {
            self.scanner.next_token()?;
            if let Some(key) = self.match_object_property_key()? {
                let fun_start = self.scanner.lookahead_start;
                let previous_strict = self.context.strict;
                let parameters = self.parse_function_parameters()?;
                self.validate_params(&parameters, None)?;
                let block = self.parse_function_source_elements()?;
                let value = Function { id: None, span: self.finalize(fun_start), body: block, parameters: parameters, generator: false };
                self.context.strict = previous_strict;
                Ok(Prop::Get(self.finalize(start), key, value))
            } else {
                let span = self.finalize(start);
                let id = Id(span, interner::KEYWORD_GET);
                self.parse_prop_init(start, PropKey::Identifier(id), false)
            }
        } else if self.scanner.lookahead == Token::Ident(interner::KEYWORD_SET) {
            self.scanner.next_token()?;
            if let Some(key) = self.match_object_property_key()? {
                let fun_start = self.scanner.lookahead_start;
                let previous_strict = self.context.strict;
                let parameters = self.parse_function_parameters()?;
                self.validate_params(&parameters, None)?;
                let block = self.parse_function_source_elements()?;
                let value = Function { id: None, span: self.finalize(fun_start), body: block, parameters: parameters, generator: false };
                self.context.strict = previous_strict;
                Ok(Prop::Set(self.finalize(start), key, value))
            } else {
                let span = self.finalize(start);
                let id = Id(span, interner::KEYWORD_SET);
                self.parse_prop_init(start, PropKey::Identifier(id), false)
            }
        } else {
            let generator = self.eat(Token::Star)?;

            if let Some(key) = self.match_object_property_key()? {
                self.parse_prop_init(start, key, generator)
            } else {
                Err(self.unexpected_token(self.scanner.lookahead))
            }
        }
    }

    fn check_duplicate_proto(&self, has_proto: bool, prop: &Prop) -> Result<bool> {
        if let Prop::Init(_, ref key, _) = *prop {
            match *key {
                PropKey::Identifier(Id(ref span, s)) => {
                    if s == interner::KEYWORD_PROTO {
                        if has_proto {
                            return Err(CompileError::new(span.end, ErrorCause::DuplicateProto))
                        } else {
                            return Ok(true);
                        }
                    }
                }
                PropKey::String(ref lit) => {
                    if lit.value == interner::KEYWORD_PROTO {
                        if has_proto {
                            return Err(CompileError::new(lit.span.end, ErrorCause::DuplicateProto))
                        } else {
                            return Ok(true);
                        }
                    }
                }
                _ => return Ok(has_proto)
            }
        }
        Ok(has_proto)
    }

    fn parse_object_initializer(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::OpenCurly)?;

        let mut properties = Vec::new();
        let mut has_proto = false;

        while self.scanner.lookahead != Token::CloseCurly {
            let prop = self.parse_object_property()?;
            has_proto = self.check_duplicate_proto(has_proto, &prop)?;

            properties.push(prop);

            if self.scanner.lookahead != Token::CloseCurly {
                self.expect(Token::Comma)?;
            }
        };

        self.expect(Token::CloseCurly)?;
        Ok(Expression::Object(self.finalize(start), properties))
    }

    fn parse_rest_element(&mut self) -> Result<Pattern<Id>> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Ellipsis)?;
        let arg = self.parse_pattern(false, VariableDeclarationKind::Var)?;
        Ok(Pattern::RestElement(self.finalize(start), Box::new(arg)))
    }

    fn parse_array_pattern(&mut self, kind: VariableDeclarationKind) -> Result<Pattern<Id>> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::OpenSquare)?;
        let mut elements = Vec::new();

        while self.scanner.lookahead != Token::CloseSquare {
            if self.eat(Token::Comma)? {
                elements.push(None);
            } else {
                if self.scanner.lookahead == Token::Ellipsis {
                    elements.push(Some(self.parse_rest_element()?));
                    break;
                } else {
                    elements.push(Some(self.parse_pattern(true, kind)?));
                };

                if self.scanner.lookahead != Token::CloseSquare {
                    self.expect(Token::Comma)?;
                }
            }
        }

        self.expect(Token::CloseSquare)?;
        Ok(Pattern::Array(self.finalize(start), elements))
    }

    fn parse_prop_pattern(&mut self, kind: VariableDeclarationKind) -> Result<PropPattern<Id>> {
        let start = self.scanner.lookahead_start;
        if let Token::Ident(_) = self.scanner.lookahead {
            let id = self.parse_id()?;
            if self.eat(Token::Eq)? {
                let expr = self.parse_assignment_expression()?;
                let pat = Pattern::Simple(id.clone());
                Ok(PropPattern {
                    span: self.finalize(start),
                    key: PropKey::Identifier(id),
                    value: Pattern::Assignment(self.finalize(start), Box::new(pat), expr),
                    shorthand: true
                })
            } else if self.scanner.lookahead != Token::Colon {
                Ok(PropPattern {
                    span: self.finalize(start),
                    key: PropKey::Identifier(id.clone()),
                    value: Pattern::Simple(id.clone()),
                    shorthand: true
                })
            } else {
                let key = PropKey::Identifier(id);
                self.expect(Token::Colon)?;
                let val = self.parse_pattern(true, kind)?;
                Ok(PropPattern {
                    span: self.finalize(start),
                    key: key,
                    value: val,
                    shorthand: false
                })
            }
        } else {
            let key = self.match_object_property_key()?.unwrap();
            self.expect(Token::Colon)?;
            let val = self.parse_pattern(true, kind)?;
            Ok(PropPattern {
                span: self.finalize(start),
                key: key,
                value: val,
                shorthand: false
             })
        }
    }

    fn parse_object_pattern(&mut self, kind: VariableDeclarationKind) -> Result<Pattern<Id>> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::OpenCurly)?;
        let mut props = Vec::new();

        while self.scanner.lookahead != Token::CloseCurly {
            props.push(self.parse_prop_pattern(kind)?);

            if self.scanner.lookahead != Token::CloseCurly {
                self.expect(Token::Comma)?;
            }
        }

        self.expect(Token::CloseCurly)?;
        Ok(Pattern::Object(self.finalize(start), props))
    }

    fn parse_pattern(&mut self, allow_default: bool, kind: VariableDeclarationKind) -> Result<Pattern<Id>> {
        match self.scanner.lookahead {
            Token::OpenSquare => self.parse_array_pattern(kind),
            Token::OpenCurly => self.parse_object_pattern(kind),
            _ => {
                if self.match_contextual_keyword(interner::RESERVED_LET) {
                    if kind == VariableDeclarationKind::Let || kind == VariableDeclarationKind::Const {
                        return Err(CompileError::new(self.scanner.lookahead_start, ErrorCause::LetInLexicalBinding))
                    }
                }
                let start = self.scanner.lookahead_start;
                let id = self.parse_id()?;
                let left = Pattern::Simple(id);
                if allow_default {
                    self.allow_yield(false, |this| this.parse_pattern_default(start, left))
                } else {
                    Ok(left)
                }
            }
        }
    }

    fn parse_pattern_default(&mut self, start: Position, left: Pattern<Id>) -> Result<Pattern<Id>> {
        match self.scanner.lookahead {
            Token::Eq => {
                self.scanner.next_token()?;
                let right = self.isolate_cover_grammar(Parser::parse_assignment_expression)?;
                Ok(Pattern::Assignment(self.finalize(start), Box::new(left), right))
            }
            _ => Ok(left)
        }
    }

    fn parse_variable_declarator(&mut self, kind: VariableDeclarationKind, in_for: bool) -> Result<VariableDeclarator> {
        let start = self.scanner.lookahead_start;
        let id = self.parse_pattern(false, kind)?;

        let init = match self.scanner.lookahead {
            Token::Eq => {
                self.check_reserved_pat_at(&id, self.scanner.last_pos, ErrorCause::RestrictedVarName)?;
                self.scanner.next_token()?;
                Some(self.isolate_cover_grammar(Parser::parse_assignment_expression)?)
            }
            _ if kind == VariableDeclarationKind::Const => {
                if !(self.matches(Token::In) || self.match_contextual_keyword(interner::RESERVED_OF)) {
                    return Err(self.error(ErrorCause::MissingInitializerInConst))
                } else {
                    None
                }
            }
            _ if !id.is_simple() && !in_for => {
                self.scanner.next_token()?;
                return Err(self.unexpected_token(self.scanner.lookahead))
            }
            _ => {
                self.check_reserved_pat_at(&id, start, ErrorCause::RestrictedVarName)?;
                None
            }
        };

        Ok(VariableDeclarator {span: self.finalize(start), id, init})
    }

    fn parse_variable_declaration(&mut self, start: Position, kind: VariableDeclarationKind, in_for: bool) -> Result<VariableDeclaration> {
        let mut declarators = Vec::new();
        declarators.push(self.parse_variable_declarator(kind, in_for)?);
        while self.eat(Token::Comma)? {
            declarators.push(self.parse_variable_declarator(kind, in_for)?)
        }
        Ok(VariableDeclaration {
            span: self.finalize(start),
            kind: kind,
            declarations: declarators
        })
    }

    fn parse_variable_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Var)?;
        let mut declaration = self.parse_variable_declaration(start, VariableDeclarationKind::Var, false)?;
        let span = self.consume_semicolon(start)?;
        declaration.span = span;
        Ok(Statement::VariableDeclaration(declaration))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Pattern<Id>>> {
        self.expect(Token::OpenParen)?;
        let mut parameters = Vec::new();

        loop {
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            }
            if self.scanner.lookahead == Token::Ellipsis {
                parameters.push(self.parse_rest_element()?);
                if self.matches(Token::Eq) {
                    return Err(CompileError::new(self.scanner.last_pos, ErrorCause::UnexpectedToken(Token::Eq)))
                }
                if !self.matches(Token::CloseParen) {
                    return Err(self.error(ErrorCause::RestParamMustBeLast))
                }
            } else {
                parameters.push(self.parse_pattern(true, VariableDeclarationKind::Var)?);
            }
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            }
            self.expect(Token::Comma)?;
        }

        self.expect(Token::CloseParen)?;

        Ok(parameters)
    }

    fn validate_pattern(&self, override_pos: &Option<Position>, param_names: &mut HashSet<Symbol>, pat: &Pattern<Id>) -> Result<()> {
        match *pat {
            Pattern::Simple(ref id) => {
                self.check_reserved_at(id.1, override_pos.unwrap_or(id.0.start), ErrorCause::StrictParamName)?;
                if self.context.strict && param_names.contains(&id.1) {
                    return Err(CompileError::new(override_pos.unwrap_or(id.0.start), ErrorCause::StrictDupeParam))
                }
                param_names.insert(id.1);
                Ok(())
            },
            Pattern::Assignment(_, ref left, ref right) => {
                self.check_reserved_expr_at(right, *override_pos, ErrorCause::StrictParamName)?;
                self.validate_pattern(override_pos, param_names, &*left)
            },
            Pattern::RestElement(_, ref arg) => {
                self.validate_pattern(override_pos, param_names, &*arg)
            }
            Pattern::Array(_, ref elements) => {
                for elem in elements {
                    if let Some(ref binding_pattern) = *elem {
                        self.validate_pattern(override_pos, param_names, binding_pattern)?;
                    }
                };
                Ok(())
            }
            Pattern::Object(_, ref props) => {
                for prop in props {
                    self.validate_pattern(override_pos, param_names, &prop.value)?;
                };
                Ok(())
            }
        }
    }

    fn validate_params(&self, params: &[Pattern<Id>], override_pos: Option<Position>) -> Result<()> {
        let mut param_names = HashSet::new();
        for param in params {
            self.validate_pattern(&override_pos, &mut param_names, param)?;
        };
        Ok(())
    }

    fn parse_function(&mut self, is_expr: bool) -> Result<Function> {
        let start = self.scanner.lookahead_start;
        let mut generator = false;
        self.expect(Token::FunctionKeyword)?;

        if self.eat(Token::Star)? {
            generator = true;
        }

        let id_loc = self.scanner.lookahead_start;
        let id = match self.scanner.lookahead {
            Token::OpenParen if !is_expr => None,
            Token::YieldKeyword if !self.context.strict && !generator && !is_expr => Some(self.parse_identifier_name()?),
            Token::YieldKeyword if !is_expr => return Err(self.unexpected_token(Token::YieldKeyword)),
            _ => Some(self.parse_id()?)
        };

        let previous_strict = self.context.strict;
        let previous_allow_yield = std::mem::replace(&mut self.context.allow_yield, generator);

        let parameters = self.parse_function_parameters()?;
        let block = self.parse_function_source_elements()?;

        self.validate_params(&parameters, None)?;

        if let Some(ref name) = id {
            self.check_reserved_at(name.1, id_loc, ErrorCause::RestrictedVarNameInFunction)?;
        }

        self.context.strict = previous_strict;
        self.context.allow_yield = previous_allow_yield;

        Ok(Function { id: id, span: self.finalize(start), body: block, parameters, generator })
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        let expr = self.isolate_cover_grammar(Parser::parse_assignment_expression)?;

        if self.matches(Token::Comma) {
            let mut expressions = vec![expr];
            while !self.matches(Token::Eof) {
                if self.eat(Token::Comma)? {
                    expressions.push(self.isolate_cover_grammar(Parser::parse_assignment_expression)?);
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
            self.scanner.last_pos = self.scanner.lookahead_start;
            Ok(Span {
                start: start,
                end: self.scanner.lookahead_start
            })
        } else {
            Err(self.unexpected_token(self.scanner.lookahead))
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        let expr = self.parse_expression()?;
        Ok(Statement::Expression(self.consume_semicolon(start)?, expr))
    }

    fn parse_if_clause(&mut self) -> Result<Statement> {
        if self.context.strict && self.scanner.lookahead == Token::FunctionKeyword {
            return Err(self.error(ErrorCause::StrictFunction))
        }
        self.parse_statement(false)
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::If)?;
        self.expect(Token::OpenParen)?;
        let test = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        let then = self.parse_if_clause()?;
        let alternate = match self.scanner.lookahead {
            Token::Else => {
                self.scanner.next_token()?;
                Some(Box::new(self.parse_if_clause()?))
            },
            _ => None
        };

        Ok(Statement::If(self.finalize(start), test, Box::new(then), alternate))
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
        let start = self.scanner.lookahead_start;
        self.expect(Token::CatchKeyword)?;
        self.expect(Token::OpenParen)?;
        let param = self.parse_pattern(false, VariableDeclarationKind::Var)?;
        self.check_reserved_pat_at(&param, self.scanner.last_pos, ErrorCause::RestrictedVarNameInCatch)?;
        self.expect(Token::CloseParen)?;
        let body = self.parse_block()?;
        Ok(CatchClause { span: self.finalize(start), param: param, body: body })
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
        let start = self.scanner.lookahead_start;
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
            consequent.push(self.parse_statement(true)?);
        };

        Ok(SwitchCase { span: self.finalize(start), test: test, consequent: consequent })
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
        let body = self.in_iteration(true, |c| Parser::parse_statement(c, true))?;
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
        let body = self.in_iteration(true, |c| Parser::parse_statement(c, true))?;
        Ok(Statement::While(self.finalize(start), test, Box::new(body)))
    }

    fn parse_for_op_statement(&mut self, left: ForOpInit) -> Result<ForOpStatement> {
        let right = self.parse_expression()?;
        self.check_reserved_expr_at(&right, None, ErrorCause::StrictReservedWord)?;
        self.expect(Token::CloseParen)?;
        let body = self.in_iteration(true, |c| Parser::parse_statement(c, true))?;
        Ok(ForOpStatement {left, right, body})
    }

    fn parse_for_in_statement(&mut self, start: Position, left: ForOpInit) -> Result<Statement> {
        let stmt = self.parse_for_op_statement(left)?;
        Ok(Statement::ForIn(self.finalize(start), Box::new(stmt)))
    }

    fn parse_for_of_statement(&mut self, start: Position, left: ForOpInit) -> Result<Statement> {
        let stmt = self.parse_for_op_statement(left)?;
        Ok(Statement::ForOf(self.finalize(start), Box::new(stmt)))
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

        let body = self.in_iteration(true, |c| Parser::parse_statement(c, true))?;
        Ok(Statement::For(self.finalize(start), Box::new(ForStatement {
            init: init,
            test: test,
            update: update,
            body: body
        })))
    }

    fn match_lexical_kind(&self) -> Option<VariableDeclarationKind> {
        match self.scanner.lookahead {
            Token::Const => Some(VariableDeclarationKind::Const),
            Token::Ident(n) if n == interner::RESERVED_LET => Some(VariableDeclarationKind::Let),
            _ => None
        }
    }

    fn lexical_kind_as_ident(&self, kind: &VariableDeclarationKind) -> Symbol {
        match *kind {
            VariableDeclarationKind::Let => interner::RESERVED_LET,
            VariableDeclarationKind::Const => interner::RESERVED_CONST,
            _ => unreachable!()
        }
    }

    fn match_contextual_keyword(&self, expected: Symbol) -> bool {
        match self.scanner.lookahead {
            Token::Ident(n) if n == expected => true,
            _ => false
        }
    }

    fn parse_for_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::ForKeyword)?;
        self.expect(Token::OpenParen)?;
        let init_start = self.scanner.lookahead_start;
        if self.eat(Token::Semi)? {
            self.parse_for_iter_statement(start, None)
        } else if self.eat(Token::Var)? {
            let decl = self.allow_in(false, |context| Parser::parse_variable_declaration(context, init_start, VariableDeclarationKind::Var, true))?;
            if decl.declarations.len() == 1 && self.eat(Token::In)? {
                self.parse_for_in_statement(start, ForOpInit::VarDecl(decl))
            } else if decl.declarations.len() == 1 && decl.declarations[0].init.is_none() && self.match_contextual_keyword(interner::RESERVED_OF) {
                self.scanner.next_token()?;
                self.parse_for_of_statement(start, ForOpInit::VarDecl(decl))
            } else {
                self.expect(Token::Semi)?;
                self.parse_for_iter_statement(start, Some(ForInit::VarDecl(decl)))
            }
        } else if let Some(kind) = self.match_lexical_kind() {
            self.scanner.next_token()?;
            if !self.context.strict && self.scanner.lookahead == Token::In {
                let init = ForOpInit::Pattern(Pattern::Simple(AssignTarget::Id(Id(self.finalize(init_start), self.lexical_kind_as_ident(&kind)))));
                self.scanner.next_token()?;
                return self.parse_for_in_statement(start, init)
            }
            let mut decl = self.allow_in(false, |context| Parser::parse_variable_declaration(context, init_start, kind, true))?;
            if decl.declarations.len() == 1 && decl.declarations[0].init.is_none() && self.eat(Token::In)? {
                self.parse_for_in_statement(start, ForOpInit::VarDecl(decl))
            } else if decl.declarations.len() == 1 && decl.declarations[0].init.is_none() && self.match_contextual_keyword(interner::RESERVED_OF) {
                self.scanner.next_token()?;
                self.parse_for_of_statement(start, ForOpInit::VarDecl(decl))
            } else {
                self.expect(Token::Semi)?;
                decl.span = self.finalize(init_start);
                self.parse_for_iter_statement(start, Some(ForInit::VarDecl(decl)))
            }
        } else {
            let previous_allow_in = std::mem::replace(&mut self.context.allow_in, false);
            let mut init_expr = self.inherit_cover_grammar(Parser::parse_assignment_expression)?;
            self.context.allow_in = previous_allow_in;
            if self.matches(Token::In) {
                if !self.context.is_assignment_target {
                    return Err(self.error(ErrorCause::InvalidLHSForIn))
                };
                if let Expression::Assignment(_, _, _, _) = init_expr {
                    return Err(self.error(ErrorCause::InvalidLHSForIn))
                };

                self.scanner.next_token()?;
                let pattern = self.reinterpret_as_assign_target(init_expr).unwrap();
                self.parse_for_in_statement(start, ForOpInit::Pattern(pattern))
            } else if self.match_contextual_keyword(interner::RESERVED_OF) {
                if !self.context.is_assignment_target {
                    return Err(self.error(ErrorCause::InvalidLHSForLoop))
                };
                if let Expression::Assignment(_, _, _, _) = init_expr {
                    return Err(self.error(ErrorCause::InvalidLHSForLoop))
                };
                self.scanner.next_token()?;
                let pattern = self.reinterpret_as_assign_target(init_expr).unwrap();
                self.parse_for_of_statement(start, ForOpInit::Pattern(pattern))
            } else {
                if self.scanner.lookahead == Token::Comma {
                    let mut seq = vec![init_expr];
                    while self.scanner.lookahead == Token::Comma {
                        self.scanner.next_token()?;
                        seq.push(self.isolate_cover_grammar(Parser::parse_assignment_expression)?)
                    };
                    init_expr = Expression::Sequence(self.finalize(init_start), seq);
                };

                self.expect(Token::Semi)?;
                self.parse_for_iter_statement(start, Some(ForInit::Expression(init_expr)))
            }
        }
    }

    fn parse_with_statement(&mut self) -> Result<Statement> {
        if self.context.strict {
            return Err(self.error(ErrorCause::StrictModeWith))
        }
        let start = self.scanner.lookahead_start;
        self.expect(Token::WithKeyword)?;
        self.expect(Token::OpenParen)?;
        let object = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        let body = self.parse_statement(true)?;
        Ok(Statement::With(self.finalize(start), object, Box::new(body)))
    }

    fn parse_labeled_statement(&mut self, start: Position, id: Id) -> Result<Statement> {
        self.expect(Token::Colon)?;

        if self.context.labels.contains(&id.1) {
            return Err(self.error(ErrorCause::DuplicateLabel(interner::resolve(id.1).to_owned())))
        }
        self.context.labels.insert(id.1);
        let body = self.parse_statement(true)?;
        self.context.labels.remove(&id.1);
        Ok(Statement::Labeled(self.consume_semicolon(start)?, id, Box::new(body)))
    }

    fn parse_break_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::BreakKeyword)?;
        if self.match_ident() && !self.scanner.at_newline() {
            let id = self.parse_id()?;
            if !self.context.labels.contains(&id.1) {
                return Err(self.error(ErrorCause::UndefinedLabel(interner::resolve(id.1).to_owned())))
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
                return Err(self.error(ErrorCause::UndefinedLabel(interner::resolve(id.1).to_owned())))
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
                if self.context.strict && s.is_strict_mode_reserved_word() {
                    Err(CompileError::new(start, ErrorCause::StrictReservedWord))
                } else {
                    Ok(Id(self.finalize(start), s))
                }
            },
            t@Token::YieldKeyword => {
                if self.context.strict {
                    Err(CompileError::new(start, ErrorCause::StrictReservedWord))
                } else if self.context.allow_yield {
                    Err(self.unexpected_token_at(start, t))
                } else {
                    Ok(Id(self.finalize(start), interner::KEYWORD_YIELD))
                }
            },
            t => Err(self.unexpected_token_at(start, t))
        }
    }

    fn parse_const_declaration(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Const)?;
        let mut declaration = self.parse_variable_declaration(start, VariableDeclarationKind::Const, false)?;
        let span = self.consume_semicolon(start)?;
        declaration.span = span;
        Ok(Statement::VariableDeclaration(declaration))
    }

    fn parse_let_declaration(&mut self, start: Position) -> Result<Statement> {
        let mut declaration = self.parse_variable_declaration(start, VariableDeclarationKind::Let, false)?;
        let span = self.consume_semicolon(start)?;
        declaration.span = span;
        Ok(Statement::VariableDeclaration(declaration))
    }

    fn parse_method_definition(&mut self) -> Result<MethodDefinition> {
        let start = self.scanner.lookahead_start;
        let mut is_static = false;
        let mut generator = false;
        let mut kind = MethodDefinitionKind::Method;
        let mut id_start = self.scanner.lookahead_start;

        if self.scanner.lookahead == Token::Ident(interner::RESERVED_STATIC) {
            self.scanner.next_token()?;
            id_start = self.scanner.lookahead_start;
            is_static = true;
            if self.eat(Token::Star)? {
                generator = true;
            }
        }

        if self.scanner.lookahead == Token::Ident(interner::KEYWORD_GET) {
            self.scanner.next_token()?;
            kind = MethodDefinitionKind::Get;
        } else if self.scanner.lookahead == Token::Ident(interner::KEYWORD_SET) {
            self.scanner.next_token()?;
            kind = MethodDefinitionKind::Set;
        };

        let key = match self.match_object_property_key()? {
            Some(k) => k,
            None if is_static => {
                is_static = false;
                PropKey::Identifier(Id(self.finalize(start), interner::RESERVED_STATIC))
            }
            _ => {
                return Err(self.unexpected_token(self.scanner.lookahead))
            }
        };

        match key {
            PropKey::Identifier(Id(_, sym)) | PropKey::String(StringLiteral{value: sym, ..}) => {
                if !is_static && sym == interner::RESERVED_CONSTRUCTOR {
                    if kind != MethodDefinitionKind::Method {
                        return Err(CompileError::new(id_start, ErrorCause::ConstructorSpecialMethod));
                    } else {
                        kind = MethodDefinitionKind::Constructor;
                    }
                };
                if is_static && sym == interner::RESERVED_PROTOTYPE {
                    return Err(CompileError::new(id_start, ErrorCause::StaticPrototype));
                };
            }
            _ => {}
        };

        let fun_start = self.scanner.lookahead_start;
        let parameters = self.parse_function_parameters()?;
        self.validate_params(&parameters, None)?;
        let body = self.parse_function_source_elements()?;
        let function = Function { id: None, span: self.finalize(fun_start), parameters, body, generator };

        Ok(MethodDefinition {
            loc: self.finalize(start),
            key: key,
            value: function,
            is_static: is_static,
            kind: kind
        })
    }

    fn parse_class_body(&mut self) -> Result<ClassBody> {
        let start = self.scanner.lookahead_start;
        let mut elems = Vec::new();
        self.expect(Token::OpenCurly)?;

        while self.scanner.lookahead != Token::CloseCurly {
            if self.scanner.lookahead == Token::Semi {
                self.scanner.next_token()?;
            } else {
                elems.push(self.parse_method_definition()?);
            }
        }

        self.expect(Token::CloseCurly)?;
        Ok(ClassBody(self.finalize(start), elems))
    }

    fn parse_class(&mut self, allow_anonymous: bool) -> Result<ClassDecl> {
        let previous_strict = self.context.strict;
        self.context.strict = true;

        self.expect(Token::ClassKeyword)?;
        let id = if allow_anonymous && !self.match_ident() {
            None
        } else {
            Some(self.parse_id()?)
        };

        let super_class = if self.eat(Token::ExtendsKeyword)? {
            Some(self.parse_lhs_expression()?)
        } else {
            None
        };
        let body = self.parse_class_body()?;

        self.context.strict = previous_strict;

        Ok(ClassDecl { id, super_class, body })
    }

    fn parse_module_specifier(&mut self) -> Result<StringLiteral> {
        let start = self.scanner.lookahead_start;

        match self.scanner.lookahead {
            Token::String(raw, value) => {
                self.scanner.next_token()?;
                Ok(StringLiteral { span: self.finalize(start), value, raw })
            },
            t => Err(self.unexpected_token(t))
        }
    }

    fn parse_export_specifier(&mut self) -> Result<ExportSpecifier> {
        let local = self.parse_identifier_name()?;

        let exported = if self.match_contextual_keyword(interner::RESERVED_AS) {
            self.scanner.next_token()?;
            self.parse_identifier_name()?
        } else {
            local.clone()
        };

        Ok(ExportSpecifier {
            local,
            exported
        })
    }

    fn parser_import_specification(&mut self) -> Result<ImportSpecification> {
        let imported = self.parse_identifier_name()?;
        let local = if self.match_contextual_keyword(interner::RESERVED_AS) {
            self.scanner.next_token()?;
            self.parse_identifier_name()?
        } else {
            imported.clone()
        };

        Ok(ImportSpecification::ImportSpecifier(ImportSpecifier {
            local: local,
            imported: imported,
        }))
    }

    fn parse_multiple_import_specifiers(&mut self) -> Result<Vec<ImportSpecification>> {
        let mut specifiers = Vec::new();
        self.expect(Token::OpenCurly)?;
        while self.scanner.lookahead != Token::CloseCurly {
            specifiers.push(self.parser_import_specification()?);

            if self.scanner.lookahead != Token::CloseCurly {
                self.expect(Token::Comma)?;
            }
        }
        self.expect(Token::CloseCurly)?;
        Ok(specifiers)
    }

    fn parse_import_declaration(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::ImportKeyword)?;

        match self.scanner.lookahead {
            Token::Star => {
                self.scanner.next_token()?;
                self.expect(Token::Ident(interner::RESERVED_AS))?;
                let local = self.parse_identifier_name()?;
                let specifiers = vec!(ImportSpecification::ImportNamespaceSpecifier(ImportNamespaceSpecifier{local: local}));
                self.expect(Token::Ident(interner::RESERVED_FROM))?;
                let source = self.parse_module_specifier()?;

                Ok(Statement::ImportDeclaration(
                        self.consume_semicolon(start)?,
                        ImportDeclaration { source, specifiers:  specifiers }
                        ))
            },
            Token::OpenCurly => {
                let mut specifiers = self.parse_multiple_import_specifiers()?;
                self.expect(Token::Ident(interner::RESERVED_FROM))?;
                let source = self.parse_module_specifier()?;
                Ok(Statement::ImportDeclaration(
                        self.consume_semicolon(start)?,
                        ImportDeclaration { source, specifiers: specifiers}
                ))

            },
            Token::Ident(_) => {
                let id = self.parse_identifier_name()?;
                let mut specifiers = Vec::new();
                specifiers.push(ImportSpecification::ImportDefaultDeclaration(ImportDefaultDeclaration {
                    identifier: id,
                }));

                if self.scanner.lookahead == Token::Comma {
                    self.scanner.next_token()?;

                    if self.scanner.lookahead == Token::Star {
                        self.scanner.next_token()?;
                        self.expect(Token::Ident(interner::RESERVED_AS))?;
                        let local = self.parse_identifier_name()?;
                        specifiers.push(ImportSpecification::ImportNamespaceSpecifier(ImportNamespaceSpecifier{local: local}));
                    } else {
                        specifiers.append(&mut self.parse_multiple_import_specifiers()?);
                    }
                }

                self.expect(Token::Ident(interner::RESERVED_FROM))?;
                let source = self.parse_module_specifier()?;

                Ok(Statement::ImportDeclaration(
                        self.consume_semicolon(start)?,
                        ImportDeclaration { source, specifiers: specifiers}
                ))
            }
            Token::String(_,_) => {
                let source = self.parse_module_specifier()?;

                Ok(Statement::ImportDeclaration(
                        self.consume_semicolon(start)?,
                        ImportDeclaration{ source: source, specifiers: vec!() }
                ))
            }

            other => {
                Err(CompileError::new(start, ErrorCause::UnexpectedToken(other)))
            }
        }
    }

    fn parse_export_declaration(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::ExportKeyword)?;

        match self.scanner.lookahead {
            Token::Star => {
                self.scanner.next_token()?;
                self.expect(Token::Ident(interner::RESERVED_FROM))?;
                let source = self.parse_module_specifier()?;
                Ok(Statement::ExportAllDeclaration(self.consume_semicolon(start)?, source))
            }
            // is it necessary to special-case const and let?
            Token::Const => {
                let decl = self.parse_const_declaration()?;

                let export = ExportNamedDeclaration {
                    declaration: Some(Box::new(decl)),
                    specifiers: Vec::new(),
                    source: None
                };
                Ok(Statement::ExportNamedDeclaration(self.finalize(start), export))
            },
            Token::Ident(interner::RESERVED_LET) => {
                let let_start = self.scanner.lookahead_start;
                self.scanner.next_token()?;
                let decl = self.parse_let_declaration(let_start)?;

                let export = ExportNamedDeclaration {
                    declaration: Some(Box::new(decl)),
                    specifiers: Vec::new(),
                    source: None
                };
                Ok(Statement::ExportNamedDeclaration(self.finalize(start), export))
            }
            Token::Var | Token::FunctionKeyword => {
                let decl = self.parse_statement(true)?;

                let export = ExportNamedDeclaration {
                    declaration: Some(Box::new(decl)),
                    specifiers: Vec::new(),
                    source: None
                };
                Ok(Statement::ExportNamedDeclaration(self.finalize(start), export))
            }
            Token::DefaultKeyword => {
                self.scanner.next_token()?;
                match self.scanner.lookahead {
                    Token::FunctionKeyword => {
                        let declaration = self.parse_function(false).map(Statement::FunctionDeclaration).map(Box::new).map(DefaultExportable::Statement)?;
                        Ok(Statement::ExportDefaultDeclaration(
                                self.finalize(start),
                                ExportDefaultDeclaration { declaration }
                                ))
                    }
                    Token::ClassKeyword => {
                        let class_start = self.scanner.lookahead_start;
                        let declaration = self.parse_class(true).map(|c| Statement::ClassDeclaration(self.finalize(class_start), c)).map(Box::new).map(DefaultExportable::Statement)?;
                        Ok(Statement::ExportDefaultDeclaration(
                                self.finalize(start),
                                ExportDefaultDeclaration { declaration }
                                ))
                    }
                    _ => {
                        if self.match_contextual_keyword(interner::RESERVED_FROM) {
                            return Err(self.error(ErrorCause::UnexpectedFrom))
                        }
                        let declaration = self.parse_assignment_expression().map(DefaultExportable::Expression)?;
                        Ok(Statement::ExportDefaultDeclaration(
                                self.consume_semicolon(start)?,
                                ExportDefaultDeclaration { declaration }
                                ))
                    }
                }
            }
            Token::OpenCurly => {
                let mut is_export_default = false;
                let mut specifiers = Vec::new();
                self.expect(Token::OpenCurly)?;
                while self.scanner.lookahead != Token::CloseCurly {
                    is_export_default = is_export_default || self.scanner.lookahead == Token::DefaultKeyword;
                    specifiers.push(self.parse_export_specifier()?);

                    if self.scanner.lookahead != Token::CloseCurly {
                        self.expect(Token::Comma)?;
                    }
                }

                self.expect(Token::CloseCurly)?;

                let source = if self.match_contextual_keyword(interner::RESERVED_FROM) {
                    self.scanner.next_token()?;
                    Some(self.parse_module_specifier()?)
                } else if is_export_default {
                    return if self.scanner.is_eof() {
                        Err(self.error(ErrorCause::MissingFromClause))
                    } else {
                        Err(self.error(ErrorCause::UnexpectedToken(self.scanner.lookahead)))
                    }
                } else {
                    None
                };

                let decl = ExportNamedDeclaration {
                    declaration: None,
                    specifiers,
                    source
                };

                Ok(Statement::ExportNamedDeclaration(self.consume_semicolon(start)?, decl))
            }
            t => Err(self.unexpected_token(t))
        }
    }

    fn parse_statement(&mut self, allow_decl: bool) -> Result<Statement> {
        let start = self.scanner.lookahead_start;

        match self.scanner.lookahead {
            Token::Var => self.parse_variable_statement(),
            Token::FunctionKeyword => {
                self.parse_function(true).map(Statement::FunctionDeclaration)
            },
            Token::ExportKeyword => {
                self.parse_export_declaration()
            },
            Token::ImportKeyword => {
                self.parse_import_declaration()
            },
            Token::ClassKeyword => {
                let class = self.parse_class(false)?;
                Ok(Statement::ClassDeclaration(self.finalize(start), class))
            },
            Token::If => self.parse_if_statement(),
            Token::OpenCurly => {
                let block = self.parse_block()?;
                Ok(Statement::Block(block))
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
            Token::Const => {
                if allow_decl {
                    self.parse_const_declaration()
                } else {
                    Err(self.unexpected_token(Token::Const))
                }
            }
            Token::Ident(name) if name == interner::RESERVED_LET && allow_decl => {
                let start = self.scanner.lookahead_start;
                let state = self.scanner.save_state();
                self.scanner.next_token()?;
                match self.scanner.lookahead {
                    Token::Ident(_) | Token::OpenSquare | Token::OpenCurly | Token::YieldKeyword => {
                        self.parse_let_declaration(start)
                    }
                    _ => {
                        self.scanner.restore(state);
                        self.parse_expression_statement()
                    }
                }
            }
            Token::Ident(_) => {
                let start = self.scanner.lookahead_start;
                let expr = self.parse_expression()?;
                if self.scanner.lookahead == Token::Colon {
                    if let &Expression::Identifier(ref id) = &expr {
                        return self.parse_labeled_statement(start, id.clone())
                    }
                }
                Ok(Statement::Expression(self.consume_semicolon(start)?, expr))
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_statement_list_item(&mut self) -> Result<StatementListItem> {
        self.context.is_assignment_target = true;
        self.parse_statement(true).map(StatementListItem::Statement)
    }

    fn parse_block(&mut self) -> Result<Block> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::OpenCurly)?;
        let mut statements = Vec::new();

        while self.scanner.lookahead != Token::CloseCurly {
            statements.push(self.parse_statement_list_item()?);
        }

        self.expect(Token::CloseCurly)?;
        Ok(Block(self.finalize(start), statements))
    }

    fn expect_because(&mut self, expected: Token, cause: ErrorCause) -> Result<Token> {
        if self.scanner.lookahead == expected {
            self.scanner.next_token()
        } else {
            Err(self.error(cause))
        }
    }

    fn expect(&mut self, expected: Token) -> Result<Token> {
        if self.scanner.lookahead == expected {
            self.scanner.next_token()
        } else {
            Err(self.unexpected_token(self.scanner.lookahead))
        }
    }

    fn unexpected_token(&self, token: Token) -> CompileError {
        self.unexpected_token_at(self.scanner.lookahead_start, token)
    }

    fn unexpected_token_at(&self, pos: Position, token: Token) -> CompileError {
        if let Token::Ident(s) = token {
            if s.is_future_reserved_word() {
                return CompileError::new(pos, ErrorCause::UnexpectedReservedWord)

            }
            if self.context.strict && s.is_strict_mode_reserved_word() {
                return CompileError::new(pos, ErrorCause::StrictReservedWord)

            }
        }
        if self.context.strict && token == Token::YieldKeyword {
            return CompileError::new(pos, ErrorCause::StrictReservedWord)
        }
        if token == Token::EnumKeyword {
            return CompileError::new(pos, ErrorCause::UnexpectedReservedWord)
        }
        if token == Token::SuperKeyword {
            return CompileError::new(pos, ErrorCause::UnexpectedReservedWord)
        }
        CompileError::new(pos, ErrorCause::UnexpectedToken(token))
    }

    fn error(&self, cause: ErrorCause) -> CompileError {
        CompileError::new(self.scanner.last_pos, cause)
    }
}
