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
    in_iteration: bool,
    in_switch: bool,
    in_function_body: bool,
    strict: bool,
    is_assignment_target: bool,
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
                in_iteration: false,
                in_switch: false,
                in_function_body: false,
                strict: false,
                is_assignment_target: false,
                labels: HashSet::new()
            }
        }
    }

    fn directive_opt(&mut self, expr: &Expression) -> Option<String> {
        if let Expression::Literal(_, Literal::String(val)) = *expr {
            let string = interner::resolve(val);
            let len = string.len();
            Some(string[1..len - 1].to_owned())
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

    fn isolate_cover_grammar<F, T>(&mut self, parse_fn: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T> {
        let is_assignment_target = std::mem::replace(&mut self.context.is_assignment_target, true);
        let result = parse_fn(self);
        std::mem::replace(&mut self.context.is_assignment_target, is_assignment_target);
        result
    }

    fn inherit_cover_grammar<F, T>(&mut self, parse_fn: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T> {
        let is_assignment_target = std::mem::replace(&mut self.context.is_assignment_target, true);
        let result = parse_fn(self);
        if self.context.is_assignment_target && is_assignment_target {
            std::mem::replace(&mut self.context.is_assignment_target, true);
        }
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
                Ok(Expression::Literal(self.finalize(start), Literal::Number(n)))
            }
            Token::BoolTrue => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                Ok(Expression::Literal(self.finalize(start), Literal::True))
            }
            Token::BoolFalse => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                Ok(Expression::Literal(self.finalize(start), Literal::False))
            }
            Token::String(s) => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                Ok(Expression::Literal(self.finalize(start), Literal::String(s)))
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
                self.context.is_assignment_target = false;
                Ok(Expression::Function(self.finalize(start), fun))
            },
            Token::ThisKeyword => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                Ok(Expression::This(self.finalize(start)))
            },
            Token::Null => {
                self.scanner.next_token()?;
                self.context.is_assignment_target = false;
                Ok(Expression::Literal(self.finalize(start), Literal::Null))
            },
            Token::Div | Token::DivEq => {
                let regex = self.scanner.regex_token()?;
                self.context.is_assignment_target = false;
                Ok(Expression::Literal(self.finalize(start), regex))
            },
            t => {
                Err(self.unexpected_token(t))
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
                let argument = ArgumentListElement::Expression(self.isolate_cover_grammar(Parser::parse_assignment_expression)?);
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

    fn expect_identifier_name(&mut self) -> Result<Symbol> {
        match self.match_identifier_name() {
            Some(ident) => {
                self.scanner.next_token()?;
                Ok(ident)
            }
            None => Err(self.error(ErrorCause::UnexpectedToken(self.scanner.lookahead)))
        }
    }

    fn match_identifier_name(&mut self) -> Option<Symbol> {
        match self.scanner.lookahead {
            Token::Ident(name) => Some(name),
            Token::If => Some(*interner::KEYWORD_IF),
            Token::Else => Some(*interner::KEYWORD_ELSE),
            Token::Null => Some(*interner::KEYWORD_NULL),
            Token::BoolTrue => Some(*interner::KEYWORD_TRUE),
            Token::BoolFalse => Some(*interner::KEYWORD_FALSE),
            Token::In => Some(*interner::KEYWORD_IN),
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
                        self.context.is_assignment_target = false;
                        let args = self.parse_arguments()?;
                        let span = self.finalize(start);
                        result = Expression::Call(span, Box::new(result), args);
                    } else {
                        break;
                    }
                },
                Token::OpenSquare => {
                    self.context.is_assignment_target = true;
                    self.expect(Token::OpenSquare)?;
                    let expr = self.isolate_cover_grammar(Parser::parse_expression)?;
                    self.expect(Token::CloseSquare)?;
                    let span = self.finalize(start);
                    result = Expression::ComputedMember(span, Box::new(result), Box::new(expr));
                },
                Token::Dot => {
                    self.context.is_assignment_target = true;
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
            let expr = self.inherit_cover_grammar(Parser::parse_unary_expression)?;
            if self.context.strict && prefix == UnOp::Delete {
                if let Expression::Identifier(_, _) = expr {
                    return Err(self.error(ErrorCause::UnqualifiedDelete))
                }
            }
            self.context.is_assignment_target = false;
            Ok(Expression::Unary(self.finalize(start), prefix, Box::new(expr)))
        } else {
            self.parse_update_expression()
        }
    }

    fn check_reserved_expr_at(&self, expr: &Expression, pos: Position, cause: ErrorCause) -> Result<()> {
        if let &Expression::Identifier(_, s) = expr {
            self.check_reserved_at(s, pos, cause)?;
        }
        return Ok(())
    }

    fn check_reserved_pat_at(&self, pat: &Pattern, pos: Position, cause: ErrorCause) -> Result<()> {
        let &Pattern::Identifier(_, s) = pat;
        self.check_reserved_at(s, pos, cause)
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
        return Ok(())
    }

    fn check_assignment_allowed(&mut self) -> Result<()> {
        if !self.context.is_assignment_target {
            return Err(self.error(ErrorCause::InvalidLHSAssignment));
        }
        return Ok(())
    }

    fn parse_update_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        if let Some(op) = self.scanner.lookahead.as_update_op() {
            self.scanner.next_token()?;
            let expr = self.inherit_cover_grammar(Parser::parse_unary_expression)?;
            self.check_reserved_expr_at(&expr, self.scanner.last_pos, ErrorCause::RestrictedVarNameInPrefix)?;
            self.check_assignment_allowed()?;
            Ok(Expression::Update(self.finalize(start), op, Box::new(expr), true))
        } else {
            let expr = self.inherit_cover_grammar(Parser::parse_lhs_expression_allow_call)?;

            if self.scanner.at_newline() {
                return Ok(expr);
            };

            if let Some(op) = self.scanner.lookahead.as_update_op() {
                self.check_reserved_expr_at(&expr, self.scanner.last_pos, ErrorCause::RestrictedVarNameInPostfix)?;
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

    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        let left = self.parse_conditional_expression()?;
        match self.scanner.lookahead.as_assign_op() {
            Some(op) => {
                self.check_reserved_expr_at(&left, start, ErrorCause::RestrictedVarNameInAssignment)?;
                self.check_assignment_allowed()?;
                self.scanner.next_token()?;
                let right = self.parse_assignment_expression()?;
                let span = self.finalize(start);
                Ok(Expression::Assignment(span, op, Box::new(left), Box::new(right)))
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

        match self.scanner.lookahead {
            Token::String(s) => {
                self.scanner.next_token()?;
                Ok(Some(PropKey::String(self.finalize(start), s)))
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

        if self.scanner.lookahead == Token::Ident(*interner::KEYWORD_GET) {
            self.scanner.next_token()?;
            if let Some(key) = self.match_object_property_key()? {
                let previous_strict = self.context.strict;
                let parameters = self.parse_function_parameters()?;
                self.validate_params(&parameters)?;
                let block = self.parse_function_source_elements()?;
                let value = Function { id: None, body: block, parameters: parameters };
                self.context.strict = previous_strict;
                Ok(Prop::Get(self.finalize(start), key, value))
            } else {
                let span = self.finalize(start);
                self.parse_prop_init(start, PropKey::Identifier(span, *interner::KEYWORD_GET))
            }
        } else if self.scanner.lookahead == Token::Ident(*interner::KEYWORD_SET) {
            self.scanner.next_token()?;
            if let Some(key) = self.match_object_property_key()? {
                let previous_strict = self.context.strict;
                let parameters = self.parse_function_parameters()?;
                self.validate_params(&parameters)?;
                let block = self.parse_function_source_elements()?;
                let value = Function { id: None, body: block, parameters: parameters };
                self.context.strict = previous_strict;
                Ok(Prop::Set(self.finalize(start), key, value))
            } else {
                let span = self.finalize(start);
                self.parse_prop_init(start, PropKey::Identifier(span, *interner::KEYWORD_SET))
            }
        } else if let Some(key) = self.match_object_property_key()? {
            self.parse_prop_init(start, key)
        } else {
            Err(self.unexpected_token(self.scanner.lookahead))
        }
    }

    fn check_duplicate_proto(&self, has_proto: bool, prop: &Prop) -> Result<bool> {
        if let &Prop::Init(_, ref key, _) = prop {
            if let &PropKey::Identifier(ref span, s) = key {
                if s == *interner::KEYWORD_PROTO {
                    if has_proto {
                        return Err(CompileError::new(span.end, ErrorCause::DuplicateProto))
                    } else {
                        return Ok(true);
                    }
                }
            }
        }
        return Ok(has_proto);
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

    fn parse_pattern(&mut self, kind: VariableDeclarationKind) -> Result<Pattern> {
        match self.scanner.lookahead {
            Token::Ident(name) => {
                if name == *interner::RESERVED_LET {
                    if kind == VariableDeclarationKind::Let || kind == VariableDeclarationKind::Const {
                        return Err(CompileError::new(self.scanner.lookahead_start, ErrorCause::LetInLexicalBinding))
                    }
                }
                let start = self.scanner.lookahead_start;
                self.scanner.next_token()?;
                Ok(Pattern::Identifier(self.finalize(start), name))
            },
            t => Err(self.unexpected_token(t))
        }
    }

    fn parse_variable_declarator(&mut self, kind: VariableDeclarationKind) -> Result<VariableDeclarator> {
        let start = self.scanner.lookahead_start;
        let id = self.parse_pattern(kind)?;

        let init = match self.scanner.lookahead {
            Token::Eq => {
                self.check_reserved_pat_at(&id, self.scanner.last_pos, ErrorCause::RestrictedVarName)?;
                self.scanner.next_token()?;
                Some(self.parse_assignment_expression()?)
            }
            _ if kind == VariableDeclarationKind::Const => {
                return Err(self.error(ErrorCause::MissingInitializerInConst))
            }
            _ => {
                self.check_reserved_pat_at(&id, start, ErrorCause::RestrictedVarName)?;
                None
            }
        };

        Ok(VariableDeclarator {id, init})
    }

    fn parse_variable_declaration(&mut self, kind: VariableDeclarationKind) -> Result<VariableDeclaration> {
        let mut declarators = Vec::new();
        declarators.push(self.parse_variable_declarator(kind)?);
        while self.eat(Token::Comma)? {
            declarators.push(self.parse_variable_declarator(kind)?)
        }
        Ok(VariableDeclaration {
            kind: kind,
            declarations: declarators
        })
    }

    fn parse_variable_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Var)?;
        let declaration = self.parse_variable_declaration(VariableDeclarationKind::Var)?;
        Ok(Statement::VariableDeclaration(self.consume_semicolon(start)?, declaration))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Pattern>> {
        self.expect(Token::OpenParen)?;
        let mut parameters = Vec::new();

        loop {
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            }
            parameters.push(self.parse_pattern(VariableDeclarationKind::Var)?);
            if let Token::CloseParen = self.scanner.lookahead {
                break;
            }
            self.expect(Token::Comma)?;
        }

        self.expect(Token::CloseParen)?;

        Ok(parameters)
    }

    fn validate_params(&self, params: &Vec<Pattern>) -> Result<()> {
        let mut param_names = HashSet::new();
        for param in params {
            let &Pattern::Identifier(ref sp, id) = param;
            self.check_reserved_at(id, sp.start, ErrorCause::StrictParamName)?;
            if self.context.strict && param_names.contains(&id) {
                return Err(CompileError::new(sp.start, ErrorCause::StrictDupeParam))
            }
            param_names.insert(id);
        };
        return Ok(())
    }

    fn parse_function(&mut self) -> Result<Function> {
        self.expect(Token::FunctionKeyword)?;

        let id_loc = self.scanner.lookahead_start;
        let id = match self.scanner.lookahead {
            Token::Ident(name) => {
                self.check_reserved_at(name, self.scanner.lookahead_start, ErrorCause::RestrictedVarNameInFunction)?;
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

        let previous_strict = self.context.strict;

        let parameters = self.parse_function_parameters()?;
        let block = self.parse_function_source_elements()?;

        self.validate_params(&parameters)?;

        if let Some(name) = id {
            self.check_reserved_at(name, id_loc, ErrorCause::RestrictedVarNameInFunction)?;
        }

        self.context.strict = previous_strict;

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
        let param = match self.scanner.lookahead {
            Token::Ident(s) => {
                self.scanner.next_token()?;
                self.check_reserved_at(s, self.scanner.lookahead_start, ErrorCause::RestrictedVarNameInCatch)?;
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

    fn parse_for_in_statement(&mut self, start: Position, left: ForInit) -> Result<Statement> {
        let right = self.parse_expression()?;
        self.expect(Token::CloseParen)?;
        let body = self.in_iteration(true, |c| Parser::parse_statement(c, true))?;
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

        let body = self.in_iteration(true, |c| Parser::parse_statement(c, true))?;
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
            let decl = self.allow_in(false, |context| Parser::parse_variable_declaration(context, VariableDeclarationKind::Var))?;
            if decl.declarations.len() == 1 && self.eat(Token::In)? {
                self.parse_for_in_statement(start, ForInit::VarDecl(decl))

            } else {
                self.expect(Token::Semi)?;
                self.parse_for_iter_statement(start, Some(ForInit::VarDecl(decl)))
            }
        } else {
            let previous_allow_in = std::mem::replace(&mut self.context.allow_in, false);
            let init = ForInit::Expression(self.inherit_cover_grammar(Parser::parse_expression)?);
            self.context.allow_in = previous_allow_in;
            if self.scanner.lookahead == Token::In {
                if !self.context.is_assignment_target {
                    return Err(self.error(ErrorCause::InvalidLHSForIn))
                };
                self.scanner.next_token()?;
                self.parse_for_in_statement(start, init)
            } else {
                self.expect(Token::Semi)?;
                self.parse_for_iter_statement(start, Some(init))
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
        if self.context.labels.contains(&id.1) {
            return Err(self.error(ErrorCause::DuplicateLabel(interner::resolve(id.1).to_owned())))
        }
        self.context.labels.insert(id.1);
        let body = self.parse_statement(true)?;
        self.context.labels.remove(&id.1);
        Ok(Statement::Labeled(self.finalize(start), id, Box::new(body)))
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
                Ok(Id(self.finalize(start), s))
            },
            t => Err(self.error(ErrorCause::UnexpectedToken(t)))
        }
    }

    fn as_id(&self, expr: &Expression) -> Option<Id> {
        match *expr {
            Expression::Identifier(ref sp, id) => Some(Id(sp.clone(), id)),
            _ => None
        }
    }

    fn parse_const_declaration(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Const)?;
        let declaration = self.parse_variable_declaration(VariableDeclarationKind::Const)?;
        Ok(Statement::VariableDeclaration(self.consume_semicolon(start)?, declaration))
    }

    fn parse_let_declaration(&mut self, start: Position) -> Result<Statement> {
        let declaration = self.parse_variable_declaration(VariableDeclarationKind::Let)?;
        Ok(Statement::VariableDeclaration(self.consume_semicolon(start)?, declaration))
    }

    fn parse_statement(&mut self, allow_decl: bool) -> Result<Statement> {
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
            Token::Const => {
                if allow_decl {
                    self.parse_const_declaration()
                } else {
                    Err(self.unexpected_token(Token::Const))
                }
            }
            Token::Ident(_) => {
                let start = self.scanner.lookahead_start;
                let expr = self.parse_expression()?;
                if allow_decl && self.is_let_decl(&expr) {
                    self.parse_let_declaration(start)
                } else {
                    let id_opt = self.as_id(&expr);
                    if id_opt.is_some() && self.eat(Token::Colon)? {
                        self.parse_labeled_statement(start, id_opt.unwrap())
                    } else {
                        Ok(Statement::Expression(self.consume_semicolon(start)?, expr))
                    }
                }
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn is_let_decl(&self, expr: &Expression) -> bool {
        match expr {
            &Expression::Identifier(_, name) if name == *interner::RESERVED_LET => {
                match self.scanner.lookahead {
                    Token::Ident(_) | Token::OpenParen | Token::OpenSquare => true,
                    _ => false
                }
            }
            _ => false
        }
    }

    fn parse_statement_list_item(&mut self) -> Result<StatementListItem> {
        self.context.is_assignment_target = true;
        self.parse_statement(true).map(StatementListItem::Statement)
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
        if self.scanner.lookahead == expected {
            self.scanner.next_token()
        } else {
            Err(self.unexpected_token(self.scanner.lookahead))
        }
    }

    fn unexpected_token(&self, token: Token) -> CompileError {
        if let Token::Ident(s) = token {
            if s.is_future_reserved_word() {
                return CompileError::new(self.scanner.lookahead_start, ErrorCause::UnexpectedReservedWord)

            }
            if self.context.strict && s.is_strict_mode_reserved_word() {
                return CompileError::new(self.scanner.lookahead_start, ErrorCause::StrictReservedWord)

            }
        }
        CompileError::new(self.scanner.lookahead_start, ErrorCause::UnexpectedToken(token))
    }

    fn error(&self, cause: ErrorCause) -> CompileError {
        CompileError::new(self.scanner.last_pos, cause)
    }
}
