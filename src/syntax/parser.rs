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
            end: self.scanner.last_pos
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
            Token::BoolTrue => {
                self.scanner.next_token();
                Ok(Expression::Literal(self.finalize(start), Literal::True))
            }
            Token::BoolFalse => {
                self.scanner.next_token();
                Ok(Expression::Literal(self.finalize(start), Literal::False))
            }
            Token::String(ref s) => {
                self.scanner.next_token();
                Ok(Expression::Literal(self.finalize(start), Literal::String(s.clone())))
            }
            Token::Ident(n) => {
                self.scanner.next_token();
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
                self.scanner.next_token();
                Ok(Expression::This(self.finalize(start)))
            },
            Token::Null => {
                self.scanner.next_token();
                Ok(Expression::Literal(self.finalize(start), Literal::Null))
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

    fn expect_identifier_name(&mut self) -> Result<String> {
        match self.match_identifier_name() {
            Some(ident) => {
                self.scanner.next_token();
                Ok(ident)
            }
            None => Err(CompileError::UnexpectedToken(self.scanner.lookahead.clone()))
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
            _ => None
        }
    }

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
                    let identifier_name = self.expect_identifier_name()?;
                    result = Expression::StaticMember(self.finalize(start), Box::new(result), identifier_name)

                }
                _ => break,
            }
        }

        Ok(result)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression> {
        if let Some(prefix) = self.match_unary_operator() {
            let start = self.scanner.lookahead_start;
            self.scanner.next_token();
            let expr = self.parse_unary_expression()?;
            Ok(Expression::Unary(self.finalize(start), prefix, Box::new(expr)))
        } else {
            self.parse_update_expression()
        }
    }

    fn parse_update_expression(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        if let Some(op) = self.match_update_op() {
            self.scanner.next_token();
            let expr = self.parse_unary_expression()?;
            Ok(Expression::Update(self.finalize(start), op, Box::new(expr), true))
        } else {
            let expr = self.parse_lhs_expression(true)?;
            if let Some(op) = self.match_update_op() {
                self.scanner.next_token();
                Ok(Expression::Update(self.finalize(start), op, Box::new(expr), false))
            } else {
                Ok(expr)
            }
        }
    }

    fn match_update_op(&mut self) -> Option<UpdateOp> {
        match self.scanner.lookahead {
            Token::PlusPlus => Some(UpdateOp::PlusPlus),
            Token::MinusMinus => Some(UpdateOp::MinusMinus),
            _ => None
        }
    }

    fn match_unary_operator(&mut self) -> Option<UnOp> {
        match self.scanner.lookahead {
            Token::Bang => Some(UnOp::Not),
            Token::Minus => Some(UnOp::Minus),
            Token::Plus => Some(UnOp::Plus),
            Token::Tilde => Some(UnOp::Tilde),
            Token::Void => Some(UnOp::Void),
            Token::Delete => Some(UnOp::Delete),
            Token::Typeof => Some(UnOp::Typeof),
            _ => None
        }
    }

    fn match_infix(&mut self) -> Option<InfixOp> {
        match self.scanner.lookahead {
            Token::Plus => Some(InfixOp::BinOp(BinOp::Plus)),
            Token::BitXor => Some(InfixOp::BinOp(BinOp::BitXor)),
            Token::BitAnd => Some(InfixOp::BinOp(BinOp::BitAnd)),
            Token::BitOr => Some(InfixOp::BinOp(BinOp::BitOr)),
            Token::LShift => Some(InfixOp::BinOp(BinOp::LShift)),
            Token::RShift => Some(InfixOp::BinOp(BinOp::RShift)),
            Token::URShift => Some(InfixOp::BinOp(BinOp::URShift)),
            Token::Times => Some(InfixOp::BinOp(BinOp::Times)),
            Token::Div => Some(InfixOp::BinOp(BinOp::Div)),
            Token::Mod => Some(InfixOp::BinOp(BinOp::Mod)),
            Token::Minus => Some(InfixOp::BinOp(BinOp::Minus)),
            Token::EqEq => Some(InfixOp::BinOp(BinOp::EqEq)),
            Token::NotEq => Some(InfixOp::BinOp(BinOp::NotEq)),
            Token::NotEqEq => Some(InfixOp::BinOp(BinOp::NotEqEq)),
            Token::EqEqEq => Some(InfixOp::BinOp(BinOp::EqEqEq)),
            Token::Lt => Some(InfixOp::BinOp(BinOp::Lt)),
            Token::Lte => Some(InfixOp::BinOp(BinOp::Lte)),
            Token::Gt => Some(InfixOp::BinOp(BinOp::Gt)),
            Token::Gte => Some(InfixOp::BinOp(BinOp::Gte)),
            Token::In => Some(InfixOp::BinOp(BinOp::In)),
            Token::Instanceof => Some(InfixOp::BinOp(BinOp::Instanceof)),
            Token::LogicalAnd => Some(InfixOp::LogOp(LogOp::AndAnd)),
            Token::LogicalOr => Some(InfixOp::LogOp(LogOp::OrOr)),
            _ => None
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
                    let marker = markers.last().unwrap();
                    expressions.push(self.combine_binary(operator, expr, right, *marker))
                }
                self.scanner.next_token();
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

        if self.scanner.lookahead == Token::QuestionMark {
            self.scanner.next_token();
            let consequent = self.parse_assignment_expression()?;
            self.expect(Token::Colon);
            let alternate = self.parse_assignment_expression()?;
            let span = self.finalize(start);
            Ok(Expression::Conditional(span, Box::new(expr), Box::new(consequent), Box::new(alternate)))
        } else {
            Ok(expr)
        }
    }

    fn match_assignment(&mut self) -> Option<AssignOp> {
        match self.scanner.lookahead {
            Token::Eq => Some(AssignOp::Eq),
            Token::TimesEq => Some(AssignOp::TimesEq),
            Token::DivEq => Some(AssignOp::DivEq),
            Token::ModEq => Some(AssignOp::ModEq),
            Token::PlusEq => Some(AssignOp::PlusEq),
            Token::MinusEq => Some(AssignOp::MinusEq),
            Token::LShiftEq => Some(AssignOp::LShiftEq),
            Token::RShiftEq => Some(AssignOp::RShiftEq),
            Token::URShiftEq => Some(AssignOp::URShiftEq),
            Token::BitAndEq => Some(AssignOp::BitAndEq),
            Token::BitXorEq => Some(AssignOp::BitXorEq),
            Token::BitOrEq => Some(AssignOp::BitOrEq),
            _ => None
        }
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression> {
        let left = self.parse_conditional_expression()?;
        match self.match_assignment() {
            Some(op) => {
                self.scanner.next_token();
                match left {
                    Expression::Identifier(_, _) => {
                        let right = self.parse_assignment_expression()?;
                        let span = left.span().merge(right.span());
                        Ok(Expression::Assignment(span, op, Box::new(left), Box::new(right)))
                    }
                    _ => panic!("Invalid assignment target")
                }
            }
            None => Ok(left)
        }
    }

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

    fn match_object_property_key(&mut self) -> Option<PropKey> {
        let start = self.scanner.lookahead_start;

        let token = self.scanner.lookahead.clone();
        match token {
            Token::String(ref s) => {
                self.scanner.next_token();
                Some(PropKey::String(self.finalize(start), s.to_string()))
            }
            Token::Number(n) => {
                self.scanner.next_token();
                Some(PropKey::Number(self.finalize(start), n))
            },
            _ => {
                self.match_identifier_name().map(|name| {
                    self.scanner.next_token();
                    PropKey::Identifier(self.finalize(start), name)
                })
            }
        }
    }

    fn parse_prop_init(&mut self, start: Position, key: PropKey) -> Result<Prop> {
        self.expect(Token::Colon);
        let value = self.parse_assignment_expression()?;
        Ok(Prop::Init(self.finalize(start), key, value))
    }

    fn parse_object_property(&mut self) -> Result<Prop> {
        let start = self.scanner.lookahead_start;
        let token = self.scanner.lookahead.clone();

        if token == Token::Ident("get".to_string()) {
            self.scanner.next_token();
            if let Some(key) = self.match_object_property_key() {
                let parameters = self.parse_function_parameters();
                let block = self.parse_block()?;
                let value = Function { id: None, body: block, parameters: parameters };
                Ok(Prop::Get(self.finalize(start), key, value))
            } else {
                let span = self.finalize(start);
                self.parse_prop_init(start, PropKey::Identifier(span, "get".to_string()))
            }
        } else if token == Token::Ident("set".to_string()) {
            self.scanner.next_token();
            if let Some(key) = self.match_object_property_key() {
                let parameters = self.parse_function_parameters();
                let block = self.parse_block()?;
                let value = Function { id: None, body: block, parameters: parameters };
                Ok(Prop::Set(self.finalize(start), key, value))
            } else {
                let span = self.finalize(start);
                self.parse_prop_init(start, PropKey::Identifier(span, "set".to_string()))
            }
        } else {
            let key = self.match_object_property_key().unwrap();
            self.parse_prop_init(start, key)
        }
    }

    fn parse_object_initializer(&mut self) -> Result<Expression> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::OpenCurly);

        let mut properties = Vec::new();

        while self.scanner.lookahead != Token::CloseCurly {
            properties.push(self.parse_object_property()?);

            if self.scanner.lookahead != Token::CloseCurly {
                self.expect(Token::Comma);
            }
        };

        self.expect(Token::CloseCurly);
        Ok(Expression::Object(self.finalize(start), properties))
    }

    fn parse_variable_declarator(&mut self) -> Result<VariableDeclarator> {
        match self.scanner.next_token() {
            Token::Ident(name) => {
                let init = match self.scanner.lookahead {
                    Token::Eq => {
                        self.scanner.next_token();
                        Some(self.parse_primary_expression()?)
                    }
                    _ => None
                };
                Ok(VariableDeclarator { id: name, init: init })
            }
            t => Err(CompileError::UnexpectedToken(t))
        }
    }

    fn parse_variable_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Var);
        let mut declarators = Vec::new();
        declarators.push(self.parse_variable_declarator()?);
        while self.scanner.lookahead == Token::Comma {
            self.scanner.next_token();
            declarators.push(self.parse_variable_declarator()?)
        }
        Ok(Statement::VariableDeclaration(self.consume_semicolon(start)?, VariableDeclaration {
            kind: VariableDeclarationKind::Var,
            declarations: declarators
        }))
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
        let start = self.scanner.lookahead_start;
        let expr = self.parse_assignment_expression()?;

        if self.scanner.lookahead == Token::Comma {
            let mut expressions = vec![expr];
            while self.scanner.lookahead != Token::Eof {
                if self.scanner.lookahead == Token::Comma {
                    self.scanner.next_token();
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
        if self.scanner.lookahead == Token::Semi {
            self.scanner.next_token();
            Ok(self.finalize(start))
        } else if self.scanner.at_newline() {
            Ok(self.finalize(start))
        } else {
            Ok(Span {
                start: start,
                end: self.scanner.lookahead_start
            })
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        let expr = self.parse_expression()?;
        Ok(Statement::Expression(self.consume_semicolon(start)?, expr))
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

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::Return);

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
        self.expect(Token::ThrowKeyword);
        let argument = self.parse_expression()?;
        Ok(Statement::Throw(self.consume_semicolon(start)?, argument))
    }

    fn parse_catch_clause(&mut self) -> Result<CatchClause> {
        self.expect(Token::CatchKeyword);
        self.expect(Token::OpenParen);
        let param = match self.scanner.next_token() {
            Token::Ident(s) => s,
            t => return Err(CompileError::UnexpectedToken(t))
        };
        self.expect(Token::CloseParen);
        let body = self.parse_block()?;
        Ok(CatchClause { param: param, body: body })
    }

    fn parse_try_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::TryKeyword);
        let block = self.parse_block()?;
        let handler = if self.scanner.lookahead == Token::CatchKeyword {
            Some(self.parse_catch_clause()?)
        } else {
            None
        };
        let finalizer = if self.scanner.lookahead == Token::FinallyKeyword {
            self.scanner.next_token();
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::Try(self.consume_semicolon(start)?, block, handler, finalizer))
    }

    fn parse_switch_case(&mut self) -> Result<SwitchCase> {
        let test = if self.scanner.lookahead == Token::DefaultKeyword {
            self.scanner.next_token();
            None
        } else {
            self.expect(Token::CaseKeyword);
            Some(self.parse_expression()?)
        };
        self.expect(Token::Colon);

        let mut consequent = Vec::new();
        loop {
            if self.scanner.lookahead == Token::CloseCurly
                || self.scanner.lookahead == Token::DefaultKeyword
                    || self.scanner.lookahead == Token::CaseKeyword {
                        break;
                    };
            consequent.push(self.parse_statement_list_item()?);
        };

        Ok(SwitchCase { test: test, consequent: Block(consequent) })
    }

    fn parse_switch_statement(&mut self) -> Result<Statement> {
        let start = self.scanner.lookahead_start;
        self.expect(Token::SwitchKeyword);

        self.expect(Token::OpenParen);
        let discriminant = self.parse_expression()?;
        self.expect(Token::CloseParen);

        let mut cases = Vec::new();
        self.expect(Token::OpenCurly);
        loop {
            if self.scanner.lookahead == Token::CloseCurly {
                break;
            };

            cases.push(self.parse_switch_case()?);
        };
        self.expect(Token::CloseCurly);

        Ok(Statement::Switch(self.consume_semicolon(start)?, discriminant, cases))
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
                self.scanner.next_token();
                Ok(Statement::Empty(self.finalize(start)))
            }
            Token::DebuggerKeyword => {
                self.scanner.next_token();
                Ok(Statement::Debugger(self.consume_semicolon(start)?))
            }
            Token::ThrowKeyword => self.parse_throw_statement(),
            Token::TryKeyword => self.parse_try_statement(),
            Token::Return => self.parse_return_statement(),
            Token::SwitchKeyword => self.parse_switch_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_statement_list_item(&mut self) -> Result<StatementListItem> {
        self.parse_statement().map(StatementListItem::Statement)
    }

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
