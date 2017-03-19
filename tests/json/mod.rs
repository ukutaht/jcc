extern crate jcc;

use std;
use jcc::syntax::ast::*;
use jcc::syntax::span::{Span, Position};
use serde_json::value::Value;

pub type Result<T> = std::result::Result<T, ()>;

fn expect_string<'a>(node: &'a Value, key: &str) -> &'a str {
    node.as_object().unwrap().get(key).unwrap().as_str().unwrap()
}

fn expect_array<'a>(node: &'a Value, key: &str) -> &'a Vec<Value> {
    node.as_object().unwrap().get(key).unwrap().as_array().unwrap()
}

fn expect_u64<'a>(node: &'a Value, key: &str) -> u64 {
    node.as_object().unwrap().get(key).unwrap().as_u64().unwrap()
}

fn expect_bool<'a>(node: &'a Value, key: &str) -> bool {
    node.as_object().unwrap().get(key).unwrap().as_bool().unwrap()
}

fn expect_value<'a>(node: &'a Value, key: &str) -> &'a Value {
    node.as_object().unwrap().get(key).unwrap()
}

fn position(node: &Value) -> Result<Position> {
    Ok(Position {
        line: expect_u64(node, "line") as u32,
        column: expect_u64(node, "column") as u32
    })
}

fn span(node: &Value) -> Result<Span> {
    let loc = expect_value(node, "loc");
    let start = expect_value(loc, "start");
    let end = expect_value(loc, "end");

    Ok(Span {
        start: position(start)?,
        end: position(end)?
    })
}

fn binary_expression(node: &Value) -> Result<Expression> {
    let left = expression(expect_value(node, "left"))?;
    let right = expression(expect_value(node, "right"))?;
    let span = span(node)?;

   let op =  match expect_string(node, "operator") {
        "==" => BinOp::EqEq,
        "===" => BinOp::EqEqEq,
        "!=" => BinOp::NotEq,
        "!==" => BinOp::NotEqEq,
        "+" => BinOp::Plus,
        "-" => BinOp::Minus,
        "*" => BinOp::Times,
        "/" => BinOp::Div,
        "%" => BinOp::Mod,
        _ => return Err(())
    };

    Ok(Expression::Binary(span, op, Box::new(left), Box::new(right)))
}

fn assignment_expression(node: &Value) -> Result<Expression> {
    let left = expression(expect_value(node, "left"))?;
    let right = expression(expect_value(node, "right"))?;
    let span = span(node)?;

    match expect_string(node, "operator") {
        "=" => {
            Ok(Expression::Assignment(span, AssignOp::Eq, Box::new(left), Box::new(right)))
        }
        _ => Err(())
    }
}

fn array_expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;
    let mut elements = Vec::new();

    for element in expect_array(node, "elements") {
        if element.is_null() {
            elements.push(None)
        } else {
            elements.push(Some(expression(element)?));
        }
    }

    Ok(Expression::Array(span, elements))
}

fn new_expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;
    let callee = expression(expect_value(node, "callee"))?;
    let mut arguments = Vec::new();

    for argument in expect_array(node, "arguments") {
        arguments.push(ArgumentListElement::Expression(expression(argument)?));
    }

    Ok(Expression::New(span, Box::new(callee), arguments))
}

fn member_expression(node: &Value) -> Result<Expression> {
    let object = expression(expect_value(node, "object"))?;
    let prop = expression(expect_value(node, "property"))?;
    let span = span(node)?;

    if expect_bool(node, "computed") {
        Ok(Expression::ComputedMember(span, Box::new(object), Box::new(prop)))
    } else {
        match prop {
            Expression::Identifier(_, ref s) => {
                Ok(Expression::StaticMember(span, Box::new(object), s.clone()))
            }
            _ => Err(())
        }
    }
}

fn literal(node: &Value) -> Result<Literal> {
    let val = expect_value(node, "value");

    if val.is_number() {
        Ok(Literal::Number(val.as_f64().unwrap()))
    } else {
        Err(())
    }
}

fn logical_expression(node: &Value) -> Result<Expression> {
    let left = expression(expect_value(node, "left"))?;
    let right = expression(expect_value(node, "right"))?;
    let span = span(node)?;

    let op = match expect_string(node, "operator") {
        "&&" => LogOp::AndAnd,
        "||" => LogOp::OrOr,
        _ => return Err(())
    };

    Ok(Expression::Logical(span, op, Box::new(left), Box::new(right)))
}

fn call_expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;
    let callee = expression(expect_value(node, "callee"))?;
    let mut arguments = Vec::new();

    for argument in expect_array(node, "arguments") {
        arguments.push(ArgumentListElement::Expression(expression(argument)?));
    }

    Ok(Expression::Call(span, Box::new(callee), arguments))
}

fn expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;

    match expect_string(node, "type") {
        "AssignmentExpression" => {
            assignment_expression(node)
        },
        "ArrayExpression" => {
            array_expression(node)
        },
        "BinaryExpression" => {
            binary_expression(node)
        },
        "CallExpression" => {
            call_expression(node)
        },
        "Identifier" => {
            Ok(Expression::Identifier(span, expect_string(node, "name").to_owned()))
        }
        "Literal" => {
            Ok(Expression::Literal(span, literal(node)?))
        }
        "LogicalExpression" => {
            logical_expression(node)
        }
        "MemberExpression" => {
            member_expression(node)
        },
        "NewExpression" => {
            new_expression(node)
        },
        _ => Err(())
    }
}

fn statement(node: &Value) -> Result<Statement> {
    let obj = node.as_object().unwrap();
    match obj.get("type").unwrap().as_str().unwrap() {
        "ExpressionStatement" => {
            expression(obj.get("expression").unwrap()).map(Statement::Expression)
        }
        _ => Err(())
    }
}

fn statement_list_item(node: &Value) -> Result<StatementListItem> {
    statement(node).map(StatementListItem::Statement)
}

pub fn parse_program(value: &Value) -> Program {
    let body = value.get("body").unwrap().as_array().unwrap();
    let mut statement_list_items = Vec::new();

    for node in body {
        match statement_list_item(node) {
            Ok(item) => statement_list_items.push(item),
            Err(_) => continue
        }
    }

    Program(statement_list_items)
}
