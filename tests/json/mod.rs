extern crate jcc;

use std;
use jcc::syntax::ast::*;
use jcc::syntax::span::{Span, Position};
use serde_json::value::Value;

pub type Result<T> = std::result::Result<T, ()>;

fn expect_string<'a>(node: &'a Value, key: &str) -> &'a str {
    node.as_object().unwrap().get(key).unwrap().as_str().unwrap()
}

fn expect_u64<'a>(node: &'a Value, key: &str) -> u64 {
    node.as_object().unwrap().get(key).unwrap().as_u64().unwrap()
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

    match expect_string(node, "operator") {
        "==" => Ok(Expression::Binary(span, BinOp::EqEq, Box::new(left), Box::new(right))),
        "===" => Ok(Expression::Binary(span, BinOp::EqEqEq, Box::new(left), Box::new(right))),
        "!=" => Ok(Expression::Binary(span, BinOp::NotEq, Box::new(left), Box::new(right))),
        "!==" => Ok(Expression::Binary(span, BinOp::NotEqEq, Box::new(left), Box::new(right))),
        _ => Err(())
    }
}

fn expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;

    match expect_string(node, "type") {
        "FunctionExpression" => {
            Err(())
        },
        "Identifier" => {
            Ok(Expression::Identifier(span, expect_string(node, "name").to_owned()))
        }
        "BinaryExpression" => {
            binary_expression(node)
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
