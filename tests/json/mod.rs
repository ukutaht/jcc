extern crate jcc;

use std;
use jcc::syntax::ast::*;
use serde_json::value::Value;

pub type Result<T> = std::result::Result<T, ()>;

fn expect_string<'a>(node: &'a Value, key: &str) -> &'a str {
    node.as_object().unwrap().get(key).unwrap().as_str().unwrap()
}

fn expect_value<'a>(node: &'a Value, key: &str) -> &'a Value {
    node.as_object().unwrap().get(key).unwrap()
}

fn binary_expression(node: &Value) -> Result<Expression> {
    match expect_string(node, "operator") {
        "==" => {
            let left = expression(expect_value(node, "left"))?;
            let right = expression(expect_value(node, "right"))?;
            Ok(Expression::Binary(BinOp::EqEq, Box::new(left), Box::new(right)))
        },
        _ => Err(())
    }
}

fn expression(node: &Value) -> Result<Expression> {
    match expect_string(node, "type") {
        "FunctionExpression" => {
            Err(())
        },
        "Identifier" => {
            Ok(Expression::Identifier(expect_string(node, "name").to_owned()))
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
