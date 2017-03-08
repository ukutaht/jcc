extern crate jcc;

use jcc::syntax::ast::*;
use serde_json::value::Value;

fn expression(node: &Value) -> Option<Expression> {
    let obj = node.as_object().unwrap();
    match obj.get("type").unwrap().as_str().unwrap() {
        "FunctionExpression" => {
            None
        },
        "Identifier" => {
            None
        }
        _ => None
    }
}

fn statement(node: &Value) -> Option<Statement> {
    let obj = node.as_object().unwrap();
    match obj.get("type").unwrap().as_str().unwrap() {
        "ExpressionStatement" => {
            expression(obj.get("expression").unwrap()).map(Statement::Expression)
        }
        _ => None
    }
}

fn statement_list_item(node: &Value) -> Option<StatementListItem> {
    statement(node).map(StatementListItem::Statement)
}

pub fn parse_program(value: &Value) -> Program {
    let body = value.get("body").unwrap().as_array().unwrap();
    let mut statement_list_items = Vec::new();

    for node in body {
        match statement_list_item(node) {
            Some(item) => statement_list_items.push(item),
            None => continue
        }
    }

    Program(statement_list_items)
}
