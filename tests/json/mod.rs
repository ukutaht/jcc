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
        "^" => BinOp::BitXor,
        "&" => BinOp::BitAnd,
        "|" => BinOp::BitOr,
        "<<" => BinOp::LShift,
        ">>" => BinOp::RShift,
        ">>>" => BinOp::URShift,
        "<" => BinOp::Lt,
        "<=" => BinOp::Lte,
        ">" => BinOp::Gt,
        ">=" => BinOp::Gte,
        "in" => BinOp::In,
        "instanceof" => BinOp::Instanceof,
        _ => return Err(())
    };

    Ok(Expression::Binary(span, op, Box::new(left), Box::new(right)))
}

fn assignment_expression(node: &Value) -> Result<Expression> {
    let left = expression(expect_value(node, "left"))?;
    let right = expression(expect_value(node, "right"))?;
    let span = span(node)?;

    let op = match expect_string(node, "operator") {
        "=" => AssignOp::Eq,
        "*=" => AssignOp::TimesEq,
        "/=" => AssignOp::DivEq,
        "%=" => AssignOp::ModEq,
        "+=" => AssignOp::PlusEq,
        "-=" => AssignOp::MinusEq,
        "<<=" => AssignOp::LShiftEq,
        ">>=" => AssignOp::RShiftEq,
        ">>>=" => AssignOp::URShiftEq,
        "&=" => AssignOp::BitAndEq,
        "^=" => AssignOp::BitXorEq,
        "|=" => AssignOp::BitOrEq,
        _ => return Err(())
    };

    Ok(Expression::Assignment(span, op, Box::new(left), Box::new(right)))
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

fn literal(node: &Value) -> Result<Expression> {
    let val = expect_value(node, "value");

    if val.is_number() {
        Ok(Expression::Literal(span(node)?, Literal::Number(val.as_f64().unwrap())))
    } else if val.is_null() {
        Ok(Expression::Literal(span(node)?, Literal::Null))
    } else if val.is_string() {
        Ok(Expression::Literal(span(node)?, Literal::String(val.as_str().unwrap().to_owned())))
    } else if val.is_boolean() {
        if val.as_bool().unwrap() {
            Ok(Expression::Literal(span(node)?, Literal::True))
        } else {
            Ok(Expression::Literal(span(node)?, Literal::False))
        }
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

fn unary_expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;
    let argument = expression(expect_value(node, "argument"))?;

    let op = match expect_string(node, "operator") {
        "-" => UnOp::Minus,
        "+" => UnOp::Plus,
        "!" => UnOp::Not,
        "~" => UnOp::Tilde,
        "void" => UnOp::Void,
        "delete" => UnOp::Delete,
        "typeof" => UnOp::Typeof,
        _ => return Err(())
    };

    Ok(Expression::Unary(span, op, Box::new(argument)))
}

fn update_expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;
    let argument = expression(expect_value(node, "argument"))?;
    let prefix = expect_bool(node, "prefix");

    let op = match expect_string(node, "operator") {
        "++" => UpdateOp::PlusPlus,
        "--" => UpdateOp::MinusMinus,
        _ => return Err(())
    };

    Ok(Expression::Update(span, op, Box::new(argument), prefix))
}

fn prop_key(node: &Value) -> Result<PropKey> {
    match expect_string(node, "type") {
        "Identifier" => Ok(PropKey::Identifier(span(node)?, expect_string(node, "name").to_owned())),
        "Literal" => {
            let val = expect_value(node, "value");

            if val.is_string() {
                Ok(PropKey::String(span(node)?, val.as_str().unwrap().to_owned()))
            } else if val.is_number() {
                Ok(PropKey::Number(span(node)?, val.as_f64().unwrap()))
            } else {
                Err(())
            }
        }
        _ => Err(())
    }
}

fn prop(node: &Value) -> Result<Prop> {
    match expect_string(node, "kind") {
        "init" => {
            let key = prop_key(expect_value(node, "key"))?;
            let value = expression(expect_value(node, "value"))?;
            Ok(Prop::Init(span(node)?, key, value))
        },
        "get" => {
            let key = prop_key(expect_value(node, "key"))?;
            let value = function(expect_value(node, "value"))?;
            Ok(Prop::Get(span(node)?, key, value))
        },
        "set" => {
            let key = prop_key(expect_value(node, "key"))?;
            let value = function(expect_value(node, "value"))?;
            Ok(Prop::Set(span(node)?, key, value))
        },
        _ => Err(())
    }
}

fn object_expression(node: &Value) -> Result<Expression> {
    let mut props = Vec::new();
    for thing in expect_array(node, "properties") {
        props.push(prop(thing)?)
    }

    Ok(Expression::Object(span(node)?, props))
}

fn conditional_expression(node: &Value) -> Result<Expression> {
    let test = expression(expect_value(node, "test"))?;
    let consequent = expression(expect_value(node, "consequent"))?;
    let alternate = expression(expect_value(node, "alternate"))?;
    Ok(Expression::Conditional(span(node)?, Box::new(test), Box::new(consequent), Box::new(alternate)))
}

fn sequence_expression(node: &Value) -> Result<Expression> {
    let mut expressions = Vec::new();
    for val in expect_array(node, "expressions") {
        expressions.push(expression(val)?);
    }
    Ok(Expression::Sequence(span(node)?, expressions))
}

fn function_expression(node: &Value) -> Result<Expression> {
    Ok(Expression::Function(span(node)?, function(node)?))
}

fn block(node: &Value) -> Result<Block> {
    let mut items = Vec::new();
    for value in expect_array(node, "body") {
        items.push(statement_list_item(value)?)
    }

    Ok(Block(items))
}

fn function(node: &Value) -> Result<Function> {
    let body = block(expect_value(node, "body"))?;
    let id = if expect_value(node, "id").is_null() {
        None
    } else {
        Some(expect_string(expect_value(node, "id"), "name").to_owned())

    };
    let mut parameters = Vec::new();
    for param in expect_array(node, "params") {
        parameters.push(pattern(param)?)
    }
    Ok(Function { id: id, parameters: parameters, body: body })
}

fn pattern(node: &Value) -> Result<Pattern> {
    match expect_string(node, "type") {
        "Identifier" => Ok(Pattern::Identifier(expect_string(node, "name").to_owned())),
        _ => Err(())
    }
}

fn expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;

    match expect_string(node, "type") {
        "AssignmentExpression" => assignment_expression(node),
        "ArrayExpression" => array_expression(node),
        "BinaryExpression" => binary_expression(node),
        "UnaryExpression" => unary_expression(node),
        "UpdateExpression" => update_expression(node),
        "CallExpression" => call_expression(node),
        "Identifier" => Ok(Expression::Identifier(span, expect_string(node, "name").to_owned())),
        "Literal" => literal(node),
        "LogicalExpression" => logical_expression(node),
        "MemberExpression" => member_expression(node),
        "NewExpression" => new_expression(node),
        "ThisExpression" => Ok(Expression::This(span)),
        "ObjectExpression" => object_expression(node),
        "ConditionalExpression" => conditional_expression(node),
        "SequenceExpression" => sequence_expression(node),
        "FunctionExpression" => function_expression(node),
        _ => Err(())
    }
}

fn block_statement(node: &Value) -> Result<Statement> {
    let mut statement_list_items = Vec::new();
    for item in expect_array(node, "body") {
        statement_list_items.push(statement_list_item(item)?);
    }

    Ok(Statement::Block(span(node)?, Block(statement_list_items)))
}

fn if_statement(node: &Value) -> Result<Statement> {
    let test = expression(expect_value(node, "test"))?;
    let consequent = statement(expect_value(node, "consequent"))?;
    let alternate = if expect_value(node, "alternate").is_null() {
        None
    } else {
        Some(Box::new(statement(expect_value(node, "alternate"))?))
    };

    Ok(Statement::If(test, Box::new(consequent), alternate))
}

fn variable_declarator(node: &Value) -> Result<VariableDeclarator> {
    let init = if expect_value(node, "init").is_null() {
        None
    } else {
        Some(expression(expect_value(node, "init"))?)
    };

    Ok(VariableDeclarator {
        id: expect_string(expect_value(node, "id"), "name").to_owned(),
        init: init
    })
}

fn variable_declaration(node: &Value) -> Result<VariableDeclaration> {
    let kind = match expect_string(node, "kind") {
        "var" => VariableDeclarationKind::Var,
        "let" => VariableDeclarationKind::Let,
        "const" => VariableDeclarationKind::Const,
        _ => return Err(())
    };
    let mut declarators = Vec::new();
    for declarator in expect_array(node, "declarations") {
        declarators.push(variable_declarator(declarator)?)
    };

    Ok(VariableDeclaration {
        kind: kind,
        declarations: declarators
    })
}

fn throw_statement(node: &Value) -> Result<Statement> {
    let argument = expression(expect_value(node, "argument"))?;
    Ok(Statement::Throw(span(node)?, argument))
}

fn catch_clause(node: &Value) -> Result<CatchClause> {
    let param = expect_string(expect_value(node, "param"), "name").to_owned();
    let body = block(expect_value(node, "body"))?;
    Ok(CatchClause { param: param, body: body })
}

fn try_statement(node: &Value) -> Result<Statement> {
    let body = block(expect_value(node, "block"))?;
    let handler = if expect_value(node, "handler").is_null() {
        None
    } else {
        Some(catch_clause(expect_value(node, "handler"))?)
    };
    let finalizer = if expect_value(node, "finalizer").is_null() {
        None
    } else {
        Some(block(expect_value(node, "finalizer"))?)
    };

    Ok(Statement::Try(span(node)?, body, handler, finalizer))
}

fn switch_case(node: &Value) -> Result<SwitchCase> {
    let test = if expect_value(node, "test").is_null() {
        None
    } else {
        Some(expression(expect_value(node, "test"))?)
    };

    let mut consequent = Vec::new();
    for item in expect_array(node, "consequent") {
        consequent.push(statement_list_item(item)?)
    }

    Ok(SwitchCase { test: test, consequent: Block(consequent) })
}

fn switch_statement(node: &Value) -> Result<Statement> {
    let discriminant = expression(expect_value(node, "discriminant"))?;

    let mut cases = Vec::new();
    for case in expect_array(node, "cases") {
        cases.push(switch_case(case)?)
    }

    Ok(Statement::Switch(span(node)?, discriminant, cases))
}

fn statement(node: &Value) -> Result<Statement> {
    match expect_string(node, "type") {
        "ExpressionStatement" => {
            let expr = expression(expect_value(node, "expression"))?;
            Ok(Statement::Expression(span(node)?, expr))
        }
        "BlockStatement" => {
            block_statement(node)
        }
        "DebuggerStatement" => {
            Ok(Statement::Debugger(span(node)?))
        }
        "EmptyStatement" => {
            Ok(Statement::Empty(span(node)?))
        }
        "IfStatement" => if_statement(node),
        "VariableDeclaration" => {
            Ok(Statement::VariableDeclaration(span(node)?, variable_declaration(node)?))
        }
        "FunctionDeclaration" => {
            Ok(Statement::FunctionDeclaration(function(node)?))
        }
        "ReturnStatement" => {
            let raw_arg = expect_value(node, "argument");
            if raw_arg.is_null() {
                Ok(Statement::Return(span(node)?, None))
            } else {
                let argument = expression(raw_arg)?;
                Ok(Statement::Return(span(node)?, Some(argument)))
            }
        },
        "ThrowStatement" => throw_statement(node),
        "TryStatement" => try_statement(node),
        "SwitchStatement" => switch_statement(node),
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
