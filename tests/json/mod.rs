use std;
use jcc::syntax::span::{Span, Position};
use jcc::syntax::ast::*;
use jcc::interner;
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

fn maybe<T>(value: &Value, reader: &Fn(&Value) -> Result<T>) -> Result<Option<T>> {
    if value.is_null() {
        Ok(None)
    } else {
        Ok(Some(reader(value)?))
    }
}

fn maybe_key<T>(node: &Value, key: &str, reader: &Fn(&Value) -> Result<T>) -> Result<Option<T>> {
    maybe(expect_value(node, key), reader)
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

fn assign_prop_pattern(node: &Value) -> Result<PropPattern<AssignTarget>> {
    let key = prop_key(false, expect_value(node, "key"))?;
    let value = assign_target(expect_value(node, "value"))?;

    Ok(PropPattern {
        span: span(node)?,
        key: key,
        value: value,
        shorthand: expect_bool(node, "shorthand")
    })
}

fn assign_target(node: &Value) -> Result<Pattern<AssignTarget>> {
    match expect_string(node, "type") {
        "Identifier" => {
            let id = interner::intern(expect_string(node, "name"));
            Ok(Pattern::Simple(AssignTarget::Id(Id(span(node)?, id))))
        }
        "AssignmentPattern" => {
            let left = assign_target(expect_value(node, "left"))?;
            let right = expression(expect_value(node, "right"))?;
            Ok(Pattern::Assignment(span(node)?, Box::new(left), right))
        }
        "RestElement" => {
            let arg = assign_target(expect_value(node, "argument"))?;
            Ok(Pattern::RestElement(span(node)?, Box::new(arg)))
        }
        "ArrayPattern" => {
            let mut elements = Vec::new();
            for element in expect_array(node, "elements") {
                elements.push(maybe(element, &assign_target)?);
            };
            Ok(Pattern::Array(span(node)?, elements))
        }
        "ObjectPattern" => {
            let mut props = Vec::new();
            for prop in expect_array(node, "properties") {
                props.push(assign_prop_pattern(prop)?);
            };
            Ok(Pattern::Object(span(node)?, props))
        }
        "MemberExpression" => {
            if expect_bool(node, "computed") {
                let left = expression(expect_value(node, "object"))?;
                let right = expression(expect_value(node, "property"))?;
                Ok(Pattern::Simple(AssignTarget::ComputedMember(span(node)?, left, right)))
            } else {
                let left = expression(expect_value(node, "object"))?;
                let right = interner::intern(expect_string(expect_value(node, "property"), "name"));
                Ok(Pattern::Simple(AssignTarget::StaticMember(span(node)?, left, right)))
            }
        }
        _ => Err(())
    }
}

fn assignment_expression(node: &Value) -> Result<Expression> {
    let left = assign_target(expect_value(node, "left"))?;
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

fn argument_list_item(node: &Value) -> Result<ArgumentListElement> {
    match expect_string(node, "type") {
        "SpreadElement" => {
            let expr = expression(expect_value(node, "argument"))?;
            Ok(ArgumentListElement::SpreadElement(span(node)?, expr))
        }
        _ => {
            expression(node).map(ArgumentListElement::Expression)
        }
    }
}

fn array_expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;
    let mut elements = Vec::new();

    for element in expect_array(node, "elements") {
        elements.push(maybe(element, &argument_list_item)?);
    }

    Ok(Expression::Array(span, elements))
}

fn new_expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;
    let callee = expression(expect_value(node, "callee"))?;
    let mut arguments = Vec::new();

    for argument in expect_array(node, "arguments") {
        arguments.push(argument_list_item(argument)?);
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
            Expression::Identifier(_, s) => {
                Ok(Expression::StaticMember(span, Box::new(object), s))
            }
            _ => Err(())
        }
    }
}

fn literal(node: &Value) -> Result<Expression> {
    if node.get("regex").is_some() {
        let regex = expect_value(node, "regex");
        let pattern = interner::intern(expect_string(regex, "pattern"));
        let flags = expect_string(regex, "flags").chars().collect();
        return Ok(Expression::Literal(span(node)?, Literal::Regex(pattern, flags)));
    };

    let val = expect_value(node, "value");

    if val.is_number() {
        Ok(Expression::Literal(span(node)?, Literal::Number(val.as_f64().unwrap())))
    } else if val.is_null() {
        Ok(Expression::Literal(span(node)?, Literal::Null))
    } else if val.is_string() {
        let string = expect_string(node, "raw");
        let sym = interner::intern(string);
        Ok(Expression::Literal(span(node)?, Literal::String(sym)))
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
        arguments.push(argument_list_item(argument)?);
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

fn prop_key(computed: bool, node: &Value) -> Result<PropKey> {
    if computed {
        let key = expression(node)?;
        return Ok(PropKey::Computed(key))
    };

    match expect_string(node, "type") {
        "Identifier" => Ok(PropKey::Identifier(span(node)?, interner::intern(expect_string(node, "name")))),
        "Literal" => {
            let val = expect_value(node, "value");

            if val.is_string() {
                let string = expect_string(node, "raw");
                Ok(PropKey::String(span(node)?, interner::intern(string)))
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
            let key = prop_key(false, expect_value(node, "key"))?;

            if expect_bool(node, "method") {
                let value = function(expect_value(node, "value"))?;
                Ok(Prop::Method(span(node)?, key, value))
            } else if expect_bool(node, "shorthand") {
                Ok(Prop::Shorthand(span(node)?, identifier(expect_value(node, "key"))?))
            } else {
                let value = expression(expect_value(node, "value"))?;
                Ok(Prop::Init(span(node)?, key, value))
            }
        },
        "get" => {
            let key = prop_key(false, expect_value(node, "key"))?;
            let value = function(expect_value(node, "value"))?;
            Ok(Prop::Get(span(node)?, key, value))
        },
        "set" => {
            let key = prop_key(false, expect_value(node, "key"))?;
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
        let name = expect_string(expect_value(node, "id"), "name");
        Some(interner::intern(name))

    };
    let mut parameters = Vec::new();
    for param in expect_array(node, "params") {
        parameters.push(pattern(param)?)
    }
    Ok(Function { id: id, parameters: parameters, body: body })
}

fn prop_pattern(node: &Value) -> Result<PropPattern<Id>> {
    let key = prop_key(false, expect_value(node, "key"))?;
    let value = pattern(expect_value(node, "value"))?;

    Ok(PropPattern {
        span: span(node)?,
        key: key,
        value: value,
        shorthand: expect_bool(node, "shorthand")
    })
}

fn pattern(node: &Value) -> Result<Pattern<Id>> {
    match expect_string(node, "type") {
        "Identifier" => {
            let id = interner::intern(expect_string(node, "name"));
            Ok(Pattern::Simple(Id(span(node)?, id)))
        }
        "AssignmentPattern" => {
            let left = pattern(expect_value(node, "left"))?;
            let right = expression(expect_value(node, "right"))?;
            Ok(Pattern::Assignment(span(node)?, Box::new(left), right))
        }
        "RestElement" => {
            let arg = pattern(expect_value(node, "argument"))?;
            Ok(Pattern::RestElement(span(node)?, Box::new(arg)))
        }
        "ArrayPattern" => {
            let mut elements = Vec::new();
            for element in expect_array(node, "elements") {
                elements.push(maybe(element, &pattern)?);
            };
            Ok(Pattern::Array(span(node)?, elements))
        }
        "ObjectPattern" => {
            let mut props = Vec::new();
            for prop in expect_array(node, "properties") {
                props.push(prop_pattern(prop)?)
            };
            Ok(Pattern::Object(span(node)?, props))
        }
        _ => Err(())
    }
}

fn class_expression(node: &Value) -> Result<Expression> {
    let decl = class(node).map(Box::new)?;
    Ok(Expression::Class(span(node)?, decl))
}

fn arrow_function_expression(node: &Value) -> Result<Expression> {
    let body = if expect_bool(node, "expression") {
        ArrowFunctionBody::Expression(Box::new(expression(expect_value(node, "body"))?))
    } else {
        ArrowFunctionBody::Block(block(expect_value(node, "body"))?)
    };

    let mut parameters = Vec::new();
    for param in expect_array(node, "params") {
        parameters.push(pattern(param)?)
    }

    let fun = ArrowFunction { body, parameters };
    Ok(Expression::ArrowFunction(span(node)?, fun))
}

fn expression(node: &Value) -> Result<Expression> {
    let span = span(node)?;

    match expect_string(node, "type") {
        "AssignmentExpression" => assignment_expression(node),
        "ArrayExpression" => array_expression(node),
        "ArrowFunctionExpression" => arrow_function_expression(node),
        "BinaryExpression" => binary_expression(node),
        "UnaryExpression" => unary_expression(node),
        "UpdateExpression" => update_expression(node),
        "CallExpression" => call_expression(node),
        "Identifier" => {
            let sym = interner::intern(expect_string(node, "name"));
            Ok(Expression::Identifier(span, sym))
        }
        "Literal" => literal(node),
        "LogicalExpression" => logical_expression(node),
        "MemberExpression" => member_expression(node),
        "NewExpression" => new_expression(node),
        "ThisExpression" => Ok(Expression::This(span)),
        "ObjectExpression" => object_expression(node),
        "ConditionalExpression" => conditional_expression(node),
        "SequenceExpression" => sequence_expression(node),
        "FunctionExpression" => function_expression(node),
        "ClassExpression" => class_expression(node),
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
    let alternate = maybe_key(node, "alternate", &statement)?.map(Box::new);

    Ok(Statement::If(test, Box::new(consequent), alternate))
}

fn variable_declarator(node: &Value) -> Result<VariableDeclarator> {
    let init = maybe_key(node, "init", &expression)?;

    Ok(VariableDeclarator {
        id: pattern(expect_value(node, "id"))?,
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
    let param = pattern(expect_value(node, "param"))?;
    let body = block(expect_value(node, "body"))?;
    Ok(CatchClause { param: param, body: body })
}

fn try_statement(node: &Value) -> Result<Statement> {
    let body = block(expect_value(node, "block"))?;
    let handler = maybe_key(node, "handler", &catch_clause)?;
    let finalizer = maybe_key(node, "finalizer", &block)?;

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

fn do_while_statement(node: &Value) -> Result<Statement> {
    let body = statement(expect_value(node, "body"))?;
    let test = expression(expect_value(node, "test"))?;

    Ok(Statement::DoWhile(span(node)?, Box::new(body), test))
}

fn while_statement(node: &Value) -> Result<Statement> {
    let test = expression(expect_value(node, "test"))?;
    let body = statement(expect_value(node, "body"))?;

    Ok(Statement::While(span(node)?, test, Box::new(body)))
}

fn for_init(node: &Value) -> Result<ForInit> {
    if expect_string(node, "type") == "VariableDeclaration" {
        Ok(ForInit::VarDecl(variable_declaration(node)?))
    } else {
        Ok(ForInit::Expression(expression(node)?))
    }
}

fn for_op_init(node: &Value) -> Result<ForOpInit> {
    if expect_string(node, "type") == "VariableDeclaration" {
        Ok(ForOpInit::VarDecl(variable_declaration(node)?))
    } else {
        Ok(ForOpInit::Pattern(assign_target(node)?))
    }
}

fn for_statement(node: &Value) -> Result<Statement> {
    let init = maybe_key(node, "init", &for_init)?;
    let test = maybe_key(node, "test", &expression)?;
    let update = maybe_key(node, "update", &expression)?;
    let body = statement(expect_value(node, "body"))?;

    Ok(Statement::For(span(node)?, Box::new(ForStatement {
        init, test, update, body
    })))
}

fn for_op_statement(node: &Value) -> Result<ForOpStatement> {
    let left = for_op_init(expect_value(node, "left"))?;
    let right = expression(expect_value(node, "right"))?;
    let body = statement(expect_value(node, "body"))?;

    Ok(ForOpStatement {left, right, body})
}

fn for_in_statement(node: &Value) -> Result<Statement> {
    let stmt = for_op_statement(node)?;
    Ok(Statement::ForIn(span(node)?, Box::new(stmt)))
}

fn for_of_statement(node: &Value) -> Result<Statement> {
    let stmt = for_op_statement(node)?;
    Ok(Statement::ForOf(span(node)?, Box::new(stmt)))
}

fn with_statement(node: &Value) -> Result<Statement> {
    let object = expression(expect_value(node, "object"))?;
    let body = statement(expect_value(node, "body"))?;

    Ok(Statement::With(span(node)?, object, Box::new(body)))
}

fn labeled_statement(node: &Value) -> Result<Statement> {
    let id = identifier(expect_value(node, "label"))?;
    let body = statement(expect_value(node, "body"))?;

    Ok(Statement::Labeled(span(node)?, id, Box::new(body)))
}

fn identifier(node: &Value) -> Result<Id> {
    let name = expect_string(node, "name");
    Ok(Id(span(node)?, interner::intern(name)))
}

fn method_def_kind(kind: &str) -> Result<MethodDefinitionKind> {
    match kind {
        "constructor" => Ok(MethodDefinitionKind::Constructor),
        "method" => Ok(MethodDefinitionKind::Method),
        "get" => Ok(MethodDefinitionKind::Get),
        "set" => Ok(MethodDefinitionKind::Set),
        _ => Err(())
    }
}

fn method(node: &Value) -> Result<MethodDefinition> {
    Ok(MethodDefinition {
        loc: span(node)?,
        key: prop_key(expect_bool(node, "computed"), expect_value(node, "key"))?,
        value: function(expect_value(node, "value"))?,
        is_static: expect_bool(node, "static"),
        kind: method_def_kind(expect_string(node, "kind"))?
    })
}

fn class(node: &Value) -> Result<ClassDecl> {
    let id = maybe_key(node, "id", &identifier)?;
    let super_class = maybe_key(node, "superClass", &expression)?;
    let mut body = Vec::new();
    for m in expect_array(expect_value(node, "body"), "body") {
        body.push(method(m)?)
    };

    Ok(ClassDecl { id, super_class, body })
}

fn statement(node: &Value) -> Result<Statement> {
    match expect_string(node, "type") {
        "ExpressionStatement" => {
            let expr = expression(expect_value(node, "expression"))?;
            if node.get("directive").is_some() {
                let name = expect_string(node, "directive").to_owned();
                Ok(Statement::Directive(span(node)?, expr, name))
            } else {
                Ok(Statement::Expression(span(node)?, expr))
            }
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
        "FunctionDeclaration" => function(node).map(Statement::FunctionDeclaration),
        "ClassDeclaration" => {
            let class = class(node)?;
            Ok(Statement::ClassDeclaration(span(node)?, class))
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
        "DoWhileStatement" => do_while_statement(node),
        "WhileStatement" => while_statement(node),
        "ForStatement" => for_statement(node),
        "ForInStatement" => for_in_statement(node),
        "ForOfStatement" => for_of_statement(node),
        "WithStatement" => with_statement(node),
        "LabeledStatement" => labeled_statement(node),
        "BreakStatement" => {
            let id = maybe_key(node, "label", &identifier)?;
            Ok(Statement::Break(span(node)?, id))
        }
        "ContinueStatement" => {
            let id = maybe_key(node, "label", &identifier)?;
            Ok(Statement::Continue(span(node)?, id))
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
