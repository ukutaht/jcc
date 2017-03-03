use syntax::ast::*;
use syntax::intern::Name;
use std::io::{Write, Result};


pub fn transpile<W: Write>(out: &mut W, program: &Program) -> Result<()> {
    for item in &program.0 {
        try!(transpile_statement_list_item(out, item))
    }
    Ok(())
}

pub fn transpile_expression<W: Write>(out: &mut W, expr: &Expression) -> Result<()> {
    match *expr {
        Expression::Literal(ref lit) => transpile_literal(out, lit),
        Expression::Identifier(name) => transpile_ident(out, name),
        Expression::Array(ref elements) => transpile_array(out, elements),
        Expression::Call(ref callee, ref arguments) => transpile_call(out, &*callee, arguments),
        Expression::Binary(ref op, ref left, ref right) => transpile_binop(out, op, &*left, &*right)
    }
}

fn transpile_binop<W: Write>(out: &mut W, op: &BinOp, left: &Expression, right: &Expression) -> Result<()> {
    try!(transpile_expression(out, left));
    match *op {
        BinOp::Plus => write!(out, " + ")?
    }
    transpile_expression(out, right)
}

fn transpile_call<W: Write>(out: &mut W,
                            callee: &Expression,
                            arguments: &[ArgumentListElement])
                            -> Result<()> {
    try!(transpile_expression(out, callee));
    try!(write!(out, "("));
    for (idx, arg) in arguments.iter().enumerate() {
        match *arg {
            ArgumentListElement::Expression(ref e) => try!(transpile_expression(out, e)),
        }

        if idx != arguments.len() - 1 {
            try!(write!(out, ", "))
        }
    }
    write!(out, ")")
}

fn transpile_array<W: Write>(out: &mut W, elements: &[Expression]) -> Result<()> {
    try!(write!(out, "["));
    for (idx, element) in elements.iter().enumerate() {
        try!(transpile_expression(out, element));
        if idx != elements.len() - 1 {
            try!(write!(out, ", "))
        }
    }
    write!(out, "]")
}

fn transpile_declarator<W: Write>(out: &mut W, dec: &VariableDeclarator) -> Result<()> {
    match dec.init {
        Some(ref initializer) => {
            try!(write!(out, "var {} = ", dec.id));
            transpile_expression(out, initializer)
        }
        None => write!(out, "var {}", dec.id),
    }
}

fn transpile_variable_declaration<W: Write>(out: &mut W, dec: &VariableDeclaration) -> Result<()> {
    for declarator in &dec.declarations {
        try!(transpile_declarator(out, declarator))
    }
    Ok(())
}

fn transpile_function_parameter<W: Write>(out: &mut W, pat: &Pattern) -> Result<()> {
    match *pat {
        Pattern::Identifier(ref n) => {
            try!(write!(out, "{}", n.to_string()))
        }
    }
    Ok(())
}

fn transpile_function_declaration<W: Write>(out: &mut W, fun: &FunctionDeclaration) -> Result<()> {
    let name = fun.id.map(|n| n.to_string()).unwrap_or("");

    try!(write!(out, "function {}(", name));
    for (idx, parameter) in fun.parameters.iter().enumerate() {
        try!(transpile_function_parameter(out, parameter));
        if idx != fun.parameters.len() - 1 {
            try!(write!(out, ", "))
        }
    }

    try!(write!(out, ") "));
    transpile_block(out, &fun.body)
}

fn transpile_statement<W: Write>(out: &mut W, statement: &Statement) -> Result<()> {
    match *statement {
        Statement::Expression(ref e) => transpile_expression(out, e),
        Statement::VariableDeclaration(ref dec) => transpile_variable_declaration(out, dec),
        Statement::FunctionDeclaration(ref dec) => transpile_function_declaration(out, dec),
        Statement::Block(ref b) => transpile_block(out, b),
        Statement::If(ref e, ref then, ref alternate) => transpile_if(out, e, then, alternate)
    }
}

fn transpile_if<W: Write>(out: &mut W, test: &Expression, then: &Statement, alternate: &Option<Box<Statement>>) -> Result<()> {
    write!(out, "if (")?;
    transpile_expression(out, test)?;
    write!(out, ") ")?;
    transpile_statement(out, then)?;
    match *alternate {
        Some(ref stmt) => {
            write!(out, " else ")?;
            transpile_statement(out, &*stmt)
        },
        None => Ok(())
    }
}

fn transpile_statement_list_item<W: Write>(out: &mut W, item: &StatementListItem) -> Result<()> {
    match *item {
        StatementListItem::Statement(ref statement) => transpile_statement(out, statement),
        StatementListItem::Declaration => panic!("How do I transpile a declaration"),
    }
}

fn transpile_block<W: Write>(out: &mut W, block: &Block) -> Result<()> {
    write!(out, "{{ ")?;
    for item in &block.0 {
        try!(transpile_statement_list_item(out, item))
    }
    write!(out, " }}")
}

fn transpile_literal<W: Write>(out: &mut W, lit: &Literal) -> Result<()> {
    match *lit {
        Literal::Number(num) => write!(out, "{}", num),
        Literal::String(s) => write!(out, "{}", s.to_string()),
    }
}

fn transpile_ident<W: Write>(out: &mut W, ident: Name) -> Result<()> {
    write!(out, "{}", ident.to_string())
}
