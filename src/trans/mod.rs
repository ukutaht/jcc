use syntax::ast::*;
use syntax::intern::Name;
use std::io::{Write, Result};


pub fn transpile<W: Write>(out: &mut W, program: &Program) -> Result<()> {
    for item in program.0.iter() {
        try!(transpile_statement_list_item(out, item))
    }
    Ok(())
}

pub fn transpile_expression<W: Write>(out: &mut W, expr: &Expression) -> Result<()> {
    match *expr {
        Expression::Literal(ref lit) => transpile_literal(out, lit),
        Expression::Identifier(name) => transpile_ident(out, name),
    }
}

fn transpile_declarator<W: Write>(out: &mut W, dec: &VariableDeclarator) -> Result<()> {
    match dec.init {
        Some(ref initializer) => {
            try!(write!(out, "var {} = ", dec.id));
            transpile_expression(out, &initializer)
        },
        None => write!(out, "var {}", dec.id),
    }
}

fn transpile_variable_declaration<W: Write>(out: &mut W, dec: &VariableDeclaration) -> Result<()> {
    for declarator in dec.declarations.iter() {
        try!(transpile_declarator(out, declarator))
    }
    Ok(())
}

fn transpile_function_declaration<W: Write>(out: &mut W, fun: &FunctionDeclaration) -> Result<()> {
    match fun.id {
        Some(n) => {
            try!(write!(out, "function {}() {{ ", n.to_string()));
            try!(transpile_block(out, &fun.body));
            write!(out, " }}")
        }
        None => write!(out, "function() {{  }}"),
    }
}

fn transpile_statement<W: Write>(out: &mut W, statement: &Statement) -> Result<()> {
    match *statement {
        Statement::Expression(ref e) => transpile_expression(out, e),
        Statement::VariableDeclaration(ref dec) => transpile_variable_declaration(out, dec),
        Statement::FunctionDeclaration(ref dec) => transpile_function_declaration(out, dec),
    }
}

fn transpile_statement_list_item<W: Write>(out: &mut W, item: &StatementListItem) -> Result<()> {
    match *item {
        StatementListItem::Statement(ref statement) => transpile_statement(out, statement),
        StatementListItem::Declaration => panic!("How do I transpile a declaration"),
    }
}

fn transpile_block<W: Write>(out: &mut W, block: &Block) -> Result<()> {
    for item in block.0.iter() {
        try!(transpile_statement_list_item(out, item))
    }
    Ok(())
}

fn transpile_literal<W: Write>(out: &mut W, lit: &Literal) -> Result<()> {
    match *lit {
        Literal::Number(num) => write!(out, "{}", num),
        Literal::String(s) => write!(out, "\"{}\"", s.to_string()),
    }
}

fn transpile_ident<W: Write>(out: &mut W, ident: Name) -> Result<()> {
    write!(out, "{}", ident.to_string())
}
