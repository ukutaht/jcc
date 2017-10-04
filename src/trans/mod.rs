use syntax::ast::*;
use std::io::{Write, Result};
use interner;

pub fn transpile<W: Write>(out: &mut W, program: &Program) -> Result<()> {
    for item in &program.0 {
        try!(transpile_statement_list_item(out, item))
    }
    write!(out, "\n")?;
    Ok(())
}

pub fn transpile_expression<W: Write>(out: &mut W, expr: &Expression) -> Result<()> {
    match *expr {
        Expression::Literal(_, ref lit) => transpile_literal(out, lit),
        Expression::Identifier(_, name) => transpile_ident(out, &*interner::resolve(name)),
        Expression::Array(_, ref elements) => transpile_array(out, elements),
        Expression::Call(_, ref callee, ref arguments) => transpile_call(out, &*callee, arguments),
        Expression::New(_, ref callee, ref arguments) => transpile_new(out, &*callee, arguments),
        Expression::Binary(_, ref op, ref left, ref right) => transpile_binop(out, op, &*left, &*right),
        Expression::Logical(_, ref op, ref left, ref right) => transpile_logop(out, op, &*left, &*right),
        Expression::StaticMember(_, ref object, property) => transpile_static_member(out, &*object, &*interner::resolve(property)),
        Expression::Unary(_, ref op, ref expr) => transpile_unary_operator(out, op, &*expr),
        Expression::Function(_, ref func) => transpile_function(out, func),
        ref e => panic!("Cannot trans: {:?}", e)
    }
}

fn transpile_unary_operator<W: Write>(out: &mut W, operator: &UnOp, expr: &Expression) -> Result<()> {
    match *operator {
        UnOp::Not => write!(out, "!")?,
        UnOp::Minus => write!(out, "-")?,
        UnOp::Plus => write!(out, "+")?,
        UnOp::Tilde => write!(out, "~")?,
        UnOp::Void => write!(out, "void ")?,
        UnOp::Delete => write!(out, "delete ")?,
        UnOp::Typeof => write!(out, "typeof ")?,
    }
    transpile_expression(out, expr)
}

fn transpile_static_member<W: Write>(out: &mut W, base: &Expression, property: &str) -> Result<()> {
    try!(transpile_expression(out, base));
    write!(out, ".{}", property)
}

fn transpile_binop<W: Write>(out: &mut W, op: &BinOp, left: &Expression, right: &Expression) -> Result<()> {
    try!(transpile_expression(out, left));
    match *op {
        BinOp::Plus => write!(out, " + ")?,
        BinOp::Minus => write!(out, " - ")?,
        BinOp::Times => write!(out, " * ")?,
        BinOp::Div => write!(out, " / ")?,
        BinOp::BitXor => write!(out, " ^ ")?,
        BinOp::BitAnd => write!(out, " & ")?,
        BinOp::BitOr => write!(out, " | ")?,
        BinOp::LShift => write!(out, " << ")?,
        BinOp::RShift => write!(out, " >> ")?,
        BinOp::URShift => write!(out, " >>> ")?,
        BinOp::Mod => write!(out, " % ")?,
        BinOp::EqEq => write!(out, " == ")?,
        BinOp::EqEqEq => write!(out, " === ")?,
        BinOp::NotEq => write!(out, " != ")?,
        BinOp::NotEqEq => write!(out, " !== ")?,
        BinOp::Lt => write!(out, " < ")?,
        BinOp::Lte => write!(out, " <= ")?,
        BinOp::Gt => write!(out, " > ")?,
        BinOp::Gte => write!(out, " >= ")?,
        BinOp::In => write!(out, " in ")?,
        BinOp::Instanceof => write!(out, " instanceof ")?,
    }
    transpile_expression(out, right)
}

fn transpile_logop<W: Write>(out: &mut W, op: &LogOp, left: &Expression, right: &Expression) -> Result<()> {
    try!(transpile_expression(out, left));
    match *op {
        LogOp::AndAnd => write!(out, " && ")?,
        LogOp::OrOr => write!(out, " || ")?
    }
    transpile_expression(out, right)
}

fn transpile_call<W: Write>(out: &mut W,
                            callee: &Expression,
                            arguments: &[ArgumentListElement])
                            -> Result<()> {
    transpile_expression(out, callee)?;
    transpile_arguments(out, arguments)
}

fn transpile_new<W: Write>(out: &mut W,
                            callee: &Expression,
                            arguments: &[ArgumentListElement])
                            -> Result<()> {
    write!(out, "new ")?;
    transpile_expression(out, callee)?;
    transpile_arguments(out, arguments)
}

fn transpile_arguments<W: Write>(out: &mut W, arguments: &[ArgumentListElement]) -> Result<()> {
    try!(write!(out, "("));
    for (idx, arg) in arguments.iter().enumerate() {
        match *arg {
            ArgumentListElement::Expression(ref e) => try!(transpile_expression(out, e)),
            _ => unimplemented!()
        }

        if idx != arguments.len() - 1 {
            try!(write!(out, ", "))
        }
    }
    write!(out, ")")
}

fn transpile_array<W: Write>(out: &mut W, elements: &[Option<ArgumentListElement>]) -> Result<()> {
    try!(write!(out, "["));
    for (idx, element) in elements.iter().enumerate() {
        match *element {
            Some(ArgumentListElement::Expression(ref e)) => transpile_expression(out, e)?,
            Some(_) => unimplemented!(),
            None => write!(out, "null")?
        }
        if idx != elements.len() - 1 {
            try!(write!(out, ", "))
        }
    }
    write!(out, "]")
}

fn transpile_declarator<W: Write>(out: &mut W, dec: &VariableDeclarator) -> Result<()> {
    match dec.init {
        Some(ref initializer) => {
            write!(out, "var ")?;
            transpile_pattern(out, &dec.id)?;
            write!(out, " = ")?;
            transpile_expression(out, initializer)
        }
        None => {
            write!(out, "var ")?;
            transpile_pattern(out, &dec.id)
        }
    }
}

fn transpile_variable_declaration<W: Write>(out: &mut W, dec: &VariableDeclaration) -> Result<()> {
    for declarator in &dec.declarations {
        try!(transpile_declarator(out, declarator))
    }
    Ok(())
}

fn transpile_pattern<W: Write>(out: &mut W, pat: &Pattern<Id>) -> Result<()> {
    match *pat {
        Pattern::Simple(ref id) => {
            write!(out, "{}", &*interner::resolve(id.1))?
        }
        _ => unimplemented!()
    }
    Ok(())
}

fn transpile_function<W: Write>(out: &mut W, fun: &Function) -> Result<()> {
    match fun.id {
        Some(id) => {
            write!(out, "function {}(", &*interner::resolve(id))?
        }
        None => write!(out, "function(")?,
    }

    for (idx, parameter) in fun.parameters.iter().enumerate() {
        transpile_pattern(out, parameter)?;
        if idx != fun.parameters.len() - 1 {
            try!(write!(out, ", "))
        }
    }

    try!(write!(out, ") "));
    transpile_block(out, &fun.body)
}

fn transpile_statement<W: Write>(out: &mut W, statement: &Statement) -> Result<()> {
    match *statement {
        Statement::Expression(_, ref e) => transpile_expression(out, e),
        Statement::Directive(_, ref e, _) => transpile_expression(out, e),
        Statement::VariableDeclaration(_, ref dec) => transpile_variable_declaration(out, dec),
        Statement::FunctionDeclaration(ref dec) => transpile_function(out, dec),
        Statement::Block(_, ref b) => transpile_block(out, b),
        Statement::If(ref e, ref then, ref alternate) => transpile_if(out, e, then, alternate),
        _ => panic!("Unknown statement")
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
        StatementListItem::Statement(ref statement) => transpile_statement(out, statement)?,
        StatementListItem::Declaration => panic!("How do I transpile a declaration"),
    }

    write!(out, ";")
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
        Literal::String(raw, _) => write!(out, "{}", &*interner::resolve(raw)),
        Literal::Null => write!(out, "null"),
        Literal::True => write!(out, "true"),
        Literal::False => write!(out, "false"),
        _ => unimplemented!()
    }
}

fn transpile_ident<W: Write>(out: &mut W, ident: &str) -> Result<()> {
    write!(out, "{}", ident)
}
