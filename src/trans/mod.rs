use syntax::ast::*;
use syntax::intern::Name;

pub fn transpile(program: &Program) -> String {
    let mut buf = "".to_owned();

    for item in &program.0 {
        buf.push_str(&transpile_statement_list_item(&item))
    }

    buf
}

pub fn transpile_expression(expr: &Expression) -> String {
    match expr {
        &Expression::Literal(ref lit) => transpile_literal(&lit),
        &Expression::Identifier(name) => transpile_ident(name),
    }
}

fn transpile_declarator(dec: &VariableDeclarator) -> String {
    match dec.init {
        Some(ref initializer) => format!("var {} = {}", dec.id, transpile_expression(&initializer)),
        None => format!("var {}", dec.id)
    }
}

fn transpile_variable_declaration(dec: &VariableDeclaration) -> String {
    let declarations: Vec<String> = dec.declarations.iter().map(transpile_declarator).collect();
    declarations.join(";")
}

fn transpile_function_declaration(fun: &FunctionDeclaration) -> String {
    let body = transpile_block(&fun.body);

    match fun.id {
        Some(n) => format!("function {}() {{ {} }}", n.to_string(), body),
        None => format!("function() {{  }}")
    }
}

fn transpile_statement(statement: &Statement) -> String {
    match statement {
        &Statement::Expression(ref e) => transpile_expression(&e),
        &Statement::VariableDeclaration(ref dec) => transpile_variable_declaration(&dec),
        &Statement::FunctionDeclaration(ref dec) => transpile_function_declaration(&dec)
    }
}

fn transpile_statement_list_item(item: &StatementListItem) -> String {
    match item {
        &StatementListItem::Statement(ref statement) => transpile_statement(&statement),
        &StatementListItem::Declaration => panic!("How do I transpile a declaration")
    }
}

fn transpile_block(block: &Block) -> String {
    let watness: Vec<String> = block.0.iter().map(transpile_statement_list_item).collect();
    watness.join("\n")
}

fn transpile_literal(lit: &Literal) -> String {
    match lit {
        &Literal::Number(num) => format!("{}", num),
        &Literal::String(s) => format!("\"{}\"", s.to_string())
    }
}

fn transpile_ident(ident: Name) -> String {
    ident.to_string().to_owned()
}
