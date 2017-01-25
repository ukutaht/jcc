use syntax::ast::*;
use syntax::intern::Name;

pub fn transpile(expr: &Expression) -> String {
    match expr {
        &Expression::Assign(ref ty, ref left, ref right) => transpile_assign(&ty, &left, &right),
        &Expression::Literal(ref lit) => transpile_literal(&lit),
        &Expression::Identifier(name) => transpile_ident(name),
        &Expression::Function(name, ref block) => transpile_function(name, &block)
    }
}

fn transpile_assign(ty: &AssignmentType, left: &Expression, right: &Expression) -> String {
    let assign_word = match ty {
        &AssignmentType::Var => "var",
        &AssignmentType::Let => "let",
        &AssignmentType::Const => "const",
    };

    format!("{} {} = {}", assign_word, transpile(left), transpile(right))
}

fn transpile_function(name: Option<Name>, block: &Block) -> String {
    let block_content = transpile_block(block);

    match name {
        Some(n) => format!("function {}() {{ {} }}", n.to_string(), block_content),
        None => format!("function() {{  }}")
    }
}

fn transpile_declarator(dec: &VariableDeclarator) -> String {
    match dec.init {
        Some(ref initializer) => format!("var {} = {}", dec.id, transpile(&initializer)),
        None => format!("var {}", dec.id)
    }
}

fn transpile_variable_declaration(dec: &VariableDeclaration) -> String {
    let declarations: Vec<String> = dec.declarations.iter().map(transpile_declarator).collect();
    declarations.join(";")
}

fn transpile_statement(statement: &Statement) -> String {
    match statement {
        &Statement::Expression(ref e) => transpile(&e),
        &Statement::VariableDeclaration(ref dec) => transpile_variable_declaration(&dec)
    }
}

fn transpile_block(block: &Block) -> String {
    let watness: Vec<String> = block.0.iter().map(|item|
      match item {
        &StatementListItem::Statement(ref statement) => transpile_statement(&statement),
        &StatementListItem::Declaration => panic!("How do I transpile a declaration")
      }
    ).collect();
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
