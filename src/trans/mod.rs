use syntax::ast::{AssignmentType, Expression, Literal};
use syntax::intern::Name;

pub fn transpile(expr: Expression) -> String {
    match expr {
        Expression::Assign(ty, left, right) => transpile_assign(ty, *left, *right),
        Expression::Literal(lit) => transpile_literal(lit),
        Expression::Identifier(name) => transpile_ident(name)
    }
}

fn transpile_assign(ty: AssignmentType, left: Expression, right: Expression) -> String {
    let assign_word = match ty {
        AssignmentType::Var => "var",
        AssignmentType::Let => "let",
        AssignmentType::Const => "const",
    };

    format!("{} {} = {}", assign_word, transpile(left), transpile(right))
}

fn transpile_literal(lit: Literal) -> String {
    match lit {
        Literal::Number(num) => format!("{}", num),
        Literal::String(s) => format!("\"{}\"", s.to_string())
    }
}

fn transpile_ident(ident: Name) -> String {
    ident.to_string().to_owned()
}
