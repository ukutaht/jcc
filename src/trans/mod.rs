use syntax::ast::Expression;

pub fn transpile(expr: Expression) -> String {
    format!("{:?}", expr)
}
