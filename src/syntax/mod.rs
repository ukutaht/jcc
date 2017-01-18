pub mod ast;
pub mod intern;
mod parser;
mod char;

pub fn parse(source: &str) -> ast::Expression {
    parser::Parser::new(source).parse()
}
