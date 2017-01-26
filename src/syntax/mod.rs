pub mod ast;
pub mod intern;
mod parser;
mod char;

pub fn parse(source: &str) -> ast::Program {
    parser::Parser::new(source).parse()
}
