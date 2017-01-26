pub mod ast;
pub mod intern;
mod token;
mod parser;
mod char;
mod scanner;

pub fn parse(source: &str) -> ast::Program {
    parser::Parser::new(source).parse()
}
