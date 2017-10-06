pub mod ast;
pub mod token;
pub mod span;
pub mod ops;
mod parser;
mod char;
mod word;
mod scanner;
use errors;

pub fn parse(source: &str) -> Result<ast::Program, errors::CompileError> {
    parser::Parser::new(source).parse()
}
