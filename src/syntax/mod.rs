pub mod ast;
pub mod token;
pub mod span;
mod parser;
mod char;
mod word;
mod scanner;
mod ops;
use errors;

pub fn parse(source: &str) -> Result<ast::Program, errors::CompileError> {
    parser::Parser::new(source).parse()
}
