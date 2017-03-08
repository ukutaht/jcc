pub mod ast;
pub mod token;
mod parser;
mod char;
mod scanner;
use errors;

pub fn parse(source: &str) -> Result<ast::Program, errors::CompileError> {
    parser::Parser::new(source).parse()
}
