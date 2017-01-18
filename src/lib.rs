#[macro_use]
extern crate lazy_static;
pub mod syntax;
pub mod trans;

#[derive(Debug, PartialEq, Eq)]
pub enum CompileError {
}

pub fn transform(code: &str) -> Result<String, CompileError> {
    let ast = syntax::parse(code);
    let output = trans::transpile(ast);
    Ok(output)
}
