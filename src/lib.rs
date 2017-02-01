#[macro_use]
extern crate lazy_static;
pub mod syntax;
pub mod trans;

use std::io::Cursor;

#[derive(Debug)]
pub enum CompileError {
    Io(std::io::Error),
}

impl std::convert::From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> CompileError {
        CompileError::Io(e)
    }
}

pub fn transform(code: &str) -> Result<String, CompileError> {
    let ast = syntax::parse(code);
    let mut buffer: Cursor<Vec<u8>> = Cursor::new(Vec::new());
    // TODO: Handle IO error
    try!(trans::transpile(&mut buffer, &ast));
    Ok(String::from_utf8(buffer.into_inner()).unwrap())
}
