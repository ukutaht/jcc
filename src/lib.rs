#![feature(optin_builtin_traits)]
extern crate fnv;
extern crate serde;

pub mod syntax;
pub mod trans;
pub mod estree;
pub mod errors;
pub mod interner;

use errors::CompileError;
use syntax::ast::Program;
use std::io::Cursor;

pub fn parse(code: &str) -> Result<Program, CompileError> {
    syntax::parse(code)
}

pub fn transform(code: &str) -> Result<String, CompileError> {
    let ast = syntax::parse(code)?;
    let mut buffer: Cursor<Vec<u8>> = Cursor::new(Vec::new());
    trans::transpile(&mut buffer, &ast).unwrap();
    Ok(String::from_utf8(buffer.into_inner()).unwrap())
}
