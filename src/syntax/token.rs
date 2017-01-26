use syntax::intern::{Name};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    Var,
    Ident(Name),
    String(Name),
    Eof,
    Number(f64),
    Equals,
    FunctionKeyword,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
}

