use syntax::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    Var,
    If,
    Else,
    New,
    Ident(String),
    String(String),
    Eof,
    Number(f64),
    Eq,
    EqEq,
    EqEqEq,
    FunctionKeyword,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,
    Comma,
    Dot,
    Plus,
    Minus,
    Bang,
    LogicalAnd,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub span: Span
}
