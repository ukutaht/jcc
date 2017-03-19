use syntax::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    Var,
    If,
    Else,
    New,
    Eof,
    Eq,
    EqEq,
    EqEqEq,
    NotEq,
    NotEqEq,
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
    Times,
    Div,
    Mod,
    Bang,
    LogicalAnd,
    LogicalOr,
    BitXor,
    Ident(String),
    String(String),
    Number(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub span: Span
}
