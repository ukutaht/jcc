#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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
    Tilde,
    Void,
    Delete,
    Typeof,
    Div,
    Mod,
    Bang,
    LogicalAnd,
    LogicalOr,
    BitXor,
    BitAnd,
    BitOr,
    LShift,
    RShift,
    URShift,
    Ident(String),
    String(String),
    Number(f64),
}
