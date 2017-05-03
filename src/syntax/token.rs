#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Bang,
    BitAnd,
    BitOr,
    BitXor,
    CloseCurly,
    CloseParen,
    CloseSquare,
    Comma,
    Delete,
    Div,
    DivEq,
    Dot,
    Else,
    Eof,
    Eq,
    EqEq,
    EqEqEq,
    FunctionKeyword,
    Gt,
    Gte,
    Ident(String),
    If,
    In,
    Instanceof,
    LShift,
    LogicalAnd,
    LogicalOr,
    Lt,
    Lte,
    Minus,
    MinusMinus,
    Mod,
    New,
    NotEq,
    NotEqEq,
    Null,
    Number(f64),
    OpenCurly,
    OpenParen,
    OpenSquare,
    Plus,
    PlusPlus,
    RShift,
    String(String),
    ThisKeyword,
    Tilde,
    Times,
    TimesEq,
    Typeof,
    URShift,
    Var,
    Void,
}
