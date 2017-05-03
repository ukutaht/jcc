#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Bang,
    BitAnd,
    BitAndEq,
    BitOr,
    BitXor,
    BitXorEq,
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
    LShiftEq,
    LogicalAnd,
    LogicalOr,
    Lt,
    Lte,
    Minus,
    MinusEq,
    MinusMinus,
    Mod,
    ModEq,
    New,
    NotEq,
    NotEqEq,
    Null,
    Number(f64),
    OpenCurly,
    OpenParen,
    OpenSquare,
    Plus,
    PlusEq,
    PlusPlus,
    RShift,
    RShiftEq,
    String(String),
    ThisKeyword,
    Tilde,
    Times,
    TimesEq,
    Typeof,
    URShift,
    URShiftEq,
    Var,
    Void,
}
