#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Bang,
    BitAnd,
    BitAndEq,
    BitOr,
    BitOrEq,
    BitXor,
    BitXorEq,
    BoolFalse,
    BoolTrue,
    BreakKeyword,
    CaseKeyword,
    CatchKeyword,
    CloseCurly,
    CloseParen,
    CloseSquare,
    Colon,
    Comma,
    ContinueKeyword,
    DebuggerKeyword,
    DefaultKeyword,
    Delete,
    Div,
    DivEq,
    DoKeyword,
    Dot,
    Else,
    Eof,
    Eq,
    EqEq,
    EqEqEq,
    FinallyKeyword,
    FunctionKeyword,
    ForKeyword,
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
    QuestionMark,
    RShift,
    RShiftEq,
    Return,
    Semi,
    String(String),
    SwitchKeyword,
    ThisKeyword,
    ThrowKeyword,
    Tilde,
    Times,
    TimesEq,
    TryKeyword,
    Typeof,
    URShift,
    URShiftEq,
    Var,
    Void,
    WhileKeyword,
    WithKeyword
}

use std::fmt;

impl fmt::Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Bang => write!(fmt, "!"),
            Token::CloseCurly => write!(fmt, "}}"),
            Token::If => write!(fmt, "if"),
            _ => write!(fmt, "?")
        }
    }
}
