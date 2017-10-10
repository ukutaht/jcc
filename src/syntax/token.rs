use interner::Symbol;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    Arrow,
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
    ClassKeyword,
    CloseCurly,
    CloseParen,
    CloseSquare,
    Colon,
    Comma,
    Const,
    ContinueKeyword,
    DebuggerKeyword,
    DefaultKeyword,
    Delete,
    Div,
    DivEq,
    DoKeyword,
    Dot,
    Ellipsis,
    Else,
    EnumKeyword,
    Eof,
    Eq,
    EqEq,
    EqEqEq,
    ExtendsKeyword,
    FinallyKeyword,
    ForKeyword,
    FunctionKeyword,
    Gt,
    Gte,
    Ident(Symbol),
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
    Star,
    String(Symbol, Symbol),
    SwitchKeyword,
    ThisKeyword,
    ThrowKeyword,
    Tilde,
    TimesEq,
    TryKeyword,
    Typeof,
    URShift,
    URShiftEq,
    Var,
    Void,
    WhileKeyword,
    WithKeyword,
    YieldKeyword,
}

use std::fmt;

impl fmt::Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Bang => write!(fmt, "!"),
            Token::BoolFalse => write!(fmt, "false"),
            Token::BoolTrue => write!(fmt, "true"),
            Token::CloseCurly => write!(fmt, "}}"),
            Token::CloseParen => write!(fmt, ")"),
            Token::CloseSquare => write!(fmt, "]"),
            Token::Comma => write!(fmt, ","),
            Token::Div => write!(fmt, "/"),
            Token::Dot => write!(fmt, "."),
            Token::If => write!(fmt, "if"),
            Token::In => write!(fmt, "in"),
            Token::Minus => write!(fmt, "-"),
            Token::Null => write!(fmt, "null"),
            Token::OpenCurly => write!(fmt, "{{"),
            Token::OpenParen => write!(fmt, "("),
            Token::Semi => write!(fmt, ";"),
            Token::Star => write!(fmt, "*"),
            Token::QuestionMark => write!(fmt, "?"),
            Token::Plus => write!(fmt, "+"),
            Token::Lte => write!(fmt, "<="),
            Token::Arrow => write!(fmt, "=>"),
            Token::Ellipsis => write!(fmt, "..."),
            Token::Colon => write!(fmt, ":"),
            Token::Eq => write!(fmt, "="),
            Token::Const => write!(fmt, "const"),
            Token::YieldKeyword => write!(fmt, "yield"),
            ref t => write!(fmt, "{:?}", t)
        }
    }
}
