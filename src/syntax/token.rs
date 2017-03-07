use syntax::intern::Name;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    Var,
    If,
    Else,
    New,
    Ident(Name),
    String(Name),
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
