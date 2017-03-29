use syntax::span::{Tracking, Span};

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
}

#[derive(Debug, PartialEq)]
pub enum ArgumentListElement {
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    BitXor,
    BitAnd,
    BitOr,
    LShift,
    RShift,
    URShift,
    EqEq,
    EqEqEq,
    NotEq,
    NotEqEq,
    Lt,
    Lte,
    Gt,
    Gte,
    In,
    Instanceof,
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Not,
    Minus,
    Plus,
    Tilde,
    Void,
    Delete,
    Typeof,
}

#[derive(Debug, PartialEq)]
pub enum LogOp {
    AndAnd,
    OrOr
}

#[derive(Debug, PartialEq)]
pub enum InfixOp {
    BinOp(BinOp),
    LogOp(LogOp)
}

#[derive(Debug, PartialEq)]
pub enum UpdateOp {
    PlusPlus,
    MinusMinus
}

impl InfixOp {
    pub fn precedence(&self) -> u8 {
        match *self {
            InfixOp::BinOp(BinOp::Times) | InfixOp::BinOp(BinOp::Div) | InfixOp::BinOp(BinOp::Mod) => 11,
            InfixOp::BinOp(BinOp::Plus) | InfixOp::BinOp(BinOp::Minus) => 9,
            InfixOp::BinOp(BinOp::LShift) | InfixOp::BinOp(BinOp::RShift) | InfixOp::BinOp(BinOp::URShift) => 8,
            InfixOp::BinOp(BinOp::Lt) | InfixOp::BinOp(BinOp::Lte) | InfixOp::BinOp(BinOp::Gt) | InfixOp::BinOp(BinOp::Gte) | InfixOp::BinOp(BinOp::In) | InfixOp::BinOp(BinOp::Instanceof) => 7,
            InfixOp::BinOp(BinOp::EqEq) | InfixOp::BinOp(BinOp::EqEqEq) | InfixOp::BinOp(BinOp::NotEq) | InfixOp::BinOp(BinOp::NotEqEq) => 6,
            InfixOp::BinOp(BinOp::BitAnd) => 5,
            InfixOp::BinOp(BinOp::BitXor) => 4,
            InfixOp::BinOp(BinOp::BitOr) => 3,
            InfixOp::LogOp(LogOp::AndAnd) => 2,
            InfixOp::LogOp(LogOp::OrOr) => 1,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AssignOp {
    Eq
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Array(Span, Vec<Option<Expression>>),
    Assignment(Span, AssignOp, Box<Expression>, Box<Expression>),
    Binary(Span, BinOp, Box<Expression>, Box<Expression>),
    Call(Span, Box<Expression>, Vec<ArgumentListElement>),
    ComputedMember(Span, Box<Expression>, Box<Expression>),
    Function(Function),
    Identifier(Span, String),
    Literal(Span, Literal),
    Logical(Span, LogOp, Box<Expression>, Box<Expression>),
    New(Span, Box<Expression>, Vec<ArgumentListElement>),
    StaticMember(Span, Box<Expression>, String),
    Unary(Span, UnOp, Box<Expression>),
    Update(Span, UpdateOp, Box<Expression>, bool),
    This(Span),
}

impl Tracking for Expression {
    fn span(&self) -> &Span {
        match self {
            &Expression::Array(ref s, _) => s,
            &Expression::Binary(ref s, _, _, _) => s,
            &Expression::Call(ref s, _, _) => s,
            &Expression::ComputedMember(ref s, _, _) => s,
            &Expression::Identifier(ref s, _) => s,
            &Expression::Literal(ref s, _) => s,
            &Expression::Logical(ref s, _, _, _) => s,
            &Expression::New(ref s, _, _) => s,
            &Expression::StaticMember(ref s, _, _) => s,
            &Expression::Unary(ref s, _, _) => s,
            e => panic!("Cannot get span for: {:?}", e)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Identifier(String),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub id: Option<String>,
    pub body: Block,
    pub parameters: Vec<Pattern>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(Function),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Block(Block)
}

#[derive(Debug, PartialEq)]
pub enum StatementListItem {
    Statement(Statement),
    Declaration,
}

#[derive(Debug, PartialEq)]
pub enum VariableDeclarationKind {
    Var,
    Let,
    Const,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclarator {
    pub id: String,
    pub init: Option<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub kind: VariableDeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<StatementListItem>);
#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<StatementListItem>);
