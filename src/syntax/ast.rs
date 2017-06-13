use syntax::span::{Tracking, Span};

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Null,
    True,
    False
}

#[derive(Debug, PartialEq)]
pub struct Id(pub Span, pub String);

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
    Eq,
    TimesEq,
    DivEq,
    ModEq,
    PlusEq,
    MinusEq,
    LShiftEq,
    RShiftEq,
    URShiftEq,
    BitAndEq,
    BitXorEq,
    BitOrEq,
}

#[derive(Debug, PartialEq)]
pub enum PropKey {
    Identifier(Span, String),
    String(Span, String),
    Number(Span, f64)
}

#[derive(Debug, PartialEq)]
pub enum Prop {
    Init(Span, PropKey, Expression),
    Get(Span, PropKey, Function),
    Set(Span, PropKey, Function)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Array(Span, Vec<Option<Expression>>),
    Conditional(Span, Box<Expression>, Box<Expression>, Box<Expression>),
    Object(Span, Vec<Prop>),
    Assignment(Span, AssignOp, Box<Expression>, Box<Expression>),
    Binary(Span, BinOp, Box<Expression>, Box<Expression>),
    Call(Span, Box<Expression>, Vec<ArgumentListElement>),
    ComputedMember(Span, Box<Expression>, Box<Expression>),
    Function(Span, Function),
    Identifier(Span, String),
    Literal(Span, Literal),
    Logical(Span, LogOp, Box<Expression>, Box<Expression>),
    New(Span, Box<Expression>, Vec<ArgumentListElement>),
    StaticMember(Span, Box<Expression>, String),
    Unary(Span, UnOp, Box<Expression>),
    Update(Span, UpdateOp, Box<Expression>, bool),
    Sequence(Span, Vec<Expression>),
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
            &Expression::Object(ref s, _) => s,
            &Expression::Conditional(ref s, _, _, _) => s,
            &Expression::Assignment(ref s, _, _, _) => s,
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
pub struct CatchClause {
    pub param: String,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Block
}

#[derive(Debug, PartialEq)]
pub enum ForInit {
    VarDecl(VariableDeclaration),
    Expression(Expression)
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Span, Expression),
    VariableDeclaration(Span, VariableDeclaration),
    FunctionDeclaration(Function),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Block(Span, Block),
    Return(Span, Option<Expression>),
    Debugger(Span),
    Empty(Span),
    Throw(Span, Expression),
    Try(Span, Block, Option<CatchClause>, Option<Block>),
    Switch(Span, Expression, Vec<SwitchCase>),
    Break(Span, Option<Id>),
    DoWhile(Span, Box<Statement>, Expression),
    While(Span, Expression, Box<Statement>),
    For(Span, Option<ForInit>, Option<Expression>, Option<Expression>, Box<Statement>),
    ForIn(Span, ForInit, Expression, Box<Statement>),
    With(Span, Expression, Box<Statement>),
    Labeled(Span, Id, Box<Statement>),
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
