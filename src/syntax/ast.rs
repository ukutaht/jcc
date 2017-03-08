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
    EqEq,
    EqEqEq
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Not,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum LogOp {
    AndAnd,
}

#[derive(Debug, PartialEq)]
pub enum InfixOp {
    BinOp(BinOp),
    LogOp(LogOp)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    Array(Vec<Expression>),
    Call(Box<Expression>, Vec<ArgumentListElement>),
    New(Box<Expression>, Vec<ArgumentListElement>),
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Logical(LogOp, Box<Expression>, Box<Expression>),
    Unary(UnOp, Box<Expression>),
    StaticMember(Box<Expression>, String),
    Function(Function)
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
