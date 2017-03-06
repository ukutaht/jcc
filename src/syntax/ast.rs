use syntax::intern::Name;

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    String(Name),
}

#[derive(Debug, PartialEq)]
pub enum ArgumentListElement {
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Plus
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Not,
    Minus
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
    Identifier(Name),
    Array(Vec<Expression>),
    Call(Box<Expression>, Vec<ArgumentListElement>),
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Logical(LogOp, Box<Expression>, Box<Expression>),
    Unary(UnOp, Box<Expression>),
    StaticMember(Box<Expression>, Name)
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Identifier(Name),
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub id: Option<Name>,
    pub body: Block,
    pub parameters: Vec<Pattern>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
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
    pub id: Name,
    pub init: Option<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub kind: VariableDeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<StatementListItem>);
pub struct Program(pub Vec<StatementListItem>);
