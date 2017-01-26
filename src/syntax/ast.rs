use syntax::intern::Name;

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    String(Name)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Name),
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub id: Option<Name>,
    pub body: Block
}
#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration)
}

#[derive(Debug, PartialEq)]
pub enum StatementListItem {
    Statement(Statement),
    Declaration
}

#[derive(Debug, PartialEq)]
pub enum VariableDeclarationKind { Var, Let, Const }

#[derive(Debug, PartialEq)]
pub struct VariableDeclarator {
    pub id: Name,
    pub init: Option<Expression>
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub kind: VariableDeclarationKind,
    pub declarations: Vec<VariableDeclarator>
}

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<StatementListItem>);
pub struct Program(pub Vec<StatementListItem>);
