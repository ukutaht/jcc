use syntax::intern::Name;

#[derive(Debug, PartialEq)]
pub enum AssignmentType {
    Var,
    Let,
    Const
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(f64)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Assign(AssignmentType, Box<Expression>, Box<Expression>),
    Literal(Literal),
    Identifier(Name)
}
