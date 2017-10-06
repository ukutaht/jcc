use syntax::span::Span;
use interner::Symbol;

#[derive(Debug, PartialEq)]
pub struct StringLiteral {
    pub span: Span,
    pub raw: Symbol,
    pub value: Symbol
}

#[derive(Debug, PartialEq)]
pub struct NumberLiteral {
    pub span: Span,
    pub value: f64
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(NumberLiteral),
    String(StringLiteral),
    Regex(Symbol, Vec<char>),
    Null,
    True,
    False
}

#[derive(Debug, PartialEq, Clone)]
pub struct Id(pub Span, pub Symbol);

#[derive(Debug, PartialEq)]
pub enum ArgumentListElement {
    Expression(Expression),
    SpreadElement(Span, Expression),
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
    Identifier(Id),
    String(StringLiteral),
    Number(NumberLiteral),
    Computed(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Prop {
    // Only used during parsing, should never leak out of the parser
    CoverInitializedName(Span, PropKey, Expression),
    Init(Span, PropKey, Expression),
    Method(Span, PropKey, Function),
    Shorthand(Span, Id),
    Get(Span, PropKey, Function),
    Set(Span, PropKey, Function)
}

#[derive(Debug, PartialEq)]
pub struct Member {
    pub span: Span,
    pub object: Expression,
    pub property: Expression,
    pub computed: bool
}

#[derive(Debug, PartialEq)]
pub enum AssignTarget {
    Id(Id),
    Member(Member),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Array(Span, Vec<Option<ArgumentListElement>>),
    Conditional(Span, Box<Expression>, Box<Expression>, Box<Expression>),
    Object(Span, Vec<Prop>),
    Assignment(Span, AssignOp, Box<Pattern<AssignTarget>>, Box<Expression>),
    Binary(Span, BinOp, Box<Expression>, Box<Expression>),
    Call(Span, Box<Expression>, Vec<ArgumentListElement>),
    Member(Box<Member>),
    Function(Function),
    Identifier(Id),
    Literal(Span, Literal),
    Logical(Span, LogOp, Box<Expression>, Box<Expression>),
    New(Span, Box<Expression>, Vec<ArgumentListElement>),
    Unary(Span, UnOp, Box<Expression>),
    Update(Span, UpdateOp, Box<Expression>, bool),
    Sequence(Span, Vec<Expression>),
    This(Span),
    ArrowFunction(ArrowFunction),
    Class(Span, Box<ClassDecl>),
    Yield(Span, Box<Option<Expression>>, bool)
}

#[derive(Debug, PartialEq)]
pub enum Pattern<T> {
    Simple(T),
    Assignment(Span, Box<Pattern<T>>, Expression),
    Array(Span, Vec<Option<Pattern<T>>>),
    Object(Span, Vec<PropPattern<T>>),
    RestElement(Span, Box<Pattern<T>>),
}

#[derive(Debug, PartialEq)]
pub struct PropPattern<T> {
    pub span: Span,
    pub key: PropKey,
    pub value: Pattern<T>,
    pub shorthand: bool
}

impl<T> Pattern<T> {
    pub fn is_simple(&self) -> bool {
        match *self {
            Pattern::Simple(_) => true,
            _ => false
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ArrowFunctionBody {
    Expression(Box<Expression>),
    Block(Block)
}

#[derive(Debug, PartialEq)]
pub struct ArrowFunction {
    pub span: Span,
    pub body: ArrowFunctionBody,
    pub parameters: Vec<Pattern<Id>>,
}

impl ArrowFunction {
    pub fn is_expression(&self) -> bool {
        match self.body {
            ArrowFunctionBody::Expression(_) => true,
            _ => false
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub span: Span,
    pub id: Option<Symbol>,
    pub body: Block,
    pub parameters: Vec<Pattern<Id>>,
    pub generator: bool
}

#[derive(Debug, PartialEq)]
pub struct CatchClause {
    pub span: Span,
    pub param: Pattern<Id>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct SwitchCase {
    pub span: Span,
    pub test: Option<Expression>,
    pub consequent: Vec<Statement>
}

#[derive(Debug, PartialEq)]
pub enum ForInit {
    VarDecl(VariableDeclaration),
    Expression(Expression)
}

#[derive(Debug, PartialEq)]
pub struct ForStatement {
    pub init: Option<ForInit>,
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Statement
}

#[derive(Debug, PartialEq)]
pub enum ForOpInit {
    VarDecl(VariableDeclaration),
    Pattern(Pattern<AssignTarget>)
}

#[derive(Debug, PartialEq)]
pub struct ForOpStatement {
    pub left: ForOpInit,
    pub right: Expression,
    pub body: Statement
}

#[derive(Debug, PartialEq)]
pub enum MethodDefinitionKind { Constructor, Method, Get, Set }

#[derive(Debug, PartialEq)]
pub struct MethodDefinition {
    pub loc: Span,
    pub key: PropKey,
    pub value: Function,
    pub is_static: bool,
    pub kind: MethodDefinitionKind
}

#[derive(Debug, PartialEq)]
pub struct ClassBody(pub Span, pub Vec<MethodDefinition>);

#[derive(Debug, PartialEq)]
pub struct ClassDecl {
    pub id: Option<Id>,
    pub super_class: Option<Expression>,
    pub body: ClassBody
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Span, Expression),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(Function),
    ClassDeclaration(Span, ClassDecl),
    If(Span, Expression, Box<Statement>, Option<Box<Statement>>),
    Block(Block),
    Return(Span, Option<Expression>),
    Debugger(Span),
    Empty(Span),
    Throw(Span, Expression),
    Try(Span, Block, Option<CatchClause>, Option<Block>),
    Switch(Span, Expression, Vec<SwitchCase>),
    Break(Span, Option<Id>),
    DoWhile(Span, Box<Statement>, Expression),
    While(Span, Expression, Box<Statement>),
    For(Span, Box<ForStatement>),
    ForIn(Span, Box<ForOpStatement>),
    ForOf(Span, Box<ForOpStatement>),
    With(Span, Expression, Box<Statement>),
    Labeled(Span, Id, Box<Statement>),
    Continue(Span, Option<Id>),
    Directive(Span, Expression, String)
}

#[derive(Debug, PartialEq)]
pub enum StatementListItem {
    Statement(Statement),
    Declaration,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VariableDeclarationKind { Var, Let, Const }

#[derive(Debug, PartialEq)]
pub struct VariableDeclarator {
    pub span: Span,
    pub id: Pattern<Id>,
    pub init: Option<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub span: Span,
    pub kind: VariableDeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

#[derive(Debug, PartialEq)]
pub struct Block(pub Span, pub Vec<StatementListItem>);
#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<StatementListItem>);
