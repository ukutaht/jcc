use syntax::ast::*;
use syntax::span::*;
use interner::{Symbol};
use serde::ser::{Serialize, Serializer, SerializeMap};

impl Serialize for Position {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut map = serializer.serialize_map(Some(2))?;
        map.serialize_entry("line", &self.line)?;
        map.serialize_entry("column", &self.column)?;
        map.end()
    }
}

impl Serialize for Span {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut map = serializer.serialize_map(Some(2))?;
        map.serialize_entry("start", &self.start)?;
        map.serialize_entry("end", &self.end)?;
        map.end()
    }
}

impl Serialize for Symbol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        serializer.serialize_str(self.as_str())
    }
}


impl Serialize for StringLiteral {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut map = serializer.serialize_map(Some(3))?;
        map.serialize_entry("type", "Literal")?;
        map.serialize_entry("value", &self.value)?;
        map.serialize_entry("loc", &self.span)?;
        map.end()
    }
}

impl Serialize for NumberLiteral {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut map = serializer.serialize_map(Some(3))?;
        map.serialize_entry("type", "Literal")?;
        map.serialize_entry("value", &self.value)?;
        map.serialize_entry("loc", &self.span)?;
        map.end()
    }
}

impl Serialize for Literal {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            Literal::Number(ref lit) => return lit.serialize(serializer),
            Literal::String(ref lit) => return lit.serialize(serializer),
            _ => {}
        };

        let mut map = serializer.serialize_map(Some(2))?;
        map.serialize_entry("type", "Literal")?;
        match *self {
            Literal::Regex(body, _) => map.serialize_entry("value", &body)?,
            Literal::Null => {
                let typed_opt: Option<u8> = None;
                map.serialize_entry("value", &typed_opt)?
            }
            Literal::True => map.serialize_entry("value", &true)?,
            Literal::False => map.serialize_entry("value", &false)?,
            _ => unreachable!()
        };

        map.end()
    }
}

impl Serialize for Id {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut map = serializer.serialize_map(Some(3))?;
        map.serialize_entry("type", "Identifier")?;
        map.serialize_entry("loc", &self.0)?;
        map.serialize_entry("name", &self.1)?;
        map.end()
    }
}

impl Serialize for Member {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut map = serializer.serialize_map(Some(5))?;
        map.serialize_entry("type", "MemberExpression")?;
        map.serialize_entry("loc", &self.span)?;
        map.serialize_entry("object", &self.object)?;
        map.serialize_entry("property", &self.property)?;
        map.serialize_entry("computed", &self.computed)?;
        map.end()
    }
}

impl Serialize for ArgumentListElement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            ArgumentListElement::Expression(ref e) => e.serialize(serializer),
            ArgumentListElement::SpreadElement(ref span, ref arg) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "SpreadElement")?;
                map.serialize_entry("loc", span)?;
                map.serialize_entry("argument", arg)?;
                map.end()
            }
        }
    }
}

impl Serialize for BinOp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let val = match *self {
            BinOp::Plus => "+",
            BinOp::Minus => "-",
            BinOp::Times => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::BitXor => "^",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::LShift => "<<",
            BinOp::RShift => ">>",
            BinOp::URShift => ">>>",
            BinOp::EqEq => "==",
            BinOp::EqEqEq => "===",
            BinOp::NotEq => "!=",
            BinOp::NotEqEq => "!==",
            BinOp::Lt => "<",
            BinOp::Lte => "<=",
            BinOp::Gt => ">",
            BinOp::Gte => ">=",
            BinOp::In => "in",
            BinOp::Instanceof => "instanceof",
        };
        serializer.serialize_str(val)
    }
}

impl Serialize for UnOp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let val = match *self {
            UnOp::Not => "!",
            UnOp::Minus => "-",
            UnOp::Plus => "+",
            UnOp::Tilde => "~",
            UnOp::Void => "void",
            UnOp::Delete => "delete",
            UnOp::Typeof => "typeof",
        };
        serializer.serialize_str(val)
    }
}

impl Serialize for LogOp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let val = match *self {
            LogOp::AndAnd => "&&",
            LogOp::OrOr => "||",
        };
        serializer.serialize_str(val)
    }
}

impl Serialize for UpdateOp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let val = match *self {
            UpdateOp::PlusPlus => "++",
            UpdateOp::MinusMinus => "--",
        };
        serializer.serialize_str(val)
    }
}

impl Serialize for AssignOp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let val = match *self {
            AssignOp::Eq => "=",
            AssignOp::TimesEq => "*=",
            AssignOp::DivEq => "/=",
            AssignOp::ModEq => "%=",
            AssignOp::PlusEq => "+=",
            AssignOp::MinusEq => "-=",
            AssignOp::LShiftEq => "<<=",
            AssignOp::RShiftEq => ">>=",
            AssignOp::URShiftEq => ">>>=",
            AssignOp::BitAndEq => "&=",
            AssignOp::BitXorEq => "^=",
            AssignOp::BitOrEq => "|=",
        };
        serializer.serialize_str(val)
    }
}

impl Serialize for PropKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            PropKey::String(ref lit) => lit.serialize(serializer),
            PropKey::Number(ref lit) => lit.serialize(serializer),
            PropKey::Identifier(ref id) => id.serialize(serializer),
            PropKey::Computed(ref expr) => expr.serialize(serializer),
        }
    }
}

impl Serialize for MethodDefinitionKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            MethodDefinitionKind::Constructor => serializer.serialize_str("constructor"),
            MethodDefinitionKind::Method => serializer.serialize_str("method"),
            MethodDefinitionKind::Get => serializer.serialize_str("get"),
            MethodDefinitionKind::Set => serializer.serialize_str("set"),
        }
    }
}

impl Serialize for MethodDefinition {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            let mut map = serializer.serialize_map(Some(6))?;
            map.serialize_entry("type", "MethodDefinition")?;
            map.serialize_entry("loc", &self.loc)?;
            map.serialize_entry("key", &self.key)?;
            map.serialize_entry("value", &self.value)?;
            map.serialize_entry("static", &self.is_static)?;
            map.serialize_entry("kind", &self.kind)?;
            map.end()
        }
}

impl Serialize for ClassBody {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            let mut map = serializer.serialize_map(Some(3))?;
            map.serialize_entry("type", "ClassBody")?;
            map.serialize_entry("loc", &self.0)?;
            map.serialize_entry("body", &self.1)?;
            map.end()
        }
}

impl Serialize for ArrowFunctionBody {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            ArrowFunctionBody::Expression(ref e) => e.serialize(serializer),
            ArrowFunctionBody::Block(ref s) => s.serialize(serializer),
        }
    }
}


impl Serialize for Prop {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            // Only used during parsing, should never leak
            Prop::CoverInitializedName(_, _, _) => unreachable!(),
            Prop::Init(ref sp, ref key, ref val) => {
                let mut map = serializer.serialize_map(Some(7))?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("key", &key)?;
                map.serialize_entry("value", &val)?;
                map.serialize_entry("kind", "init")?;
                map.serialize_entry("shorthand", &false)?;
                map.serialize_entry("method", &false)?;
                map.serialize_entry("computed", &false)?;
                map.end()
            }
            Prop::Method(ref sp, ref key, ref function) => {
                let mut map = serializer.serialize_map(Some(8))?;
                map.serialize_entry("type", "Property")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("key", &key)?;
                map.serialize_entry("value", function)?;
                map.serialize_entry("kind", "init")?;
                map.serialize_entry("shorthand", &false)?;
                map.serialize_entry("method", &true)?;
                map.serialize_entry("computed", &false)?;
                map.end()
            }
            Prop::Shorthand(ref sp, ref id) => {
                let mut map = serializer.serialize_map(Some(8))?;
                map.serialize_entry("type", "Property")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("key", &id)?;
                map.serialize_entry("value", &id)?;
                map.serialize_entry("kind", "init")?;
                map.serialize_entry("shorthand", &true)?;
                map.serialize_entry("method", &false)?;
                map.serialize_entry("computed", &false)?;
                map.end()
            }
            Prop::Get(ref sp, ref key, ref function) => {
                let mut map = serializer.serialize_map(Some(8))?;
                map.serialize_entry("type", "Property")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("key", &key)?;
                map.serialize_entry("value", function)?;
                map.serialize_entry("kind", "get")?;
                map.serialize_entry("shorthand", &false)?;
                map.serialize_entry("method", &false)?;
                map.serialize_entry("computed", &false)?;
                map.end()
            }
            Prop::Set(ref sp, ref key, ref function) => {
                let mut map = serializer.serialize_map(Some(8))?;
                map.serialize_entry("type", "Property")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("key", &key)?;
                map.serialize_entry("value", function)?;
                map.serialize_entry("kind", "set")?;
                map.serialize_entry("shorthand", &false)?;
                map.serialize_entry("method", &false)?;
                map.serialize_entry("computed", &false)?;
                map.end()
            }
        }
    }
}

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            let mut map = serializer.serialize_map(Some(6))?;
            map.serialize_entry("type", "FunctionExpression")?;
            map.serialize_entry("loc", &self.span)?;
            map.serialize_entry("id", &self.id)?;
            map.serialize_entry("parameters", &self.parameters)?;
            map.serialize_entry("body", &self.body)?;
            map.serialize_entry("generator", &self.generator)?;
            map.end()
        }
}

impl Serialize for Expression {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            Expression::Array(ref sp, ref elements) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "ArrayExpression")?;
                map.serialize_entry("loc", sp)?;
                map.serialize_entry("elements", elements)?;
                map.end()
            }
            Expression::Conditional(ref sp, ref test, ref consequent, ref alternate) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "ConditionalExpression")?;
                map.serialize_entry("loc", sp)?;
                map.serialize_entry("test", test)?;
                map.serialize_entry("consequent", consequent)?;
                map.serialize_entry("alternate", alternate)?;
                map.end()
            }
            Expression::Object(ref sp, ref props) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "ObjectExpression")?;
                map.serialize_entry("loc", sp)?;
                map.serialize_entry("properties", props)?;
                map.end()
            }
            Expression::Assignment(ref sp, ref op, ref left, ref right) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "AssignmentExpression")?;
                map.serialize_entry("loc", sp)?;
                map.serialize_entry("operator", op)?;
                map.serialize_entry("left", left)?;
                map.serialize_entry("right", right)?;
                map.end()
            }
            Expression::Binary(ref sp, ref op, ref left, ref right) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "BinaryExpression")?;
                map.serialize_entry("loc", sp)?;
                map.serialize_entry("operator", op)?;
                map.serialize_entry("left", left)?;
                map.serialize_entry("right", right)?;
                map.end()
            }
            Expression::Call(ref sp, ref callee, ref args) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "CallExpression")?;
                map.serialize_entry("loc", sp)?;
                map.serialize_entry("callee", callee)?;
                map.serialize_entry("arguments", args)?;
                map.end()
            }
            Expression::Member(ref member) => member.serialize(serializer),
            Expression::Function(ref function) => function.serialize(serializer),
            Expression::ArrowFunction(ref function) => {
                let mut map = serializer.serialize_map(Some(6))?;
                map.serialize_entry("type", "ArrowFunctionExpression")?;
                map.serialize_entry("loc", &function.span)?;
                let id: Option<u32> = None;
                map.serialize_entry("id", &id)?;
                map.serialize_entry("parameters", &function.parameters)?;
                map.serialize_entry("body", &function.body)?;
                map.serialize_entry("expression", &function.is_expression())?;
                map.end()
            }
            Expression::Identifier(ref id) => id.serialize(serializer),
            Expression::Literal(_, ref lit) => lit.serialize(serializer),
            Expression::Logical(ref sp, ref op, ref left, ref right) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "LogicalExpression")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("operator", &op)?;
                map.serialize_entry("left", &left)?;
                map.serialize_entry("right", &right)?;
                map.end()
            }
            Expression::New(ref sp, ref callee, ref args) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "NewExpression")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("callee", &callee)?;
                map.serialize_entry("arguments", &args)?;
                map.end()
            }
            Expression::Unary(ref sp, ref op, ref arg) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "UnaryExpression")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("operator", &op)?;
                map.serialize_entry("argument", &arg)?;
                map.end()
            }
            Expression::Update(ref sp, ref op, ref arg, ref prefix) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "UpdateExpression")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("operator", &op)?;
                map.serialize_entry("argument", &arg)?;
                map.serialize_entry("prefix", &prefix)?;
                map.end()
            }
            Expression::Sequence(ref sp, ref expressions) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "SequenceExpression")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("expressions", &expressions)?;
                map.end()
            }
            Expression::This(ref sp) => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("type", "ThisExpression")?;
                map.serialize_entry("loc", &sp)?;
                map.end()
            }
            Expression::Yield(ref sp, ref argument, ref delegate) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "YieldExpression")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("argument", &argument)?;
                map.serialize_entry("delegate", &delegate)?;
                map.end()
            }
            Expression::Class(ref sp, ref decl) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "ClassExpression")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("id", &decl.id)?;
                map.serialize_entry("superClass", &decl.super_class)?;
                map.serialize_entry("body", &decl.body)?;
                map.end()
            }
        }
    }
}

impl<T> Serialize for Pattern<T> where T: Serialize {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            Pattern::Simple(ref id) => id.serialize(serializer),
            Pattern::Assignment(ref span, ref left, ref right) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "AssignmentPattern")?;
                map.serialize_entry("loc", span)?;
                map.serialize_entry("left", left)?;
                map.serialize_entry("right", right)?;
                map.end()
            }
            Pattern::Array(ref span, ref elements) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "ArrayPattern")?;
                map.serialize_entry("loc", span)?;
                map.serialize_entry("elements", elements)?;
                map.end()
            }
            Pattern::Object(ref span, ref properties) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "ObjectPattern")?;
                map.serialize_entry("loc", span)?;
                map.serialize_entry("properties", properties)?;
                map.end()
            }
            Pattern::RestElement(ref span, ref argument) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "RestElement")?;
                map.serialize_entry("loc", span)?;
                map.serialize_entry("argument", argument)?;
                map.end()
            }
        }
    }
}

impl<T> Serialize for PropPattern<T> where T: Serialize {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut map = serializer.serialize_map(Some(5))?;
        map.serialize_entry("type", "Property")?;
        map.serialize_entry("loc", &self.span)?;
        map.serialize_entry("key", &self.key)?;
        map.serialize_entry("value", &self.value)?;
        map.serialize_entry("shorthand", &self.shorthand)?;
        map.end()
    }
}

impl Serialize for AssignTarget {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            AssignTarget::Id(ref id) => id.serialize(serializer),
            AssignTarget::Member(ref member) => member.serialize(serializer),
        }
    }
}

impl Serialize for Block {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        let mut map = serializer.serialize_map(Some(3))?;
        map.serialize_entry("type", "BlockStatement")?;
        map.serialize_entry("loc", &self.0)?;
        map.serialize_entry("body", &self.1)?;
        map.end()
    }
}

impl Serialize for VariableDeclarationKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            VariableDeclarationKind::Var => serializer.serialize_str("var"),
            VariableDeclarationKind::Let => serializer.serialize_str("let"),
            VariableDeclarationKind::Const => serializer.serialize_str("const"),
        }
    }
}

impl Serialize for VariableDeclarator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            let mut map = serializer.serialize_map(Some(4))?;
            map.serialize_entry("type", "VariableDeclarator")?;
            map.serialize_entry("loc", &self.span)?;
            map.serialize_entry("id", &self.id)?;
            map.serialize_entry("init", &self.init)?;
            map.end()
        }
}

impl Serialize for VariableDeclaration {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            let mut map = serializer.serialize_map(Some(4))?;
            map.serialize_entry("type", "VariableDeclaration")?;
            map.serialize_entry("loc", &self.span)?;
            map.serialize_entry("kind", &self.kind)?;
            map.serialize_entry("declarations", &self.declarations)?;
            map.end()
        }
}

impl Serialize for Statement {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            Statement::Expression(ref span, ref expr) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "ExpressionStatement")?;
                map.serialize_entry("loc", span)?;
                map.serialize_entry("expression", expr)?;
                map.end()
            }
            Statement::VariableDeclaration(ref decl) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "VariableDeclaration")?;
                map.serialize_entry("loc", &decl.span)?;
                map.serialize_entry("kind", &decl.kind)?;
                map.serialize_entry("declarations", &decl.declarations)?;
                map.end()
            }
            Statement::FunctionDeclaration(ref function) => {
                let mut map = serializer.serialize_map(Some(6))?;
                map.serialize_entry("type", "FunctionDeclaration")?;
                map.serialize_entry("loc", &function.span)?;
                map.serialize_entry("id", &function.id)?;
                map.serialize_entry("parameters", &function.parameters)?;
                map.serialize_entry("body", &function.body)?;
                map.serialize_entry("generator", &function.generator)?;
                map.end()
            }
            Statement::ClassDeclaration(ref sp, ref decl) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "ClassDeclaration")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("id", &decl.id)?;
                map.serialize_entry("superClass", &decl.super_class)?;
                map.serialize_entry("body", &decl.body)?;
                map.end()
            }
            Statement::If(ref sp, ref test, ref consequent, ref alternate) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "IfStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("test", &test)?;
                map.serialize_entry("consequent", &consequent)?;
                map.serialize_entry("alternate", &alternate)?;
                map.end()
            }
            Statement::Block(ref block) => block.serialize(serializer),
            Statement::Return(ref sp, ref argument) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "ReturnStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("argument", &argument)?;
                map.end()
            }
            Statement::Debugger(ref sp) => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("type", "DebuggerStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.end()
            }
            Statement::Empty(ref sp) => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("type", "EmptyStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.end()
            }
            Statement::Throw(ref sp, ref argument) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "ThrowStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("argument", &argument)?;
                map.end()
            }
            Statement::Try(ref sp, ref block, ref handler, ref body) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "ThrowStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("block", &block)?;
                map.serialize_entry("handler", &handler)?;
                map.serialize_entry("body", &body)?;
                map.end()
            }
            Statement::Switch(ref sp, ref discriminant, ref cases) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "ThrowStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("discriminant", &discriminant)?;
                map.serialize_entry("cases", &cases)?;
                map.end()
            }
            Statement::Break(ref sp, ref label) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "BreakStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("label", &label)?;
                map.end()
            }
            Statement::DoWhile(ref sp, ref body, ref test) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "DoWhileStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("body", &body)?;
                map.serialize_entry("test", &test)?;
                map.end()
            }
            Statement::While(ref sp, ref test, ref body) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "DoWhileStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("test", &test)?;
                map.serialize_entry("body", &body)?;
                map.end()
            }
            Statement::For(ref sp, ref stmt) => {
                let mut map = serializer.serialize_map(Some(6))?;
                map.serialize_entry("type", "ForStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("init", &stmt.init)?;
                map.serialize_entry("test", &stmt.test)?;
                map.serialize_entry("update", &stmt.update)?;
                map.serialize_entry("body", &stmt.body)?;
                map.end()
            }
            Statement::ForIn(ref sp, ref stmt) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "ForInStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("left", &stmt.left)?;
                map.serialize_entry("right", &stmt.right)?;
                map.serialize_entry("body", &stmt.body)?;
                map.end()
            }
            Statement::ForOf(ref sp, ref stmt) => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("type", "ForOfStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("left", &stmt.left)?;
                map.serialize_entry("right", &stmt.right)?;
                map.serialize_entry("body", &stmt.body)?;
                map.end()
            }
            Statement::With(ref sp, ref object, ref body) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "WithStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("object", &object)?;
                map.serialize_entry("body", &body)?;
                map.end()
            }
            Statement::Labeled(ref sp, ref label, ref body) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "LabeledStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("label", &label)?;
                map.serialize_entry("body", &body)?;
                map.end()
            }
            Statement::Continue(ref sp, ref label) => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("type", "ContinueStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("label", &label)?;
                map.end()
            }
            Statement::Directive(ref sp, ref expr, ref dir) => {
                let mut map = serializer.serialize_map(Some(4))?;
                map.serialize_entry("type", "ExpressionStatement")?;
                map.serialize_entry("loc", &sp)?;
                map.serialize_entry("expression", &expr)?;
                map.serialize_entry("directive", &dir)?;
                map.end()
            }
        }
    }
}

impl Serialize for ForInit {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            match *self {
                ForInit::VarDecl(ref decl) => decl.serialize(serializer),
                ForInit::Expression(ref expr) => expr.serialize(serializer),
            }
        }
}

impl Serialize for ForOpInit {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            match *self {
                ForOpInit::VarDecl(ref decl) => decl.serialize(serializer),
                ForOpInit::Pattern(ref pat) => pat.serialize(serializer),
            }
        }
}

impl Serialize for CatchClause {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            let mut map = serializer.serialize_map(Some(4))?;
            map.serialize_entry("type", "CatchClause")?;
            map.serialize_entry("loc", &self.span)?;
            map.serialize_entry("param", &self.param)?;
            map.serialize_entry("body", &self.body)?;
            map.end()
        }
}

impl Serialize for SwitchCase {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            let mut map = serializer.serialize_map(Some(4))?;
            map.serialize_entry("type", "SwitchCase")?;
            map.serialize_entry("loc", &self.span)?;
            map.serialize_entry("test", &self.test)?;
            map.serialize_entry("consequent", &self.consequent)?;
            map.end()
        }
}

impl Serialize for StatementListItem {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        match *self {
            StatementListItem::Statement(ref statement) => statement.serialize(serializer),
            StatementListItem::Declaration => unimplemented!()
        }
    }
}

impl Serialize for Program {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
        {
            let mut map = serializer.serialize_map(Some(2))?;
            map.serialize_entry("type", "Program")?;
            map.serialize_entry("body", &self.0)?;
            map.end()
        }
}
