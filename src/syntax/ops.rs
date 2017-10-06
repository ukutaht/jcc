use syntax::ast::{UnOp, UpdateOp, InfixOp, BinOp, LogOp, AssignOp};
use syntax::token::Token;

pub trait AsOperator {
    fn as_unary_op(&self) -> Option<UnOp>;
    fn as_update_op(&self) -> Option<UpdateOp>;
    fn as_infix_op(&self, bool) -> Option<InfixOp>;
    fn as_assign_op(&self) -> Option<AssignOp>;
}

impl AsOperator for Token {
    fn as_unary_op(&self) -> Option<UnOp> {
        match *self {
            Token::Bang => Some(UnOp::Not),
            Token::Minus => Some(UnOp::Minus),
            Token::Plus => Some(UnOp::Plus),
            Token::Tilde => Some(UnOp::Tilde),
            Token::Void => Some(UnOp::Void),
            Token::Delete => Some(UnOp::Delete),
            Token::Typeof => Some(UnOp::Typeof),
            _ => None
        }
    }

    fn as_update_op(&self) -> Option<UpdateOp> {
        match *self {
            Token::PlusPlus => Some(UpdateOp::PlusPlus),
            Token::MinusMinus => Some(UpdateOp::MinusMinus),
            _ => None
        }
    }

    fn as_infix_op(&self, allow_in: bool) -> Option<InfixOp> {
        match *self {
            Token::Plus => Some(InfixOp::BinOp(BinOp::Plus)),
            Token::BitXor => Some(InfixOp::BinOp(BinOp::BitXor)),
            Token::BitAnd => Some(InfixOp::BinOp(BinOp::BitAnd)),
            Token::BitOr => Some(InfixOp::BinOp(BinOp::BitOr)),
            Token::LShift => Some(InfixOp::BinOp(BinOp::LShift)),
            Token::RShift => Some(InfixOp::BinOp(BinOp::RShift)),
            Token::URShift => Some(InfixOp::BinOp(BinOp::URShift)),
            Token::Star => Some(InfixOp::BinOp(BinOp::Times)),
            Token::Div => Some(InfixOp::BinOp(BinOp::Div)),
            Token::Mod => Some(InfixOp::BinOp(BinOp::Mod)),
            Token::Minus => Some(InfixOp::BinOp(BinOp::Minus)),
            Token::EqEq => Some(InfixOp::BinOp(BinOp::EqEq)),
            Token::NotEq => Some(InfixOp::BinOp(BinOp::NotEq)),
            Token::NotEqEq => Some(InfixOp::BinOp(BinOp::NotEqEq)),
            Token::EqEqEq => Some(InfixOp::BinOp(BinOp::EqEqEq)),
            Token::Lt => Some(InfixOp::BinOp(BinOp::Lt)),
            Token::Lte => Some(InfixOp::BinOp(BinOp::Lte)),
            Token::Gt => Some(InfixOp::BinOp(BinOp::Gt)),
            Token::Gte => Some(InfixOp::BinOp(BinOp::Gte)),
            Token::In if allow_in => Some(InfixOp::BinOp(BinOp::In)),
            Token::Instanceof => Some(InfixOp::BinOp(BinOp::Instanceof)),
            Token::LogicalAnd => Some(InfixOp::LogOp(LogOp::AndAnd)),
            Token::LogicalOr => Some(InfixOp::LogOp(LogOp::OrOr)),
            _ => None
        }
    }

    fn as_assign_op(&self) -> Option<AssignOp> {
        match *self {
            Token::Eq => Some(AssignOp::Eq),
            Token::TimesEq => Some(AssignOp::TimesEq),
            Token::DivEq => Some(AssignOp::DivEq),
            Token::ModEq => Some(AssignOp::ModEq),
            Token::PlusEq => Some(AssignOp::PlusEq),
            Token::MinusEq => Some(AssignOp::MinusEq),
            Token::LShiftEq => Some(AssignOp::LShiftEq),
            Token::RShiftEq => Some(AssignOp::RShiftEq),
            Token::URShiftEq => Some(AssignOp::URShiftEq),
            Token::BitAndEq => Some(AssignOp::BitAndEq),
            Token::BitXorEq => Some(AssignOp::BitXorEq),
            Token::BitOrEq => Some(AssignOp::BitOrEq),
            _ => None
        }
    }
}

pub trait AsStr {
    fn as_str(&self) -> &str;
}

impl AsStr for BinOp {
    fn as_str(&self) -> &'static str {
        match *self {
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
        }
    }
}

impl AsStr for UnOp {
    fn as_str(&self) -> &'static str {
        match *self {
            UnOp::Not => "!",
            UnOp::Minus => "-",
            UnOp::Plus => "+",
            UnOp::Tilde => "~",
            UnOp::Void => "void",
            UnOp::Delete => "delete",
            UnOp::Typeof => "typeof",
        }
    }
}

impl AsStr for LogOp {
    fn as_str(&self) -> &'static str {
        match *self {
            LogOp::AndAnd => "&&",
            LogOp::OrOr => "||",
        }
    }
}

impl AsStr for UpdateOp {
    fn as_str(&self) -> &'static str {
        match *self {
            UpdateOp::PlusPlus => "++",
            UpdateOp::MinusMinus => "--",
        }
    }
}

impl AsStr for AssignOp {
    fn as_str(&self) -> &'static str {
        match *self {
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
        }
    }
}
