#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    pub line: u32,
    pub column: u32
}

impl Position {
    pub fn origin() -> Position {
        Position { line: 1, column: 0 }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub start: Position,
    pub end: Position
}

impl Span {
    pub fn initial() -> Span {
        Span {
            start: Position::origin(),
            end: Position::origin()
        }
    }

    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.clone(),
            end: other.end.clone()
        }
    }

    pub fn to(&self, other: &Span) -> Span {
        Span {
            start: self.start.clone(),
            end: other.start.clone()
        }
    }
}

pub trait Tracking {
    fn span(&self) -> &Span;
}
