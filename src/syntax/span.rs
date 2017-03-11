#[derive(Debug, PartialEq, Clone, Copy)]
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

    pub fn to(&self, other: &Span) -> Span {
        Span {
            start: self.start,
            end: other.end
        }
    }

    pub fn to_pos(&self, end: Position) -> Span {
        Span {
            start: self.start,
            end: end
        }
    }
}

pub trait Tracking {
    fn span(&self) -> &Span;
}
