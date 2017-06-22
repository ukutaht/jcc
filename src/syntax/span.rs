#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position {
    pub line: u32,
    pub column: u32
}

impl Position {
    pub fn origin() -> Position {
        Position { line: 1, column: 0 }
    }

    pub fn one_indexed(self) -> Position {
        Position { line: self.line, column: self.column + 1 }
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
}

pub trait Tracking {
    fn span(&self) -> &Span;
}
