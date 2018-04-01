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
pub struct Range {
    pub from: u32,
    pub to: u32
}

#[derive(Debug, PartialEq, Clone)]
pub struct SourceLocation {
    pub start: Position,
    pub end: Position
}

impl SourceLocation {
    pub fn initial() -> SourceLocation {
        SourceLocation {
            start: Position::origin(),
            end: Position::origin()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub range: Option<Range>,
    pub loc: Option<SourceLocation>
}


impl Span {
    pub fn loc(start: Position, end: Position) -> Span {
        Span {
            range: None,
            loc: Some(SourceLocation { start: start, end: end})
        }
    }
}
