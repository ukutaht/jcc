use syntax::token::Token;
use syntax::span::Span;
use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorCause {
    UnexpectedEndOfInput,
    UnexpectedToken(Token),
    IllegalChar(char)
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompileError {
    pub loc: Span,
    pub cause: ErrorCause
}

impl fmt::Display for CompileError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let msg = match self.cause {
            ErrorCause::UnexpectedEndOfInput => {
                "Unexpected end of input"
            },
            ErrorCause::IllegalChar(_) => {
                "Illegal character"
            }
            ErrorCause::UnexpectedToken(_) => {
                "Unexpected token"
            }
        };

        write!(fmt, "Error: Line {}: {}", self.loc.start.line, msg)
    }
}

impl Error for CompileError {
    fn description(&self) -> &str {
        match self.cause {
            ErrorCause::UnexpectedEndOfInput => {
                "Unexpected end of input"
            },
            ErrorCause::IllegalChar(_) => {
                "Illegal character"
            }
            ErrorCause::UnexpectedToken(_) => {
                "Unexpected token"
            }
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}
