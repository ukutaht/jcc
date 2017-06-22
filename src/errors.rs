use syntax::token::Token;
use syntax::span::Position;
use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorCause {
    MissingCatchOrFinally,
    MultipleDefaultsInSwitch,
    UnexpectedToken(Token),
    IllegalChar(char)
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompileError {
    pub pos: Position,
    pub cause: ErrorCause
}

impl fmt::Display for CompileError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Error: Line {}: ", self.pos.line)?;

         match self.cause {
            ErrorCause::MissingCatchOrFinally => {
                write!(fmt, "Missing catch or finally after try")
            },
            ErrorCause::MultipleDefaultsInSwitch => {
                write!(fmt, "More than one default clause in switch statement")
            },
            ErrorCause::IllegalChar(_) => {
                write!(fmt, "Illegal character")
            }
            ErrorCause::UnexpectedToken(Token::Eof) => {
                write!(fmt, "Unexpected end of input")
            }
            ErrorCause::UnexpectedToken(Token::Ident(_)) => {
                write!(fmt, "Unexpected identifier")
            }
            ErrorCause::UnexpectedToken(Token::Number(_)) => {
                write!(fmt, "Unexpected number")
            }
            ErrorCause::UnexpectedToken(Token::String(_)) => {
                write!(fmt, "Unexpected string")
            }
            ErrorCause::UnexpectedToken(ref t) => {
                write!(fmt, "Unexpected token {}", t)
            }
        }
    }
}

impl Error for CompileError {
    fn description(&self) -> &str {
        "Compile error"
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}
