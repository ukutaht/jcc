use syntax::token::Token;
use syntax::span::Position;
use std::error::Error;
use std::fmt;
use std;

pub type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorCause {
    MissingCatchOrFinally,
    MultipleDefaultsInSwitch,
    InvalidLHSAssignment,
    IllegalBreak,
    IllegalContinue,
    IllegalReturn,
    IllegalToken,
    InvalidHexEscape,
    NewLineAfterThrow,
    UndefinedLabel(String),
    DuplicateLabel(String),
    UnexpectedToken(Token),
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
            ErrorCause::UndefinedLabel(ref s) => {
                write!(fmt, "Undefined label '{}'", s)
            },
            ErrorCause::DuplicateLabel(ref s) => {
                write!(fmt, "Label '{}' has already been declared", s)
            },
            ErrorCause::IllegalToken => {
                write!(fmt, "Unexpected token ILLEGAL")
            },
            ErrorCause::InvalidHexEscape => {
                write!(fmt, "Invalid hexadecimal escape sequence")
            },
            ErrorCause::IllegalReturn => {
                write!(fmt, "Illegal return statement")
            },
            ErrorCause::IllegalBreak => {
                write!(fmt, "Illegal break statement")
            },
            ErrorCause::NewLineAfterThrow => {
                write!(fmt, "Illegal newline after throw")
            },
            ErrorCause::IllegalContinue => {
                write!(fmt, "Illegal continue statement")
            },
            ErrorCause::InvalidLHSAssignment => {
                write!(fmt, "Invalid left-hand side in assignment")
            },
            ErrorCause::MultipleDefaultsInSwitch => {
                write!(fmt, "More than one default clause in switch statement")
            },
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
