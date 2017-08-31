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
    UnqualifiedDelete,
    StrictModeWith,
    StrictReservedWord,
    StrictParamName,
    RestrictedVarName,
    RestrictedVarNameInAssignment,
    RestrictedVarNameInCatch,
    RestrictedVarNameInPrefix,
    RestrictedVarNameInPostfix,
    RestrictedVarNameInFunction,
    UndefinedLabel(String),
    DuplicateLabel(String),
    UnexpectedToken(Token),
    UnexpectedReservedWord,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompileError {
    pub pos: Position,
    pub cause: ErrorCause
}

impl CompileError {
    pub fn new(pos: Position, cause: ErrorCause) -> CompileError {
        CompileError { pos: pos.one_indexed(), cause: cause }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Error: Line {}: ", self.pos.line)?;

         match self.cause {
            ErrorCause::StrictReservedWord => {
                write!(fmt, "Use of future reserved word in strict mode")
            },
            ErrorCause::StrictParamName => {
                write!(fmt, "Parameter name eval or arguments is not allowed in strict mode")
            },
            ErrorCause::MissingCatchOrFinally => {
                write!(fmt, "Missing catch or finally after try")
            },
            ErrorCause::StrictModeWith => {
                write!(fmt, "Strict mode code may not include a with statement")
            },
            ErrorCause::RestrictedVarName => {
                write!(fmt, "Variable name may not be eval or arguments in strict mode")
            },
            ErrorCause::RestrictedVarNameInFunction => {
                write!(fmt, "Function name may not be eval or arguments in strict mode")
            },
            ErrorCause::RestrictedVarNameInCatch => {
                write!(fmt, "Catch variable may not be eval or arguments in strict mode")
            },
            ErrorCause::RestrictedVarNameInPrefix => {
                write!(fmt, "Prefix increment/decrement may not have eval or arguments operand in strict mode")
            },
            ErrorCause::RestrictedVarNameInPostfix => {
                write!(fmt, "Postfix increment/decrement may not have eval or arguments operand in strict mode")
            },
            ErrorCause::RestrictedVarNameInAssignment => {
                write!(fmt, "Assignment to eval or arguments is not allowed in strict mode")
            },
            ErrorCause::UndefinedLabel(ref s) => {
                write!(fmt, "Undefined label '{}'", s)
            },
            ErrorCause::UnqualifiedDelete => {
                write!(fmt, "Delete of an unqualified identifier in strict mode.")
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
            ErrorCause::UnexpectedReservedWord => {
                write!(fmt, "Unexpected reserved word")
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
