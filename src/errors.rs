use syntax::token::Token;
use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum CompileError {
    UnexpectedEndOfInput,
    UnexpectedToken(Token),
    IllegalChar(char)
}

impl fmt::Display for CompileError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.description())
    }
}

impl Error for CompileError {
    fn description(&self) -> &str {
        match *self {
            CompileError::UnexpectedEndOfInput => {
                "Unexpected end of input"
            },
            CompileError::IllegalChar(_) => {
                "Illegal character"
            }
            CompileError::UnexpectedToken(_) => {
                "Unexpected token"
            }
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}
