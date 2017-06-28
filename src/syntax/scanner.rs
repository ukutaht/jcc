use syntax::char::ESCharExt;
use syntax::span::Position;
use syntax::token::Token;
use errors::{CompileError, ErrorCause, Result};
use std::str;
use std::mem;
use std::char;

static KEYWORD_VAR: &'static str = "var";
static KEYWORD_FUNCTION: &'static str = "function";
static KEYWORD_THIS: &'static str = "this";
static KEYWORD_NULL: &'static str = "null";
static KEYWORD_IF: &'static str = "if";
static KEYWORD_ELSE: &'static str = "else";
static KEYWORD_NEW: &'static str = "new";
static KEYWORD_VOID: &'static str = "void";
static KEYWORD_DELETE: &'static str = "delete";
static KEYWORD_TYPEOF: &'static str = "typeof";
static KEYWORD_IN: &'static str = "in";
static KEYWORD_INSTANCEOF: &'static str = "instanceof";
static KEYWORD_RETURN: &'static str = "return";
static KEYWORD_DEBUGGER: &'static str = "debugger";
static KEYWORD_THROW: &'static str = "throw";
static KEYWORD_TRY: &'static str = "try";
static KEYWORD_CATCH: &'static str = "catch";
static KEYWORD_FINALLY: &'static str = "finally";
static KEYWORD_SWITCH: &'static str = "switch";
static KEYWORD_CASE: &'static str = "case";
static KEYWORD_DEFAULT: &'static str = "default";
static KEYWORD_BREAK: &'static str = "break";
static KEYWORD_DO: &'static str = "do";
static KEYWORD_WHILE: &'static str = "while";
static KEYWORD_FOR: &'static str = "for";
static KEYWORD_WITH: &'static str = "with";
static KEYWORD_CONTINUE: &'static str = "continue";
static BOOL_TRUE: &'static str = "true";
static BOOL_FALSE: &'static str = "false";

pub struct Scanner<'a> {
    bytes: &'a [u8],
    index: usize,
    line: u32,
    column: u32,
    pub last_pos: Position,
    pub lookahead_start: Position,
    pub lookahead: Token,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner {
        Scanner {
            bytes: source.as_bytes(),
            index: 0,
            column: 0,
            line: 1,
            last_pos: Position::origin(),
            lookahead_start: Position::origin(),
            lookahead: Token::Eof
        }
    }

    pub fn at_newline(&self) -> bool {
        self.last_pos.line != self.lookahead_start.line
    }

    pub fn position_at_start(&mut self) -> Result<()> {
        self.lookahead = self.lex()?;
        Ok(())
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.last_pos = self.pos();
        let tok = self.lex()?;
        Ok(mem::replace(&mut self.lookahead, tok))
    }

    pub fn pos(&self) -> Position {
        Position { column: self.column, line: self.line }
    }

    fn skip_single_line_comment(&mut self) {
        self.next_byte(); self.next_byte();
        while let Some(ch) = self.next_char() {
            if ch.is_es_newline() {
                if ch == '\r' && self.current_byte() == Some(b'\n') {
                    self.next_byte();
                }
                self.line += 1;
                self.column = 0;
                break;
            }
        }
    }

    fn skip_multi_line_comment(&mut self) -> Result<()> {
        self.next_byte(); self.next_byte();

        loop {
            match self.next_char() {
                Some(ch) if ch.is_es_newline() => {
                    if ch == '\r' && self.current_byte() == Some(b'\n') {
                        self.next_byte();
                    }
                    self.line += 1;
                    self.column = 0;
                },
                Some('*') => {
                    if self.eat_byte(b'/') {
                        return Ok(())
                    }
                },
                Some(_) => continue,
                None => {
                    return Err(self.invalid_token())
                }
            }
        }
    }

    fn lex(&mut self) -> Result<Token> {
        let character;

        loop {
            match self.current_byte() {
                Some(b'\n') => {
                    self.next_byte();
                    self.line += 1;
                    self.column = 0;
                }
                Some(b'\r') => {
                    self.next_byte();
                    self.eat_byte(b'\n');
                    self.line += 1;
                    self.column = 0;
                }
                Some(b' ') => {
                    self.next_byte();
                },
                Some(b'/') => {
                    match self.peek_byte() {
                        Some(b'/') => self.skip_single_line_comment(),
                        Some(b'*') => self.skip_multi_line_comment()?,
                        Some(c) => {
                            character = c;
                            break;
                        },
                        None => {
                            self.lookahead_start = self.pos();
                            return Ok(Token::Eof)
                        }
                    }
                }
                Some(c) => {
                    character = c;
                    break;
                }
                None => {
                    self.lookahead_start = self.pos();
                    return Ok(Token::Eof)
                }
            };
        }

        self.lookahead_start = self.pos();
        if character == b'\'' || character == b'"' {
            self.scan_string(character)
        } else if (character as char).is_digit(10) {
            self.scan_number()
        } else if self.eat_byte(b'=') {
            if self.eat_byte(b'=') {
                if self.eat_byte(b'=') {
                    Ok(Token::EqEqEq)
                } else {
                    Ok(Token::EqEq)
                }
            } else {
                Ok(Token::Eq)
            }
        } else if self.eat_byte(b'(') {
            Ok(Token::OpenParen)
        } else if self.eat_byte(b')') {
            Ok(Token::CloseParen)
        } else if self.eat_byte(b'{') {
            Ok(Token::OpenCurly)
        } else if self.eat_byte(b'}') {
            Ok(Token::CloseCurly)
        } else if self.eat_byte(b'[') {
            Ok(Token::OpenSquare)
        } else if self.eat_byte(b']') {
            Ok(Token::CloseSquare)
        } else if self.eat_byte(b'?') {
            Ok(Token::QuestionMark)
        } else if self.eat_byte(b':') {
            Ok(Token::Colon)
        } else if self.eat_byte(b';') {
            Ok(Token::Semi)
        } else if self.eat_byte(b'+') {
            if self.eat_byte(b'+') {
                Ok(Token::PlusPlus)
            } else if self.eat_byte(b'=') {
                Ok(Token::PlusEq)
            } else {
                Ok(Token::Plus)
            }
        } else if self.eat_byte(b'^') {
            if self.eat_byte(b'=') {
                Ok(Token::BitXorEq)
            } else {
                Ok(Token::BitXor)
            }
        } else if self.eat_byte(b'*') {
            if self.eat_byte(b'=') {
                Ok(Token::TimesEq)
            } else {
                Ok(Token::Times)
            }
        } else if self.eat_byte(b'/') {
            if self.eat_byte(b'=') {
                Ok(Token::DivEq)
            } else {
                Ok(Token::Div)
            }
        } else if self.eat_byte(b'%') {
            if self.eat_byte(b'=') {
                Ok(Token::ModEq)
            } else {
                Ok(Token::Mod)
            }
        } else if self.eat_byte(b',') {
            Ok(Token::Comma)
        } else if self.eat_byte(b'<') {
            if self.eat_byte(b'<') {
                if self.eat_byte(b'=') {
                    Ok(Token::LShiftEq)
                } else {
                    Ok(Token::LShift)
                }
            } else if self.eat_byte(b'=') {
                Ok(Token::Lte)
            } else {
                Ok(Token::Lt)
            }
        } else if self.eat_byte(b'>') {
            if self.eat_byte(b'>') {
                if self.eat_byte(b'>') {
                    if self.eat_byte(b'=') {
                        Ok(Token::URShiftEq)
                    } else {
                        Ok(Token::URShift)
                    }
                } else if self.eat_byte(b'=') {
                    Ok(Token::RShiftEq)
                } else {
                    Ok(Token::RShift)
                }
            } else if self.eat_byte(b'=') {
                Ok(Token::Gte)
            } else {
                Ok(Token::Gt)
            }
        } else if character == b'.' {
            match self.peek_byte() {
                Some(ch) if (ch as char).is_digit(10) => {
                    self.scan_number()
                }
                _ => {
                    self.next_byte();
                    Ok(Token::Dot)
               }
            }
        } else if self.eat_byte(b'!') {
            if self.eat_byte(b'=') {
                if self.eat_byte(b'=') {
                    Ok(Token::NotEqEq)
                } else {
                    Ok(Token::NotEq)
                }
            } else {
                Ok(Token::Bang)
            }
        } else if self.eat_byte(b'-') {
            if self.eat_byte(b'-') {
                Ok(Token::MinusMinus)
            } else if self.eat_byte(b'=') {
                Ok(Token::MinusEq)
            } else {
                Ok(Token::Minus)
            }
        } else if self.eat_byte(b'~') {
            Ok(Token::Tilde)
        } else if self.eat_byte(b'&') {
            if self.eat_byte(b'&') {
                Ok(Token::LogicalAnd)
            } else if self.eat_byte(b'=') {
                Ok(Token::BitAndEq)
            } else {
                Ok(Token::BitAnd)
            }
        } else if self.eat_byte(b'|') {
            if self.eat_byte(b'|') {
                Ok(Token::LogicalOr)
            } else if self.eat_byte(b'=') {
                Ok(Token::BitOrEq)
            } else {
                Ok(Token::BitOr)
            }
        } else if self.current_char().unwrap().is_es_identifier_start() {
            Ok(self.scan_identifier())
        } else {
            Err(self.invalid_token())
        }
    }

    fn scan_number(&mut self) -> Result<Token> {
        let nr = match (self.current_byte(), self.peek_byte()) {
            (Some(b'0'), Some(b'x')) | (Some(b'0'), Some(b'X')) => {
                self.scan_hex()?
            }
            _ => self.scan_float()?
        };

        match self.current_char() {
            Some(ch) if ch.is_es_identifier_start() => {
                Err(self.invalid_token())
            },
            _ => Ok(nr)
        }
    }

    fn scan_hex(&mut self) -> Result<Token> {
        self.next_byte();self.next_byte();
        let start = self.index;
        self.take_while(|c| (c as char).is_digit(16));
        let hex = unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]) };

        match u32::from_str_radix(hex, 16) {
            Ok(val) => Ok(Token::Number(val as f64)),
            Err(_) => Err(self.invalid_token())
        }
    }

    fn scan_float(&mut self) -> Result<Token> {
        let start = self.index;

        if self.eat_byte(b'0') {
            let mut is_octal = true;
            self.take_while(|c| {
                is_octal = is_octal && (c as char).is_digit(8);
                (c as char).is_digit(10)
            });

            if is_octal {
                let number = unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]) };
                return Ok(Token::Number(i32::from_str_radix(number, 8).unwrap() as f64));
            }
        } else {
            self.take_while(|c| (c as char).is_digit(10));
        }

        if self.eat_byte(b'.') {
            self.take_while(|c| (c as char).is_digit(10));
        }

        if self.eat_byte(b'e') {
            self.eat_byte(b'+') || self.eat_byte(b'-');

            match self.current_byte() {
                Some(ch) if (ch as char).is_digit(10) => {
                    self.take_while(|c| (c as char).is_digit(10));
                }
                _ => return Err(self.invalid_token())
            }
        }

        let float = unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]) }.parse().unwrap();
        Ok(Token::Number(float))
    }

    fn scan_string(&mut self, quote: u8) -> Result<Token> {
        let start = self.index;
        self.eat_byte(quote);

        while let Some(ch) = self.next_char() {
            if ch == (quote as char) {
                break;
            } else if ch == '\\' {
                match self.next_char() {
                    Some(ch) if ch.is_digit(8) => {
                        let mut code = 0;
                        for _ in 0..3 {
                            match self.current_char() {
                                Some(ch) if ch.is_digit(8) => {
                                    let new_code = (code * 8) + ch.to_digit(8).unwrap();
                                    if new_code > 255 {
                                        break;
                                    }
                                    self.next_char();
                                    code = new_code;
                                },
                                _ => { break; }
                            }
                        }
                    },
                    Some('\\') => continue,
                    Some('\'') => continue,
                    Some('"') => continue,
                    Some('n') => continue,
                    Some('r') => continue,
                    Some('t') => continue,
                    Some('b') => continue,
                    Some('v') => continue,
                    Some('f') => continue,
                    Some('x') => {
                        self.scan_hex_digit()?;
                        self.scan_hex_digit()?;
                        continue;
                    },
                    Some(ch) if ch.is_es_newline() => {
                        if ch == '\r' && self.current_byte() == Some(b'\n')  {
                            self.eat_byte(b'\n');
                        }
                        self.line += 1;
                        self.column = 0;
                    },
                    Some('8') | Some('9') => {
                        return Err(self.invalid_token());
                    }
                    _ => return Err(self.invalid_token_at(self.lookahead_start))
                }
            } else if ch.is_es_newline() {
                return Err(self.invalid_token_at(self.lookahead_start));
            } else {
                continue;
            }
        }

        let string = unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]).to_string() };
        Ok(Token::String(string))
    }

    fn scan_hex_digit(&mut self) -> Result<u32> {
        match self.current_char() {
            Some(ch) if ch.is_digit(16) => {
                self.next_char();
                Ok(ch.to_digit(16).unwrap())
            },
            _ => Err(CompileError::new(self.pos(), ErrorCause::InvalidHexEscape))
        }
    }

    fn scan_identifier(&mut self) -> Token {
        let value = self.get_identifier();

        if value == *KEYWORD_VAR {
            Token::Var
        } else if value == *KEYWORD_FUNCTION {
            Token::FunctionKeyword
        } else if value == *KEYWORD_THIS {
            Token::ThisKeyword
        } else if value == *KEYWORD_NULL {
            Token::Null
        } else if value == *KEYWORD_IF {
            Token::If
        } else if value == *KEYWORD_ELSE {
            Token::Else
        } else if value == *KEYWORD_NEW {
            Token::New
        } else if value == *KEYWORD_VOID {
            Token::Void
        } else if value == *KEYWORD_DELETE {
            Token::Delete
        } else if value == *KEYWORD_TYPEOF {
            Token::Typeof
        } else if value == *KEYWORD_IN {
            Token::In
        } else if value == *KEYWORD_INSTANCEOF {
            Token::Instanceof
        } else if value == *KEYWORD_RETURN {
            Token::Return
        } else if value == *KEYWORD_DEBUGGER {
            Token::DebuggerKeyword
        } else if value == *KEYWORD_THROW {
            Token::ThrowKeyword
        } else if value == *KEYWORD_TRY {
            Token::TryKeyword
        } else if value == *KEYWORD_CATCH {
            Token::CatchKeyword
        } else if value == *KEYWORD_FINALLY {
            Token::FinallyKeyword
        } else if value == *KEYWORD_SWITCH {
            Token::SwitchKeyword
        } else if value == *KEYWORD_CASE {
            Token::CaseKeyword
        } else if value == *KEYWORD_DEFAULT {
            Token::DefaultKeyword
        } else if value == *KEYWORD_BREAK {
            Token::BreakKeyword
        } else if value == *KEYWORD_DO {
            Token::DoKeyword
        } else if value == *KEYWORD_WHILE {
            Token::WhileKeyword
        } else if value == *KEYWORD_FOR {
            Token::ForKeyword
        } else if value == *KEYWORD_WITH {
            Token::WithKeyword
        } else if value == *KEYWORD_CONTINUE {
            Token::ContinueKeyword
        } else if value == *BOOL_TRUE {
            Token::BoolTrue
        } else if value == *BOOL_FALSE {
            Token::BoolFalse
        } else {
            Token::Ident(value)
        }
    }

    fn get_identifier(&mut self) -> String {
        let start = self.index;
        self.take_chars_while(|c| (c as char).is_es_identifier_continue());
        unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]).to_string() }
    }

    fn take_while<F>(&mut self, mut predicate: F) where F: FnMut(u8) -> bool {
        while let Some(ch) = self.current_byte() {
            if predicate(ch) {
                self.next_byte();
            } else {
                break;
            }
        }
    }

    fn take_chars_while<F>(&mut self, predicate: F) where F: Fn(char) -> bool {
        while let Some(ch) = self.current_char() {
            if predicate(ch) {
                self.next_char();
            } else {
                break;
            }
        }
    }

    fn current_byte(&self) -> Option<u8> {
        self.get_byte(self.index)
    }

    fn peek_byte(&self) -> Option<u8> {
        self.get_byte(self.index + 1)
    }

    fn get_byte(&self, index: usize) -> Option<u8> {
        if index < self.bytes.len() {
            Some(self.bytes[index])
        } else {
            None
        }
    }

    fn current_char(&mut self) -> Option<char> {
      unsafe { str::from_utf8_unchecked(&self.bytes[self.index..]) }.chars().next()
    }

    fn next_byte(&mut self) -> Option<u8> {
        self.current_byte().map(|b| {
            debug_assert!(b < 128);
            self.index += 1;
            self.column += 1;
            b
        })
    }

    fn eat_byte(&mut self, byte: u8) -> bool {
        if let Some(b) = self.current_byte() {
            if b == byte {
                self.next_byte();
                return true
            }
            return false;
        }
        false
    }

    fn next_char(&mut self) -> Option<char> {
        unsafe { str::from_utf8_unchecked(&self.bytes[self.index..]) }.chars().next().map(|c| {
            self.index += c.len_utf8();
            self.column += 1;
            c
        })
    }

    fn invalid_token(&mut self) -> CompileError {
        self.invalid_token_at(self.pos())
    }

    fn invalid_token_at(&self, pos: Position) -> CompileError {
        CompileError::new(pos, ErrorCause::IllegalToken)
    }
}
