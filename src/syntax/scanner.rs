use interner;
use syntax::char::ESCharExt;
use syntax::span::Position;
use syntax::token::Token;
use syntax::ast::Literal;
use errors::{CompileError, ErrorCause, Result};
use std::str;
use std::mem;
use std::char;

pub struct Scanner<'a> {
    bytes: &'a [u8],
    index: usize,
    line: u32,
    column: u32,
    pub last_pos: Position,
    pub lookahead_start: Position,
    pub lookahead: Token,
}

pub struct ScannerState(usize, u32, u32, Position, Position, Token);

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
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

    pub fn save_state(&self) -> ScannerState {
        ScannerState(self.index, self.line, self.column, self.last_pos, self.lookahead_start, self.lookahead)
    }

    pub fn restore(&mut self, state: ScannerState) {
        let ScannerState(index, line, column, last_pos, lookahead_start, lookahead) = state;
        self.index = index;
        self.line = line;
        self.column = column;
        self.last_pos = last_pos;
        self.lookahead_start = lookahead_start;
        self.lookahead = lookahead;
    }

    pub fn at_newline(&self) -> bool {
        self.last_pos.line != self.lookahead_start.line
    }

    pub fn is_eof(&self) -> bool {
        self.lookahead == Token::Eof
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

    pub fn regex_token(&mut self) -> Result<Literal> {
        debug_assert!(
            self.lookahead == Token::Div || self.lookahead == Token::DivEq,
            "Regex must start with a forward slash"
        );
        let regex_token = self.scan_regex()?;
        self.last_pos = self.pos();
        let next_token = self.lex()?;
        mem::replace(&mut self.lookahead, next_token);
        Ok(regex_token)
    }

    pub fn pos(&self) -> Position {
        Position { column: self.column, line: self.line }
    }

    fn skip_single_line_comment(&mut self, offset: u8) {
        for _x in 0..offset {
            self.next_byte();
        };

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
        let start = self.column == 0;

        let character = loop {
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
                Some(b'\t') => {
                    self.next_byte();
                },
                Some(b'/') => {
                    match self.peek_byte() {
                        Some(b'/') => self.skip_single_line_comment(2),
                        Some(b'*') => self.skip_multi_line_comment()?,
                        _ => {
                            break b'/';
                        }
                    }
                }
                Some(b'<') => {
                    match self.peek_3() {
                        (Some(b'!'), Some(b'-'), Some(b'-')) => {
                            self.skip_single_line_comment(4);
                        }
                        _ => {
                            break b'<';
                        }
                    }
                },
                Some(b'-') => {
                    match self.peek_2() {
                        (Some(b'-'), Some(b'>')) if start => {
                            self.skip_single_line_comment(3);
                        }
                        _ => {
                            break b'-';
                        }
                    }
                },
                Some(c) => {
                    break c;
                }
                None => {
                    self.lookahead_start = self.pos();
                    return Ok(Token::Eof)
                }
            };
        };

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

            } else if self.eat_byte(b'>') {
                Ok(Token::Arrow)
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
                Ok(Token::Star)
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
                Some(b'.') => {
                    match self.peek_2() {
                        (Some(b'.'), Some(b'.')) => {
                            self.next_byte();
                            self.next_byte();
                            self.next_byte();
                            Ok(Token::Ellipsis)
                        }
                        _ => {
                            self.next_byte();
                            Ok(Token::Dot)
                        }
                    }
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
                self.scan_radix(16)?
            }
            (Some(b'0'), Some(b'o')) | (Some(b'0'), Some(b'O')) => {
                self.scan_radix(8)?
            }
            (Some(b'0'), Some(b'b')) | (Some(b'0'), Some(b'B')) => {
                self.scan_radix(2)?
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

    fn scan_radix(&mut self, base: u32) -> Result<Token> {
        self.next_byte();self.next_byte();
        let start = self.index;
        self.take_while(|c| (c as char).is_digit(base));

        if self.index == start {
            return Err(self.invalid_token())
        }

        if let Some(b) = self.current_byte() {
            if (b as char).is_digit(10) {
                return Err(self.invalid_token())
            }
        }

        let hex = unsafe { str::from_utf8_unchecked(&self.bytes[start..self.index]) };

        match u32::from_str_radix(hex, base) {
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
        let mut terminated = false;
        let mut value = String::new();
        self.eat_byte(quote);

        while let Some(ch) = self.next_char() {
            if ch == (quote as char) {
                terminated = true;
                break;
            } else if ch == '\\' {
                match self.next_char() {
                    Some('\\') => value.push('\\'),
                    Some('\'') => value.push('\''),
                    Some('"') => value.push('"'),
                    Some('n') => value.push('\n'),
                    Some('r') => value.push('\r'),
                    Some('t') => value.push('\t'),
                    Some('b') => value.push('\u{0008}'),
                    Some('v') => value.push('\u{000B}'),
                    Some('f') => value.push('\u{000C}'),
                    Some(ch) if ch.is_digit(8) => {
                        let mut code = ch.to_digit(8).unwrap();
                        for _ in 0..2 {
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
                        value.push(char::from_u32(code).unwrap_or('?'));
                    },
                    Some('x') => {
                        let mut code = 0;
                        code += self.scan_hex_digit()? * 16;
                        code += self.scan_hex_digit()?;
                        value.push(char::from_u32(code).unwrap_or('?'));
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
                    _ => value.push(ch)
                }
            } else if ch.is_es_newline() {
                return Err(self.invalid_token_at(self.lookahead_start));
            } else {
                value.push(ch);
            }
        }

        if !terminated {
            return Err(self.invalid_token_at(self.lookahead_start));
        }

        let raw = unsafe { str::from_utf8_unchecked(self.bytes.get_unchecked(start..self.index)) };
        Ok(Token::String(interner::intern(raw), interner::intern(&value)))
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

    fn scan_regex_pattern(&mut self) -> Result<interner::Symbol> {
        let start = match self.lookahead {
            Token::Div => self.index,
            Token::DivEq => self.index - 1,
            _ => unreachable!()
        };

        let mut class_marker = false;
        let mut terminated = false;

        while let Some(ch) = self.next_char() {
            if ch == '\\' {
                let next = self.next_char();
                match next {
                    Some(c) if !c.is_es_newline() => continue,
                    _ => return Err(CompileError::new(self.pos(), ErrorCause::UnterminatedRegex))
                }
            } else if ch.is_es_newline() {
                return Err(CompileError::new(self.pos(), ErrorCause::UnterminatedRegex))
            } else if class_marker {
                if ch == ']' {
                    class_marker = false;
                }
            } else if ch == '/' {
                terminated = true;
                break;
            } else if ch == '[' {
                class_marker = true;
            }
        }

        if !terminated {
            return Err(CompileError::new(self.pos(), ErrorCause::UnterminatedRegex))
        }

        let string = unsafe { str::from_utf8_unchecked(self.bytes.get_unchecked(start..self.index - 1)) };
        Ok(interner::intern(string))
    }

    fn scan_regex_flags(&mut self) -> Result<Vec<char>> {
        let mut flags = Vec::new();
        while let Some(ch) = self.current_char() {
            if !ch.is_es_identifier_continue() {
                break;
            }

            flags.push(ch);
            self.next_char();
        };

        Ok(flags)
    }

    fn scan_regex(&mut self) -> Result<Literal> {
        let pattern = self.scan_regex_pattern()?;
        let flags = self.scan_regex_flags()?;
        Ok(Literal::Regex(pattern, flags))
    }

    fn scan_identifier(&mut self) -> Token {
        let start = self.index;
        self.take_chars_while(char::is_es_identifier_continue);
        let value = unsafe { str::from_utf8_unchecked(self.bytes.get_unchecked(start..self.index)) };

        match value {
            "break" => Token::BreakKeyword,
            "case" => Token::CaseKeyword,
            "catch" => Token::CatchKeyword,
            "class" => Token::ClassKeyword,
            "const" => Token::Const,
            "continue" => Token::ContinueKeyword,
            "debugger" => Token::DebuggerKeyword,
            "default" => Token::DefaultKeyword,
            "delete" => Token::Delete,
            "do" => Token::DoKeyword,
            "else" => Token::Else,
            "enum" => Token::EnumKeyword,
            "extends" => Token::ExtendsKeyword,
            "false" => Token::BoolFalse,
            "finally" => Token::FinallyKeyword,
            "for" => Token::ForKeyword,
            "function" => Token::FunctionKeyword,
            "if" => Token::If,
            "in" => Token::In,
            "instanceof" => Token::Instanceof,
            "new" => Token::New,
            "null" => Token::Null,
            "return" => Token::Return,
            "super" => Token::SuperKeyword,
            "switch" => Token::SwitchKeyword,
            "this" => Token::ThisKeyword,
            "throw" => Token::ThrowKeyword,
            "true" => Token::BoolTrue,
            "try" => Token::TryKeyword,
            "typeof" => Token::Typeof,
            "var" => Token::Var,
            "void" => Token::Void,
            "while" => Token::WhileKeyword,
            "with" => Token::WithKeyword,
            "yield" => Token::YieldKeyword,
            "export" => Token::ExportKeyword,
            "import" => Token::ImportKeyword,
            _ => {
                Token::Ident(interner::intern(value))
            }
        }
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

    fn peek_3(&self) -> (Option<u8>, Option<u8>, Option<u8>) {
        (
            self.get_byte(self.index + 1),
            self.get_byte(self.index + 2),
            self.get_byte(self.index + 3)
        )
    }

    fn peek_2(&self) -> (Option<u8>, Option<u8>) {
        (
            self.get_byte(self.index + 1),
            self.get_byte(self.index + 2)
        )
    }

    fn get_byte(&self, index: usize) -> Option<u8> {
        if index < self.bytes.len() {
            unsafe { Some(*self.bytes.get_unchecked(index)) }
        } else {
            None
        }
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
        match self.current_byte() {
            Some(b) if b == byte => {
                self.next_byte();
                true
            }
            _ => false
        }
    }

    fn current_char(&mut self) -> Option<char> {
      unsafe { str::from_utf8_unchecked(&self.bytes[self.index..]) }.chars().next()
    }


    fn next_char(&mut self) -> Option<char> {
        self.current_char().map(|c| {
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
