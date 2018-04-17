use unic_ucd_ident;

pub trait ESCharExt {
    fn is_es_newline(self) -> bool;
    fn is_es_identifier_start(self) -> bool;
    fn is_es_identifier_continue(self) -> bool;
}

impl ESCharExt for char {
    fn is_es_newline(self) -> bool {
        match self {
            '\u{000a}' | '\u{000d}' | '\u{2028}' | '\u{2029}' => true,
            _ => false,
        }
    }

    fn is_es_identifier_start(self) -> bool {
        match self {
            '$' | '_' => true,
            c => unic_ucd_ident::is_id_start(c),
        }
    }

    fn is_es_identifier_continue(self) -> bool {
        match self {
            '$' | '_' => true,
            c => unic_ucd_ident::is_id_continue(c),
        }
    }
}

#[cfg(test)]
mod tests {
    use syntax::char::ESCharExt;

    #[test]
    fn line_break_is_not_identifier_continue() {
        let slashn: char = '\n';
        assert!(!slashn.is_es_identifier_continue());

        let slashr: char = '\r';
        assert!(!slashr.is_es_identifier_continue());

    }

    #[test]
    fn end_of_line() {
        let eof: char = 26u8 as char;
        assert!(!eof.is_es_identifier_continue());
    }
}
