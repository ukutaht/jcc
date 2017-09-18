pub trait EsWord {
    fn is_future_reserved_word(&self) -> bool;
    fn is_restricted_word(&self) -> bool;
    fn is_strict_mode_reserved_word(&self) -> bool;
}

impl EsWord for str {
    fn is_future_reserved_word(&self) -> bool {
        match self {
            "enum" | "import" | "export" | "super" => true,
            _ => false
        }
    }

    fn is_restricted_word(&self) -> bool {
        match self {
            "eval" | "arguments" => true,
            _ => false
        }
    }

    fn is_strict_mode_reserved_word(&self) -> bool {
        match self {
            "implements" | "interface" | "package" | "private" | "protected" | "public" | "static" | "yield" | "let" => true,
            _ => false
        }
    }
}
