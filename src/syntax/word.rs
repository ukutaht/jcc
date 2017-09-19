use interner::*;

pub trait EsWord {
    fn is_future_reserved_word(&self) -> bool;
    fn is_restricted_word(&self) -> bool;
    fn is_strict_mode_reserved_word(&self) -> bool;
}

impl EsWord for Symbol {
    fn is_future_reserved_word(&self) -> bool {
        return *self == *RESERVED_ENUM
            || *self == *RESERVED_IMPORT
            || *self == *RESERVED_EXPORT
            || *self == *RESERVED_SUPER
    }

    fn is_restricted_word(&self) -> bool {
        return *self == *RESERVED_EVAL
            || *self == *RESERVED_ARGUMENTS
    }

    fn is_strict_mode_reserved_word(&self) -> bool {
        return *self == *RESERVED_IMPLEMENTS
            || *self == *RESERVED_INTERFACE
            || *self == *RESERVED_PACKAGE
            || *self == *RESERVED_PRIVATE
            || *self == *RESERVED_PROTECTED
            || *self == *RESERVED_PUBLIC
            || *self == *RESERVED_STATIC
            || *self == *RESERVED_YIELD
            || *self == *RESERVED_LET
    }
}
