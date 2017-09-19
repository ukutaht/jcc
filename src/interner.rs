use string_interner::DefaultStringInterner;
use std::sync::{RwLock, RwLockReadGuard};

pub type Symbol = usize;

lazy_static! {
    static ref INTERNER: RwLock<DefaultStringInterner> = {
        RwLock::new(DefaultStringInterner::default())
    };

    pub static ref KEYWORD_NULL: Symbol = { intern("null") };
    pub static ref KEYWORD_IF: Symbol = { intern("if") };
    pub static ref KEYWORD_ELSE: Symbol = { intern("else") };
    pub static ref KEYWORD_IN: Symbol = { intern("in") };
    pub static ref KEYWORD_TRUE: Symbol = { intern("true") };
    pub static ref KEYWORD_FALSE: Symbol = { intern("false") };
    pub static ref KEYWORD_GET: Symbol = { intern("get") };
    pub static ref KEYWORD_SET: Symbol = { intern("set") };
    pub static ref KEYWORD_PROTO: Symbol = { intern("__proto__") };

    pub static ref RESERVED_ENUM: Symbol = { intern("enum") };
    pub static ref RESERVED_IMPORT: Symbol = { intern("import") };
    pub static ref RESERVED_EXPORT: Symbol = { intern("export") };
    pub static ref RESERVED_SUPER: Symbol = { intern("super") };
    pub static ref RESERVED_EVAL: Symbol = { intern("eval") };
    pub static ref RESERVED_ARGUMENTS: Symbol = { intern("arguments") };
    pub static ref RESERVED_IMPLEMENTS: Symbol = { intern("implements") };
    pub static ref RESERVED_INTERFACE: Symbol = { intern("interface") };
    pub static ref RESERVED_PACKAGE: Symbol = { intern("package") };
    pub static ref RESERVED_PRIVATE: Symbol = { intern("private") };
    pub static ref RESERVED_PROTECTED: Symbol = { intern("protected") };
    pub static ref RESERVED_PUBLIC: Symbol = { intern("public") };
    pub static ref RESERVED_STATIC: Symbol = { intern("static") };
    pub static ref RESERVED_YIELD: Symbol = { intern("yield") };
    pub static ref RESERVED_LET: Symbol = { intern("let") };
}

pub fn intern(val: &str) -> Symbol {
    INTERNER.write().expect("RwLock failed").get_or_intern(val)
}

pub fn read<'a>() -> RwLockReadGuard<'a, DefaultStringInterner> {
    INTERNER.read().expect("RwLock failed")
}
