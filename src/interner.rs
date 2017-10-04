use string_interner::StringInterner;
use std::sync::{RwLock, RwLockReadGuard};
use std::ops::Deref;
use fnv::FnvBuildHasher;

pub type Symbol = usize;
pub type Interner = StringInterner<Symbol, FnvBuildHasher>;

pub struct LookupRef<'a>(RwLockReadGuard<'a, Interner>, Symbol);

impl<'a> Deref for LookupRef<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.resolve_unchecked(self.1) }
    }
}

lazy_static! {
    static ref INTERNER: RwLock<Interner> = {
        RwLock::new(StringInterner::with_hasher(FnvBuildHasher::default()))
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
    pub static ref KEYWORD_YIELD: Symbol = { intern("yield") };

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
    pub static ref RESERVED_CONST: Symbol = { intern("const") };
    pub static ref RESERVED_OF: Symbol = { intern("of") };
    pub static ref RESERVED_CONSTRUCTOR: Symbol = { intern("constructor") };
    pub static ref RESERVED_PROTOTYPE: Symbol = { intern("prototype") };
    pub static ref DIRECTIVE_USE_STRICT: Symbol = { intern("use strict") };
}

pub fn intern(val: &str) -> Symbol {
    INTERNER.write().expect("RwLock failed").get_or_intern(val)
}

pub fn resolve<'a>(key: Symbol) -> LookupRef<'a> {
    LookupRef(INTERNER.read().expect("RwLock failed"), key)
}
