use std::sync::{RwLock, RwLockReadGuard};
use std::ops::Deref;
use fnv::FnvHashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Symbol(u32);

pub struct Interner {
    names: FnvHashMap<Box<str>, Symbol>,
    strings: Vec<Box<str>>,
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            names: FnvHashMap::default(),
            strings: Vec::new()
        }
    }

    fn prefill(init: &[&str]) -> Self {
        let mut this = Interner::new();
        for &string in init {
            this.intern(string);
        }
        this
    }

    pub fn intern(&mut self, string: &str) -> Symbol {
        if let Some(&name) = self.names.get(string) {
            return name;
        }

        let name = Symbol(self.strings.len() as u32);
        let string = string.to_string().into_boxed_str();
        self.strings.push(string.clone());
        self.names.insert(string, name);
        name
    }

    pub fn get(&self, symbol: Symbol) -> &str {
        unsafe {self.strings.get_unchecked(symbol.0 as usize) }
    }
}

pub struct LookupRef<'a>(RwLockReadGuard<'a, Interner>, Symbol);

impl<'a> Deref for LookupRef<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.0.get(self.1)
    }
}

lazy_static! {
    static ref INTERNER: RwLock<Interner> = {
        RwLock::new(Interner::new())
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
    INTERNER.write().expect("RwLock failed").intern(val)
}

pub fn resolve<'a>(key: Symbol) -> LookupRef<'a> {
    LookupRef(INTERNER.read().expect("RwLock failed"), key)
}
