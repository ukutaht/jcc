use fnv::FnvHashMap;
use std::cell::RefCell;
use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Symbol(u32);

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self, self.0)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.as_str(), f)
    }
}

impl Symbol {
    pub fn as_str<'a>(self) -> &'a str {
        resolve(self)
    }
}

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

macro_rules! declare_keywords {(
    $( ($index: expr, $konst: ident, $string: expr) )*
) => {
    $(
        pub const $konst: Symbol = Symbol($index);
     )*

    impl Interner {
        fn fresh() -> Self {
            Interner::prefill(&[$($string,)*])
        }
    }
}}

declare_keywords! {
    (0,  KEYWORD_NULL,         "null")
    (1,  KEYWORD_IF,           "if")
    (2,  KEYWORD_ELSE,         "else")
    (3,  KEYWORD_IN,           "in")
    (4,  KEYWORD_TRUE,         "true")
    (5,  KEYWORD_FALSE,        "false")
    (6,  KEYWORD_GET,          "get")
    (7,  KEYWORD_SET,          "set")
    (8,  KEYWORD_PROTO,        "__proto__")
    (9,  KEYWORD_YIELD,        "yield")
    (10, RESERVED_ENUM,        "enum")
    (11, RESERVED_IMPORT,      "import")
    (12, RESERVED_EXPORT,      "export")
    (13, RESERVED_SUPER,       "super")
    (14, RESERVED_EVAL,        "eval")
    (15, RESERVED_ARGUMENTS,   "arguments")
    (16, RESERVED_IMPLEMENTS,  "implements")
    (17, RESERVED_INTERFACE,   "interface")
    (18, RESERVED_PACKAGE,     "package")
    (19, RESERVED_PRIVATE,     "private")
    (20, RESERVED_PROTECTED,   "protected")
    (21, RESERVED_PUBLIC,      "public")
    (22, RESERVED_STATIC,      "static")
    (23, RESERVED_LET,         "let")
    (24, RESERVED_CONST,       "const")
    (25, RESERVED_OF,          "of")
    (26, RESERVED_CONSTRUCTOR, "constructor")
    (27, RESERVED_PROTOTYPE,   "prototype")
    (28, DIRECTIVE_USE_STRICT, "use strict")
}

fn with_interner<T, F: FnOnce(&mut Interner) -> T>(f: F) -> T {
    thread_local!(static INTERNER: RefCell<Interner> = {
        RefCell::new(Interner::fresh())
    });
    INTERNER.with(|interner| f(&mut *interner.borrow_mut()))
}

pub fn intern(val: &str) -> Symbol {
    with_interner(|interner| interner.intern(val))
}

pub fn resolve<'a>(key: Symbol) -> &'a str {
    with_interner(|interner| {
        unsafe { ::std::mem::transmute::<&str, &str>(interner.get(key)) }
    })
}
