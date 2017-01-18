use std::collections::HashMap;
use std::fmt;
use std::sync::RwLock;
use std::mem;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Name(u32);

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.to_string())
    }
}

impl Name {
    pub fn to_string(&self) -> &'static str {
        INTERNER.read().unwrap().get(self)
    }
}

struct Interner {
    names: HashMap<&'static str, Name>,
    strings: Vec<&'static str>,
}

impl Interner {
    fn new() -> Self {
        Interner {
            names: HashMap::new(),
            strings: Vec::new(),
        }
    }

    fn intern(&mut self, string: &str) -> Name {
        if let Some(&name) = self.names.get(string) {
            return name;
        }

        let static_str: &'static str = unsafe { mem::transmute(string) };
        let name = Name(self.strings.len() as u32);
        self.strings.push(static_str);
        self.names.insert(static_str, name);
        name
    }

    fn get(&self, name: &Name) -> &'static str {
        self.strings[name.0 as usize]
    }
}

lazy_static! {
    static ref INTERNER: RwLock<Interner> = RwLock::new(Interner::new());
}

pub fn intern(string: &str) -> Name {
    INTERNER.write().unwrap().intern(string)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interner_tests() {
        assert_eq!(intern("dog"), intern("dog"));
        assert!(intern("dog") != intern("cat"));
        assert_eq!(intern("dog").to_string(), "dog");
    }
}
