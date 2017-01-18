extern crate jcc;

use std::env;
use std::io::prelude::*;
use std::fs::File;

fn main() {
    let args: Vec<_> = env::args().collect();
    let filename = &args[1];

    let mut f = File::open(filename).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();

    let transformed = jcc::transform(&buffer);

    match transformed {
        Ok(code) => println!("{}", code),
        Err(err) => panic!("{:?}", err),
    }
}
