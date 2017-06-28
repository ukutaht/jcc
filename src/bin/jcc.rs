extern crate jcc;

use std::env;
use std::io::prelude::*;
use std::fs::File;

fn parse(filename: &str) {
    let mut f = File::open(filename).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();

    let ast = jcc::parse(&buffer);

    match ast {
        Ok(code) => println!("{:#?}", code),
        Err(err) => panic!("{}", err),
    }
}

fn transpile(filename: &str) {
    let mut f = File::open(filename).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();

    let transformed = jcc::transform(&buffer);

    match transformed {
        Ok(code) => println!("{}", code),
        Err(err) => panic!("{:?}", err),
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    match args[1].as_ref() {
        "parse" => parse(&args[2]),
        filename => transpile(filename)
    }
}
