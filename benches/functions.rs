#![feature(test)]
extern crate test;
extern crate jcc;
use test::Bencher;

#[bench]
fn bench_named_function(b: &mut Bencher) {
    b.iter(|| jcc::transform("function hello() { var a = 1 }"));
}
