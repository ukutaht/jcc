#![feature(test)]
extern crate test;
extern crate jcc;
use test::Bencher;

#[bench]
fn bench_named_function(b: &mut Bencher) {
    b.iter(|| jcc::transform("function hello() { var a = 1 }"));
}

#[bench]
fn bench_named_function_with_arguments(b: &mut Bencher) {
    b.iter(|| jcc::transform("function hello(a, b) { var c = 1 }"));
}
