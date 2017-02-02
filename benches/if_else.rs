#![feature(test)]
extern crate test;
extern crate jcc;
use test::Bencher;

#[bench]
fn bench_if_statement(b: &mut Bencher) {
    b.iter(|| jcc::transform("if (true) { something() }"));
}

#[bench]
fn bench_if_statement_with_else(b: &mut Bencher) {
    b.iter(|| jcc::transform("if (true) { something() } else { somethingElse() }"));
}
