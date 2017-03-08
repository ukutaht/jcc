extern crate test;
extern crate jcc;
use test::Bencher;

#[bench]
fn bench_plain_string_literal(b: &mut Bencher) {
    b.iter(|| jcc::transform("\"Hello\""));
}
