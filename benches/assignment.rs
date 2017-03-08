extern crate test;
extern crate jcc;
use test::Bencher;

#[bench]
fn bench_assignment(b: &mut Bencher) {
    b.iter(|| jcc::transform("var a = 1"));
}
