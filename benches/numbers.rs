extern crate test;
extern crate jcc;

use test::Bencher;

#[bench]
fn bench_single_digit_number(b: &mut Bencher) {
    b.iter(|| jcc::transform("1"));
}

#[bench]
fn bench_float_notation(b: &mut Bencher) {
    b.iter(|| jcc::transform("1.123"));
}
