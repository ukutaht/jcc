#![cfg(test)]

extern crate jcc;
extern crate test;
extern crate glob;

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use glob::glob;
use test::{Bencher, TestDesc, TestDescAndFn, TestName, TestFn, TDynBenchFn, test_main};
use test::ShouldPanic::No;

struct DynBenchFn<F> {
    run: F
}

impl<F: Fn(&mut Bencher) + Send + 'static> TDynBenchFn for DynBenchFn<F> {
    fn run(&self, harness: &mut Bencher) {
        (self.run)(harness);
    }
}

fn add_bench<F: Fn(&mut Bencher) + Send + 'static>(tests: &mut Vec<TestDescAndFn>, name: String, f: F) {
    tests.push(TestDescAndFn {
        desc: TestDesc {
            name: TestName::DynTestName(name),
            ignore: false,
            should_panic: No
        },
        testfn: TestFn::DynBenchFn(Box::new(DynBenchFn { run: f }))
    });
}

fn parse_benches(target: &mut Vec<TestDescAndFn>) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let base = root.join(Path::new("benches/fixtures"));

    let files = glob(&format!("{}/**/*.js", base.to_str().unwrap())).unwrap().filter_map(|f| {
        f.ok()
    });

    for path in files {
         let name = path.strip_prefix(&root).unwrap().to_str().unwrap().to_string();
        add_bench(target, name, move |bench| {
            let mut source = String::new();
            File::open(&path).unwrap().read_to_string(&mut source).unwrap();

            bench.iter(|| {
                test::black_box(jcc::parse(&source))
            })
        });

    };
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut benches = Vec::new();
    parse_benches(&mut benches);
    test_main(&args, benches);
}
