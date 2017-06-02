#![cfg(test)]

extern crate jcc;
extern crate test;
extern crate glob;
extern crate serde_json;

mod json;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use glob::glob;
use test::{TestDesc, TestDescAndFn, TestName, TestFn, test_main};
use test::ShouldPanic::No;
use serde_json::value::Value;

fn add_test<F: FnOnce() + Send + 'static>(tests: &mut Vec<TestDescAndFn>, name: String, ignore: bool, f: F) {
    tests.push(TestDescAndFn {
        desc: TestDesc {
            name: TestName::DynTestName(name),
            ignore: ignore,
            should_panic: No
        },
        testfn: TestFn::dyn_test_fn(f)
    });
}

fn ignores_from(ignore_file: &str) -> Vec<glob::Pattern> {
    ignore_file.lines()
        .filter(|s| !s.is_empty() && !s.starts_with('#'))
        .map(|s| glob::Pattern::new(s).unwrap())
        .collect()
}

fn file_pairs(base_str: &str, pair_extension: &str, ignore_file: &str) -> Vec<(PathBuf, PathBuf, String, bool)> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let base = root.join(Path::new(base_str));
    let testignore = ignores_from(ignore_file);

    glob(&format!("{}/**/*{}", base.to_str().unwrap(), pair_extension)).unwrap()
        .filter_map(|entry| {
            let tree_path = entry.unwrap();
            let source_path = PathBuf::from(&tree_path.to_str().unwrap().replace(pair_extension, ".js"));
            if source_path.exists() {
                let name = source_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string();
                let ignore = {
                    let local_test_path = source_path.strip_prefix(&base).unwrap().with_extension("");
                    testignore.iter().any(|ignore| ignore.matches_path(&local_test_path))
                };
                Some((tree_path, source_path, name, ignore))
            } else {
                None
            }
        }).collect()
}

fn esprima_tests(target: &mut Vec<TestDescAndFn>) {
    let pairs = file_pairs("tests/esprima-fixtures", ".tree.json", include_str!("esprima-ignore"));

    for (tree_path, source_path, name, ignore) in pairs {
        add_test(target, name, ignore, move || {
            let expected_json: Value = serde_json::de::from_reader(File::open(tree_path).unwrap()).unwrap();
            let expected = json::parse_program(&expected_json);
            let mut source = String::new();
            File::open(source_path).unwrap().read_to_string(&mut source).unwrap();
            match (jcc::parse(&source[..]), expected) {
                (Ok(actual_ast), expected) => {
                    assert!(actual_ast == expected, "esprima test got wrong result\n\
                    expected: {:#?}\n\
                    actual AST: {:#?}", expected, actual_ast);
                }
                (Err(actual_err), _) => {
                    assert!(false, "esprima test failed to parse:\n{:#?}", actual_err);
                }
            }
        });
    }
}

fn trans_tests(target: &mut Vec<TestDescAndFn>) {
    for (out_path, source_path, name, _) in file_pairs("tests/trans-fixtures", ".out.js", "") {
        add_test(target, name, false, move || {
            let mut expected = String::new();
            File::open(out_path).unwrap().read_to_string(&mut expected).unwrap();
            let mut source = String::new();
            File::open(source_path).unwrap().read_to_string(&mut source).unwrap();
            match (jcc::transform(&source[..]), expected) {
                (Ok(out), expected) => {
                    assert!(out == expected, "trans test got wrong result\n\
                    expected: {:#?}\n\
                    actual: {:#?}", expected, out);
                }
                (Err(actual_err), _) => {
                    assert!(false, "trans test failed to parse:\n{:#?}", actual_err);
                }
            }
        });
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    esprima_tests(&mut tests);
    trans_tests(&mut tests);
    test_main(&args, tests);
}
