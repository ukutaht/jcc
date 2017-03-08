#![cfg(test)]

extern crate jcc;
extern crate test;
extern crate glob;
extern crate serde_json;

mod json;
use std::env;
use std::fs::{File, read_dir};
use std::io::Read;
use std::path::Path;
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

fn unit_tests(target: &mut Vec<TestDescAndFn>) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let fixtures = root.join(Path::new("tests/esprima-fixtures"));

    let testignore: Vec<_> =
        include_str!(".esprima-ignore")
        .lines()
        .filter(|s| !s.is_empty() && !s.starts_with("#"))
        .map(|s| glob::Pattern::new(s).unwrap())
        .collect();

    let files =
        read_dir(fixtures.as_path()).unwrap()
        .flat_map(|dir| glob(&format!("{}/**/*.tree.json", dir.unwrap().path().to_str().unwrap())).unwrap())
        .filter_map(|entry| {
            let tree_path = entry.unwrap();
            let source_path = {
                let tree_file_name = tree_path.file_name().unwrap().to_str().unwrap();
                let source_file_name = tree_file_name[..tree_file_name.len() - 9].to_string() + "js";
                tree_path.with_file_name(source_file_name)
            };
            if source_path.exists() {
                let ignore = {
                    let local_test_path = source_path.strip_prefix(&fixtures).unwrap().with_extension("");
                    testignore.iter().any(|ignore| ignore.matches_path(&local_test_path))
                };
                Some((tree_path, source_path, ignore))
            } else {
                None
            }
        });

    for (tree_path, source_path, ignore) in files {
        add_test(target, source_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string(), ignore, move || {
            let expected_json: Value = serde_json::de::from_reader(File::open(tree_path).unwrap()).unwrap();
            let expected = json::parse_program(&expected_json);
            let mut source = String::new();
            File::open(source_path).unwrap().read_to_string(&mut source).unwrap();
            match (jcc::parse(&source[..]), expected) {
                (Ok(actual_ast), expected) => {
                    assert!(actual_ast == expected, "unit test got wrong result\n\
                    expected: {:#?}\n\
                    actual AST: {:#?}", expected, actual_ast);
                }
                (Err(actual_err), _) => {
                    assert!(false, "unit test failed to parse:\n{:#?}", actual_err);
                }
            }
        });
    }
}

fn trans_tests(target: &mut Vec<TestDescAndFn>) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let fixtures = root.join(Path::new("tests/trans-fixtures"));

    let files =
        read_dir(fixtures.as_path()).unwrap()
        .flat_map(|dir| glob(&format!("{}/**/*.out.js", dir.unwrap().path().to_str().unwrap())).unwrap())
        .filter_map(|entry| {
            let tree_path = entry.unwrap();
            let source_path = {
                let tree_file_name = tree_path.file_name().unwrap().to_str().unwrap();
                let source_file_name = tree_file_name[..tree_file_name.len() - 6].to_string() + "js";
                tree_path.with_file_name(source_file_name)
            };
            if source_path.exists() {
                let ignore = false;
                Some((tree_path, source_path, ignore))
            } else {
                None
            }
        });

    for (out_path, source_path, ignore) in files {
        add_test(target, source_path.strip_prefix(&root).unwrap().to_str().unwrap().to_string(), ignore, move || {
            let mut expected = String::new();
            File::open(out_path).unwrap().read_to_string(&mut expected).unwrap();
            let mut source = String::new();
            File::open(source_path).unwrap().read_to_string(&mut source).unwrap();
            match (jcc::transform(&source[..]), expected) {
                (Ok(out), expected) => {
                    assert!(out == expected, "trans test got wrong result\n\
                    expected: {:#?}\n\
                    actual AST: {:#?}", expected, out);
                }
                (Err(actual_err), _) => {
                    assert!(false, "unit test failed to parse:\n{:#?}", actual_err);
                }
            }
        });
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    unit_tests(&mut tests);
    trans_tests(&mut tests);
    test_main(&args, tests);
}
