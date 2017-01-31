extern crate jcc;

#[test]
fn transforms_simple_function_call() {
    let result = jcc::transform("someFunction()").unwrap();

    assert_eq!(result, "someFunction()".to_owned());
}

#[test]
fn transforms_function_call_with_mixed_arguments() {
    let result = jcc::transform("someFunction(a, 1, \"Hello\")").unwrap();

    assert_eq!(result, "someFunction(a, 1, \"Hello\")".to_owned());
}
