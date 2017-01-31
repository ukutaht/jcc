extern crate jcc;

#[test]
fn transforms_named_function() {
    let result = jcc::transform("function hello() { var a = 1 }").unwrap();

    assert_eq!(result, "function hello() { var a = 1 }".to_owned());
}

#[test]
fn transforms_unnamed_function() {
    let result = jcc::transform("function() { var a = 1 }").unwrap();

    assert_eq!(result, "function () { var a = 1 }".to_owned());
}

#[test]
fn transforms_named_function_with_single_parameter() {
    let result = jcc::transform("function hello(a) { var a = 1 }").unwrap();

    assert_eq!(result, "function hello(a) { var a = 1 }".to_owned());
}

#[test]
fn transforms_named_function_with_multiple_parameters() {
    let result = jcc::transform("function hello(a, b) { var a = 1 }").unwrap();

    assert_eq!(result, "function hello(a, b) { var a = 1 }".to_owned());
}
