extern crate jcc;

#[test]
fn transforms_empty_array() {
    let result = jcc::transform("[]").unwrap();

    assert_eq!(result, "[]".to_owned());
}

#[test]
fn transforms_array_with_elements() {
    let result = jcc::transform("[1, \"Hello\", some_var]").unwrap();

    assert_eq!(result, "[1, \"Hello\", some_var]".to_owned());
}
