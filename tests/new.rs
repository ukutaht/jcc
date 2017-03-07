extern crate jcc;

#[test]
fn simple_new() {
    let result = jcc::transform("new Error").unwrap();

    assert_eq!(result, "new Error();".to_owned());
}

#[test]
fn simple_new_with_arguments() {
    let result = jcc::transform("new Error(1, 2)").unwrap();

    assert_eq!(result, "new Error(1, 2);".to_owned());
}
