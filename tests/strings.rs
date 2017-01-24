extern crate jcc;

#[test]
fn transforms_plain_string_literal() {
    let result = jcc::transform("\"Hello\"").unwrap();

    assert_eq!(result, "\"Hello\"".to_owned());
}
