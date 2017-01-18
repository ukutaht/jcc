extern crate jcc;

#[test]
fn transforms_single_digit_number() {
    let result = jcc::transform("1");

    assert_eq!(result, Ok("1".to_owned()));
}

#[test]
fn transforms_float_notation() {
    let result = jcc::transform("1.123");

    assert_eq!(result, Ok("1.123".to_owned()));
}
