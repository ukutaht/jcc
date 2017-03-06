extern crate jcc;

#[test]
fn transforms_not_op() {
    let result = jcc::transform("!whatever").unwrap();

    assert_eq!(result, "!whatever".to_owned());
}

#[test]
fn transforms_not_not() {
    let result = jcc::transform("!!whatever").unwrap();

    assert_eq!(result, "!!whatever".to_owned());
}

#[test]
fn transforms_not_minus() {
    let result = jcc::transform("!-whatever").unwrap();

    assert_eq!(result, "!-whatever".to_owned());
}
