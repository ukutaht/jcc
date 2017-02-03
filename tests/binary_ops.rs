extern crate jcc;

#[test]
fn transforms_plus_op() {
    let result = jcc::transform("1 + 1").unwrap();

    assert_eq!(result, "1 + 1".to_owned());
}
