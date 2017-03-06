extern crate jcc;

#[test]
fn transforms_plus_op() {
    let result = jcc::transform("1 && 2").unwrap();

    assert_eq!(result, "1 && 2".to_owned());
}
