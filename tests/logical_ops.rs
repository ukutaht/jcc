extern crate jcc;

#[test]
fn transforms_logical_and() {
    let result = jcc::transform("1 && 2").unwrap();

    assert_eq!(result, "1 && 2;".to_owned());
}
