extern crate jcc;

#[test]
fn transforms_member_expression() {
    let result = jcc::transform("abc.wat").unwrap();

    assert_eq!(result, "abc.wat".to_owned());
}
