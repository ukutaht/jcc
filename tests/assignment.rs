extern crate jcc;

#[test]
fn transforms_var_assignment() {
    let result = jcc::transform("var a = 1");

    assert_eq!(result, Ok("var a = 1".to_owned()));
}
