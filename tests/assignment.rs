extern crate jcc;

#[test]
fn transforms_var_assignment() {
    let result = jcc::transform("var a = 1").unwrap();

    assert_eq!(result, "var a = 1;".to_owned());
}
