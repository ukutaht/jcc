extern crate jcc;

#[test]
fn transforms_if_statement() {
    let result = jcc::transform("if (true) { something() }").unwrap();

    assert_eq!(result, "if (true) { something() }".to_owned());
}
