extern crate jcc;

#[test]
fn transforms_if_statement() {
    let result = jcc::transform("if (true) { something() }").unwrap();

    assert_eq!(result, "if (true) { something(); };".to_owned());
}

#[test]
fn transforms_if_statement_with_else() {
    let result = jcc::transform("if (true) { something() } else { somethingElse() }").unwrap();

    assert_eq!(result, "if (true) { something(); } else { somethingElse(); };".to_owned());
}
