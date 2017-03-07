extern crate jcc;

#[test]
fn transforms_member_expression() {
    let result = jcc::transform("abc.wat").unwrap();

    assert_eq!(result, "abc.wat;".to_owned());
}

#[test]
fn transforms_member_expression_call() {
    let result = jcc::transform("abc.wat()").unwrap();

    assert_eq!(result, "abc.wat();".to_owned());
}

#[test]
fn transforms_mixed_member_expressions_and_calls() {
    let result = jcc::transform("abc.wat().hello.hi()").unwrap();

    assert_eq!(result, "abc.wat().hello.hi();".to_owned());
}
