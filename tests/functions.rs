extern crate jcc;

#[test]
fn transforms_named_function() {
    let result = jcc::transform("function hello() { var a = 1 }");

    assert_eq!(result, Ok("function hello() { var a = 1 }".to_owned()));
}
