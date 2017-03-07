extern crate jcc;

#[test]
fn transforms_two_statements() {
    let result = jcc::transform("one.method()\nsecond.method()").unwrap();

    assert_eq!(result, "one.method();second.method();".to_owned());
}
