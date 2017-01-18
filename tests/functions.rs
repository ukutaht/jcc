extern crate jcc;

#[test]
fn transforms_named_function() {
    let result = jcc::transform("function hello() { return 1 }");

    assert_eq!(result, Ok("function hello() { return 1 }".to_owned()));
}

//#[test]
//fn transforms_arrow_function() {
//    let result = jcc::transform("() => { return 1 }");
//
//    assert_eq!(result, Ok("function() { return 1 }".to_owned()));
//}
