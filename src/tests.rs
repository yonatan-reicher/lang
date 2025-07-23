use crate::{execute_string, value::Value};
use indoc::indoc;

#[test]
fn assignment_and_print() {
    assert_eq!(
        execute_string(indoc! {"
                x = 3;
                print x;
                print x;
            "})
        .unwrap(),
        vec![3.into(), 3.into()],
    );
}

#[test]
fn test_function_call() {
    assert_eq!(
        execute_string(indoc! {"
            f = (x => x + 1);
            print (f 10);
        "})
        .unwrap(),
        vec![11.into()],
    );
}

#[test]
fn test_function_stdlib() {
    assert_eq!(
        execute_string(indoc! {"
            import stdlib exposing (abs neg);
            print 10;
            print (neg 10);
            print (abs 10);
            print (abs (neg 10));
        "})
        .unwrap(),
        vec![10.into(), (-10).into(), 10.into(), 10.into()],
    );
}

#[test]
fn test_label() {
    let stdout = execute_string(indoc! {"
            label Nil;
            label Cons head tail;
            print (Cons 2 Nil);
        "})
    .unwrap();

    dbg!(&stdout);
    let Value::Labeled { label, arguments } = &stdout[0] else {
        panic!()
    };
    assert_eq!(label.name, "Cons");
    assert_eq!(arguments[0], 2.into());
    let Value::Labeled { label, arguments } = &arguments[1] else {
        panic!()
    };
    assert_eq!(label.name, "Nil");
    assert_eq!(arguments, &vec![]);
}
