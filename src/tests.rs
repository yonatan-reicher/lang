use crate::{
    execute_string,
    value::{Type, Value},
};
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
fn test_user_type() {
    let stdout = execute_string(indoc! {"
            type List {
                Nil,
                Cons head tail,
            };
            import List exposing (Nil Cons);
            print (Cons 2 Nil);
        "})
    .unwrap();

    dbg!(&stdout);
    let Value::Constructed(c, args) = &stdout[0] else {
        panic!("the value should have been constructed by a constructor, but was {}", &stdout[0])
    };
    assert_eq!(c.name, "Cons");
    assert_eq!(args[0], 2.into());
    let Value::Constructed(c, args) = &args[1] else {
        panic!()
    };
    assert_eq!(c.name, "Nil");
    assert_eq!(args, &vec![]);
}
