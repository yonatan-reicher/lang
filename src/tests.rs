use crate::execute_string;
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
