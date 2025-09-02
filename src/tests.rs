use crate::{execute_string, value::Labeled, value_ref::ValueRef};
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
    let ValueRef::Labeled(Labeled { label, args }) = stdout[0].as_ref() else {
        panic!()
    };
    assert_eq!(label.name, "Cons");
    assert_eq!(args[0], 2.into());
    let ValueRef::Labeled(Labeled { label, args }) = args[1].as_ref() else {
        panic!()
    };
    assert_eq!(label.name, "Nil");
    assert_eq!(args, &vec![]);
}

#[test]
fn test_print() {
    let stdout = execute_string(indoc! {r"
        import stdlib exposing (Print None);
        print (Print 2 None);
    "}).unwrap();
    dbg!(&stdout);
    let ValueRef::Labeled(Labeled { label, args }) = stdout[0].as_ref() else {
        panic!()
    };
    assert_eq!(label.name, "Print");
    assert_eq!(args[0], 2.into());
    let ValueRef::Labeled(Labeled { label, args }) = args[1].as_ref() else {
        dbg!(&args[1]);
        panic!()
    };
    assert_eq!(label.name, "None");
    assert_eq!(args, &vec![]);
}

#[test]
fn test_match() {
    let stdout = execute_string(indoc! {r#"
        label A;
        label B;
        print (
            match B
            | A => 42
            | B => 69
        );
    "#}).unwrap();
    dbg!(&stdout);
    assert_eq!(stdout, vec![69.into()]);
}

#[test]
fn test_failed_match() {
    let res = execute_string(indoc! {r#"
        label A;
        label B;
        print (
            match B
            | A => 42
        );
    "#});
    dbg!(&res);
    assert!(res.is_err());
}
