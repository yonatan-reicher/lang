use crate::{execute_string, value_ref::ValueRef};
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
    let ValueRef::Labeled(labeled) = stdout[0].as_ref() else {
        panic!()
    };
    assert_eq!(labeled.label().name, "Cons");
    assert_eq!(labeled.args()[0], 2.into());
    let ValueRef::Labeled(labeled) = labeled.args()[1].as_ref() else {
        panic!()
    };
    assert_eq!(labeled.label().name, "Nil");
    assert_eq!(labeled.args(), &vec![]);
}

#[test]
fn test_print() {
    let stdout = execute_string(indoc! {r"
        import stdlib exposing (Print None);
        print (Print 2 None);
    "})
    .unwrap();
    dbg!(&stdout);
    let ValueRef::Labeled(x) = stdout[0].as_ref() else {
        panic!()
    };
    assert_eq!(x.label().name, "Print");
    assert_eq!(x.args()[0], 2.into());
    let ValueRef::Labeled(x) = x.args()[1].as_ref() else {
        dbg!(&x.args()[1]);
        panic!()
    };
    assert_eq!(x.label().name, "None");
    assert_eq!(x.args(), &vec![]);
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
    "#})
    .unwrap();
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

#[test]
fn complex_syntax() {
    let res = execute_string(indoc! {r#"
        import stdlib exposing (
            Cons
            IsDir
            Nil
            None
            Print
            Read
            fix
        );

        print_file = fix (print_file => p => (
            print_files = fix (print_files => l =>
                match l
                | Nil => None
                | Cons h t => Print h (print_files t)
            );

            IsDir p (is_dir =>
                if is_dir
                then Read p print_files
                else Print p None
            )
        ));

        print (print_file ".");
    "#});
    dbg!(&res);

    let value = res
        .expect("Should execute successfully")
        .first()
        .cloned()
        .expect("Should produce a single value");

    let ValueRef::Labeled(x) = value.as_ref() else {
        panic!()
    };

    assert_eq!(x.label().name, "IsDir");
    assert_eq!(x.args().len(), 2);
}
