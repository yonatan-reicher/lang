use lang::prelude::*;

fn main() {
    let text = std::io::stdin()
        .lines()
        .map(|line| line.unwrap())
        .collect::<Vec<_>>()
        .join("\n");
    let program = parse(&text).unwrap_or_else(|err| {
        eprintln!("Error parsing program: {err}");
        std::process::exit(1);
    });
    dbg!(&program);
    let mut context = Context::default();
    context.add_stdlib();
    program.execute(&mut context);
}
