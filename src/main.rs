use lang::*;

fn main() {
    let text = std::io::stdin()
        .lines()
        .map(|line| line.unwrap())
        .collect::<Vec<_>>()
        .join("\n");
    let program = parse::parse(&text).unwrap_or_else(|err| {
        eprintln!("Error parsing program: {}", err);
        std::process::exit(1);
    });
    dbg!(program);
}
