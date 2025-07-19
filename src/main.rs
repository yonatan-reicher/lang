use std::collections::VecDeque;
use std::io::{Write, stdin, stdout};
use std::path::PathBuf;
use std::process::exit;

use indoc::indoc;

use lang::prelude::*;

const USAGE: &str = indoc! {r"
    USAGE:
        lang <file>
        lang -
        lang repl

    Calling `lang <file>` runs the program from a given file. Calling `lang -` is the same as
    calling with a named file, but reads the contents from stdin instead of a file. Calling
    `lang repl` starts a Read-Eval-Print loop.
"};

#[derive(Debug, Clone, PartialEq)]
struct MainConfig {
    command: Command,
    executable_name: String,
    help_flag: bool,
}

#[derive(Debug, Clone, PartialEq)]
enum Command {
    Stdin,
    File(PathBuf),
    Repl,
    Error(Vec<String>),
}

impl MainConfig {
    pub fn new(mut args: VecDeque<String>) -> Self {
        let executable_name = args
            .pop_front()
            .expect("arguments for the program should contain the executable name");
        let mut help_flag = false;
        args.retain(|arg| {
            let is_help_flag = arg == "--help" || arg == "-h";
            help_flag = is_help_flag;
            !is_help_flag
        });
        let command = match &args.iter().map(|x| x.as_str()).collect::<Vec<_>>()[..] {
            ["-"] => Command::Stdin,
            ["repl"] => Command::Repl,
            [file] => Command::File(file.into()),
            _ => Command::Error(args.into()),
        };
        Self {
            command,
            executable_name,
            help_flag,
        }
    }
}

fn run_text(text: &str) {
    let program = parse(text).unwrap_or_else(|err| {
        eprintln!("Error parsing program: {err}");
        exit(1);
    });
    dbg!(&program);
    let mut context = Context::default();
    context.add_stdlib();
    program.execute(&mut context);
}

fn main() {
    let config = MainConfig::new(std::env::args().collect());

    if config.help_flag {
        println!("{USAGE}");
        exit(0);
    }

    match config.command {
        Command::Stdin => {
            let text = std::io::stdin()
                .lines()
                .map(|line| line.unwrap())
                .collect::<Vec<_>>()
                .join("\n");
            run_text(&text)
        }
        Command::File(path_buf) => {
            let text = std::fs::read_to_string(&path_buf).unwrap_or_else(|err| {
                let path = path_buf.display();
                eprintln!("Error: the file '{path}' does not exist.");
                eprintln!("IO Error: {err}");
                panic!();
            });
            run_text(&text)
        }
        Command::Repl => {
            let mut line = String::new();
            loop {
                print!(">   ");
                stdout().flush().expect("stdout should be flushable");
                line.clear();
                stdin()
                    .read_line(&mut line)
                    .expect("stdin should be readable");
                // TODO
                run_text(&line);
            }
        }
        Command::Error(items) => {
            let items = items.join("\n");
            eprintln!("Error: could not understand the command");
            eprintln!("Alien arguments: {items}");
            eprintln!("{USAGE}");
            exit(1);
        }
    }
}
