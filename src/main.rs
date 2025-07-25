use std::collections::VecDeque;
use std::io::{Result as IoResult, Write, stdin, stdout};
use std::path::PathBuf;
use std::process::exit;

use indoc::indoc;

use lang::prelude::*;
use lang::value::{Func, LabelFunc, PLabel};

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
    let Some(ret) = program.execute(&mut context) else {
        eprintln!("This program does not return a value");
        exit(0);
    };

    execute_monad(&ret, &context).unwrap()
}

fn execute_monad(x: &Value, context: &Context) -> IoResult<()> {
    fn lbl(value: &Value) -> Option<PLabel> {
        if let Value::Func(Func::Label(LabelFunc { label, .. })) = value {
            return Some(label.clone());
        } else if let Value::Labeled { label, .. } = value {
            return Some(label.clone());
        }
        None
    }
    fn lbl_stdlib(name: &'static str, context: &Context) -> Option<PLabel> {
        lbl(context.modules["stdlib"].values.get(name)?)
    }

    let error = || {
        eprintln!("Error: bad value '{x}'");
        exit(1);
    };

    match x.clone() {
        Value::Labeled { label, arguments } => {
            // Print
            if label == lbl_stdlib("Print", context).unwrap() {
                let [value, next] = arguments.as_slice() else {
                    panic!()
                };
                println!("{value}");
                execute_monad(next, context)
            // Input
            } else if label == lbl_stdlib("Input", context).unwrap() {
                let [Value::Func(f)] = arguments.as_slice() else {
                    panic!()
                };
                let mut line = String::new();
                stdin().read_line(&mut line).unwrap();
                line = line.trim_end_matches("\r\n").trim_end_matches("\n").into();
                let next = f.apply(line.into());
                execute_monad(&next, context)
            // None
            } else if label == lbl_stdlib("None", context).unwrap() {
                Ok(())
            } else {
                error()
            }
        }
        _ => error(),
    }
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
