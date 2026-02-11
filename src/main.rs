// std imports
use std::collections::VecDeque;
use std::io::{Write, stdin, stdout};
use std::path::PathBuf;
use std::process::exit;
// library imports
use indoc::indoc;
use thiserror::Error;
// our imports
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

#[derive(Debug, Error)]
enum Error {
    #[error("command error: {0}")]
    IoCommand(#[from] lang::io_commands::Error),
    #[error("run-time error: {0}")]
    Eval(#[from] lang::eval::Error),
    #[error("generic error: {0}")]
    Io(#[from] std::io::Error),
    #[error("type-checking error: {0}")]
    Type(#[from] lang::typing::Error),
    // #[error("{0}")]
    // Parse(#[from] lang::parse::Error),
}

type Result<T, E = Error> = std::result::Result<T, E>;

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

struct ReplContext {
    pub run_context: Context,
    pub type_context: lang::typing::Context,
}

impl Default for ReplContext {
    fn default() -> Self {
        let mut run_context = Context::default();
        let mut type_context = lang::typing::Context::default();
        // Attach the standard library to both contexts.
        let stdlib = lang::stdlib::Stdlib::new();
        stdlib.attach(&mut run_context);
        stdlib.attach(&mut type_context);
        // Import it all of it, so that we can use it without a prefix.
        let import = Statement::Import {
            module_name: "stdlib".into(),
            exposing: stdlib.module().values.keys().cloned().collect(),
        };
        import.execute(&mut run_context).unwrap();
        import.infer(&mut type_context).unwrap();
        Self { run_context, type_context }
    }
}

impl AsRef<Context> for ReplContext {
    fn as_ref(&self) -> &Context {
        &self.run_context
    }
}
impl AsMut<Context> for ReplContext {
    fn as_mut(&mut self) -> &mut Context {
        &mut self.run_context
    }
}

fn run_repl_line(line: &str, c: &mut ReplContext) -> Result<()> {
    let program = parse(line.as_bytes())?;
    let t = program.infer(&mut c.type_context)?;
    if let Some(t) = t {
        println!("{t}");
    }
    let ret = program.execute(c.as_mut())?;
    if let Some(ret) = ret {
        println!("{ret}");
    }
    Ok(())
}

fn run_text(text: &str) -> Result<()> {
    let mut context = Context::default();
    let stdlib = lang::stdlib::Stdlib::new();
    stdlib.attach(&mut context);
    let program = parse(text.as_bytes())?;
    let Some(ret) = program.execute(&mut context)? else {
        eprintln!("This program does not return a value");
        exit(0);
    };

    stdlib.execute(&ret, &mut stdin().lock())?;
    Ok(())
}

fn log_error<T, E: std::fmt::Display>(res: Result<T, E>) -> Option<T> {
    match res {
        Ok(value) => Some(value),
        Err(err) => {
            eprintln!("{err}");
            None
        }
    }
}

fn handle_error<T, E: std::fmt::Display>(res: Result<T, E>) -> T {
    match res {
        Ok(value) => value,
        Err(err) => {
            eprintln!("Error: {err}");
            exit(1);
        }
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
            handle_error(run_text(&text));
        }
        Command::File(path_buf) => {
            let text = std::fs::read_to_string(&path_buf).unwrap_or_else(|err| {
                let path = path_buf.display();
                eprintln!("Error: the file '{path}' does not exist.");
                eprintln!("IO Error: {err}");
                panic!();
            });
            handle_error(run_text(&text));
        }
        Command::Repl => {
            let mut line = String::new();
            let mut c = ReplContext::default();
            loop {
                print!(">   ");
                stdout().flush().expect("stdout should be flushable");
                line.clear();
                stdin()
                    .read_line(&mut line)
                    .expect("stdin should be readable");
                // TODO
                if line.trim() == "q" {
                    break;
                }
                // TODO
                log_error(run_repl_line(&line, &mut c));
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
