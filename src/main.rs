use std::env;

mod builtins;
mod check;
mod compiler;
mod error;
mod lexer;
mod logger;
mod parser;
mod run;

fn main() {
    let mut args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        run::run_file(args.remove(1));
    } else {
        println!("Usage: {} <file_name>", args[0]);
    }
}
