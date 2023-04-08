use std::{env, fs, path::Path, rc::Rc};

mod check;
mod error;
mod lexer;
mod logger;
mod parser;
mod run;

fn main() {
    let mut args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        run_file(args.remove(1));
    } else {
        println!("Usage: {} <file_name>", args[0]);
    }
}

fn run_file(file_name: String) {
    let source = fs::read_to_string(&file_name).expect("Error while reading file.");
    let path_pointer = Rc::new(Path::new(&file_name).to_path_buf());

    run::run(source, path_pointer);
}
