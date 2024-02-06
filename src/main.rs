use clap::Parser;

mod expression_tree;
mod interpreter;
mod lexer;
mod parser;
mod runtime;
mod syntax_tree;
mod token;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    file: String,
}

fn main() {
    let args = Args::parse();
    let bytes = match std::fs::read(args.file) {
        Ok(bytes) => bytes,
        Err(err) => {
            println!("{}", err);
            return;
        }
    };

    match crate::interpreter::run(&bytes) {
        Ok(()) => (),
        Err(err) => println!("{}", err),
    }
}
