extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;

use std::process::exit;

use clap::{AppSettings, Clap};

/// This doc string acts as a help message when the user runs '--help'
/// as do all doc strings on fields
#[derive(Clap)]
#[clap(version = "1.0", author = "jiweixu <ctsinon@gmail.com>")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(short, long)]
    input: String
}

fn main() {
    let opts: Opts = Opts::parse();
    match ast::Program::parse(&opts.input) {
        Err(x) => {
            print!("{}", x);
            exit(-1);
        },
        Ok(x) => {
            for i in x.statements {
                println!("{:#?}\n", i);
            }
        }
    }
}