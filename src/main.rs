extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod ir;

use ast::Program;
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

    let ast = match Program::parse(&opts.input) {
        Ok(x) => x,
        Err(x) => panic!("{}", x)
    };


}