#![feature(map_try_insert)]
extern crate pest;
#[macro_use]
extern crate pest_derive;

mod ast;
mod ir;
mod codegen;
mod analysis;

use std::{fs::File, io::Read};

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

fn compile(file: String) -> Result<(), String> {
    let mut src_file = File::open(file)
        .expect("Failed when opening file.");

    let mut src_str = String::new();
    src_file.read_to_string(&mut src_str).unwrap();

    let ast = ast::Program::parse(&src_str)?;
    

    let mut ir = ir::Program::new(ast);
    ir.emit()?;
    // ir.pprint();

    let mut cfg = analysis::cfg::CFG::new(ir);
    cfg.build();
    cfg.pprint();

    let mut rd = analysis::reaching_definition::RDContext::new(&mut cfg);
    rd.analysis();
    // rd.pprint();

    let mut lv = analysis::live_variable::LVContext::new(&mut cfg);
    lv.analysis();
    lv.pprint();
    Ok(())
}

fn main() {
    let opts: Opts = Opts::parse();
    match compile(opts.input) {
        Ok(_) => (),
        Err(x) => println!("{}", x)
    }; 
}