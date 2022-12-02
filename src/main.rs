use clap::Parser;
use clap_derive::Parser;
use std::fs::File;
use std::io::Read;

use anyhow::Result;

use pseudocode_interpreter::parse;
use pseudocode_interpreter::typecheck;

#[derive(Parser)]
struct Args {
    /// Pseudocode source file.
    source: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut file = File::open(&args.source).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let ast = parse::parse(&src)?;
    println!("{:#?}", typecheck::typecheck(&ast)?);

    Ok(())
}
