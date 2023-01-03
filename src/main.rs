use clap::Parser;
use clap_derive::Parser;
use pseudocode_interpreter::eval::ProgramState;

use std::fs::File;
use std::io::Read;

use anyhow::Result;

use pseudocode_interpreter::compile;
use pseudocode_interpreter::error;
use pseudocode_interpreter::parse;

#[derive(Parser)]
struct Args {
    /// Pseudocode source file.
    source: String,
}

fn print_error_with_location(s: &str, err: error::Error<parse::TextAst>) {
    let mut s = s.to_owned();
    let r = err.get_error_location();
    let replacement = "\x1b[31;1m".to_owned() + &s[r.clone()] + "\x1b[;m";
    s.replace_range(r, &replacement);
    println!("Error in input program");
    println!("{}", s);
    println!("{}", err);
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut file = File::open(args.source).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let ast = (|src| {
        let a = parse::parse(src)?;
        let compiled = compile::compile(&a)?;
        {
            let mut state = ProgramState::new(compiled)?;
            while !state.eval_step()? {}
            state.evaluate_fun("main", &[])?;
            while !state.eval_step()? {
                // println!("{:?}", state);
            }
            for line in state.stdout() {
                println!("{line}");
            }
        }
        Ok(())
    })(&src);

    if let Err(err) = ast {
        print_error_with_location(&src, err);
        return Ok(());
    }

    Ok(())
}
