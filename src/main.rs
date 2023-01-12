use clap::Parser;
use pseudocode_interpreter::ast::Program;
use pseudocode_interpreter::eval::ProgramState;
use pseudocode_interpreter::eval::StackFrame;
use pseudocode_interpreter::parse::TextAst;

use std::cmp::Ordering;
use std::fs::File;
use std::io::Read;
use std::time::Duration;

use anyhow::Result;

use pseudocode_interpreter::compile;
use pseudocode_interpreter::error;
use pseudocode_interpreter::parse;

#[derive(Parser)]
struct Args {
    /// Print debugging information during execution.
    #[arg(short, long, default_value_t = false)]
    debug: bool,

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

fn print_stack_frame<'a>(program: &'a Program<TextAst>, frame: StackFrame<'a, TextAst>, src: &str) {
    let (b, e) = if let Some(fun) = frame.fun {
        (fun.info.start, fun.info.end)
    } else {
        (0, src.len())
    };
    let mut replacements = vec![];

    if let Some(ex) = frame.current_expr {
        replacements.push((ex.info.start, ex.info.start, "\x1b[4m".to_owned()));
        replacements.push((ex.info.end, ex.info.end, "\x1b[24m".to_owned()));
    }

    for (var, val) in frame.variables.iter() {
        let pos = program.var(*var).ident.info.end;
        replacements.push((pos, pos, format!(" \x1b[34;3m▸{:?}\x1b[;m", val)));
    }

    for (node, val) in frame.temporaries.iter() {
        replacements.push((
            node.info.start,
            node.info.end,
            format!("\x1b[34;1m{:?}\x1b[m", val),
        ));
    }

    replacements.sort_by(|a, b| {
        if a.0 < b.0 {
            Ordering::Less
        } else if a.0 > b.0 {
            Ordering::Greater
        } else {
            // a.0 == b.0
            // First 0-length replacements, then all the others in decreasing order of length.
            if a.1 == a.0 && b.1 == b.0 {
                Ordering::Equal
            } else if a.1 == a.0 {
                Ordering::Less
            } else if b.1 == b.0 {
                Ordering::Greater
            } else {
                b.1.cmp(&a.1)
            }
        }
    });

    let mut output = String::new();
    let mut skip_until = b;
    let mut current_replacement = 0;
    for pos in b..e {
        while current_replacement < replacements.len() && pos >= replacements[current_replacement].0
        {
            if pos >= skip_until && pos == replacements[current_replacement].0 {
                output += &replacements[current_replacement].2;
                skip_until = replacements[current_replacement].1;
            }
            current_replacement += 1;
        }
        if pos >= skip_until {
            output += &src[pos..pos + 1];
        }
    }
    println!("{}", &output);
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
            let mut next_print = 0;
            let mut state = ProgramState::new(compiled)?;
            while !state.eval_step()? {}
            let mut current = 0;
            state.evaluate_fun("main", &[])?;
            if args.debug {
                println!("before start: {:?}\n\n", state.stack_frames());
            }
            while !state.eval_step()? {
                if next_print <= current && args.debug {
                    next_print += current + 1;
                    print!("\x1bc");
                    for stack_frame in state.stack_frames() {
                        print_stack_frame(&a, stack_frame, src);
                    }
                    std::thread::sleep(Duration::from_millis(1000));
                }
                current += 1;
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
