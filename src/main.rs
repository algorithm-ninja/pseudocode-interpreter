use clap::Parser;
use clap_derive::Parser;
use sc_int::eval_on_args;
use sc_int::format_code;
use sc_int::format_snippet;
use sc_int::Value;
use std::fs::File;
use std::io::Read;

#[derive(Parser)]
struct Args {
    /// Pseudocode source file.
    source: String,

    /// Name of the function to run.
    #[clap(long, default_value = "f")]
    fname: String,

    /// Arguments for the function
    #[clap(long, short)]
    arg: Vec<i64>,

    /// Format only
    #[clap(long)]
    format_only: bool,

    /// Indent level
    #[clap(long, default_value = "0")]
    indent: usize,

    /// Snippet only (i.e. statements)
    #[clap(long)]
    snippet: Option<usize>,

    /// Code this is a snippet of.
    #[clap(long)]
    base_code: Option<String>,
}

fn main() {
    let args = Args::parse();

    let mut file = File::open(&args.source).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    if args.format_only {
        if let Some(sn) = args.snippet {
            let mut base_src = String::new();
            let mut file = File::open(&args.base_code.unwrap()).expect("Unable to open file");
            file.read_to_string(&mut base_src)
                .expect("Unable to read file");
            print!(
                "{}",
                format_snippet(
                    src,
                    base_src,
                    args.fname,
                    sn,
                    args.indent,
                    /*emit_html=*/ true
                )
            );
        } else {
            print!("{}", format_code(src, args.indent, /*emit_html=*/ true));
        }
        return;
    }

    let data = eval_on_args(
        src,
        args.fname.clone(),
        args.arg.iter().map(|x| Value::Integer(*x)).collect(),
        /*emit_html=*/ false,
    );

    for d in data {
        print!("\x1bc{d}");
        std::thread::sleep(std::time::Duration::from_millis(500));
    }
}
