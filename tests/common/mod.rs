use pseudocode_interpreter::compile;
use pseudocode_interpreter::error::Error;
use pseudocode_interpreter::eval::ProgramState;
use pseudocode_interpreter::parse::{self, TextAst};

pub fn run_program(source: &str, input: &str) -> Result<Vec<String>, Error<TextAst>> {
    let ast = parse::parse(source)?;
    let program = compile::compile(&ast)?;
    let mut state = ProgramState::new(program, input)?;
    while !state.eval_step()? {}
    Ok(state.stdout)
}

/// Parses and compiles a given line, by itself if global == true,
/// or inside a main function
#[allow(dead_code)]
pub fn compile_line(expr: &str, global: bool) -> Result<(), Error<TextAst>> {
    let src = if global {
        format!(
            "
        {expr}
        function main()
        end function"
        )
    } else {
        format!(
            "
        function main()
            {expr}
        end function"
        )
    };
    let ast = parse::parse(&src)?;
    compile::compile(&ast)?;
    Ok(())
}
