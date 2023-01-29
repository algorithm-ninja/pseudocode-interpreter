use pseudocode_interpreter::compile;
use pseudocode_interpreter::error::Error;
use pseudocode_interpreter::eval::ProgramState;
use pseudocode_interpreter::parse::{self, TextAst};

pub fn run_program(
    source: &str,
    input: &str,
    function: &str,
) -> Result<Vec<String>, Error<TextAst>> {
    let ast = parse::parse(source)?;
    let program = compile::compile(&ast)?;
    let mut state = ProgramState::new(program, input)?;
    while !state.eval_step()? {}

    state.evaluate_fun(function, &[])?;
    while !state.eval_step()? {}

    Ok(state.stdout)
}
