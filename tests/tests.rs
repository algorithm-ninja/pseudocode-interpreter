use pseudocode_interpreter::compile;
use pseudocode_interpreter::error::Error;
use pseudocode_interpreter::eval::ProgramState;
use pseudocode_interpreter::parse::{self, TextAst};

fn run_program(source: &str, input: &str, function: &str) -> Result<Vec<String>, Error<TextAst>> {
    let ast = parse::parse(source)?;
    let program = compile::compile(&ast)?;
    let mut state = ProgramState::new(program, input)?;
    while !state.eval_step()? {}

    state.evaluate_fun(function, &[])?;
    while !state.eval_step()? {}

    Ok(state.stdout)
}

#[test]
fn simple_output() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function main()
        output(12)
    end function",
        "",
        "main",
    )?;

    assert_eq!(stdout, vec!["12"]);
    Ok(())
}

#[test]
fn builtins() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function main()
        output(min(5, 10))
        output(max(5, 10))
        output(min(-1.5, 5.5))
        output(max(0.5, 5.5))

        output(\"Case #\" + to_string(12) + \": \")

    end function",
        "",
        "main",
    )?;

    assert_eq!(stdout, vec!["5", "10", "-1.5", "5.5", "Case #12: "]);
    Ok(())
}

#[test]
fn function_calls() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function f(a: integer) -> integer
        return a + 2
    end function

    function nop()
        output(\"nop\")
    end function

    function main()
        nop()
        output(f(12))
        g(repeat(\"test\", 3), 3)
        output(1)
    end function

    function g(a: string[], n: integer)
        for i in [0...n) do
            output(a[i])
        end for
    end function
    ",
        "",
        "main",
    )?;

    assert_eq!(stdout, vec!["nop", "14", "test", "test", "test", "1"]);
    Ok(())
}
