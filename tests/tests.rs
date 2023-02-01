use common::run_program;
use pseudocode_interpreter::{error::Error, parse::TextAst};

mod common;

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

#[test]
fn custom_entry_point_name() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function main()
        output(\"invalid\")
    end function

    function custom_entry()
        output(\"correct\")
    end function
    ",
        "",
        "custom_entry",
    )?;

    assert_eq!(stdout, vec!["correct"]);
    Ok(())
}

const READ_STDIN_SOURCE: &'static str = "
variable n: integer

function main()
    n <- next_int()
    for i in [1...n] do
        variable x: integer <- 0
        if has_int() then
            x <- next_int()
            output(x)
        else
            output(\"String \" + next_string())
        end if
    end for
end function
";

#[test]
fn read_stdin() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        READ_STDIN_SOURCE,
        "
    10
    2 a -4000000000\t123
    test\r\ntest
    0.0 a


    a 22
    ",
        "main",
    )?;
    assert_eq!(
        stdout,
        vec![
            "2",
            "String a",
            "-4000000000",
            "123",
            "String test",
            "String test",
            "String 0.0",
            "String a",
            "String a",
            "22"
        ]
    );
    Ok(())
}

#[test]
fn read_stdin_eos() -> Result<(), Error<TextAst>> {
    let res = run_program(READ_STDIN_SOURCE, "10 2 a", "main");
    assert!(matches!(res, Err(Error::NextStringFailed(_, _))));
    Ok(())
}

#[test]
fn read_stdin_eos2() -> Result<(), Error<TextAst>> {
    let res = run_program(READ_STDIN_SOURCE, "", "main");
    assert!(matches!(res, Err(Error::NextIntFailed(_, _))));
    Ok(())
}

#[test]
fn read_stdin_parse_error() -> Result<(), Error<TextAst>> {
    let res = run_program(READ_STDIN_SOURCE, "invalid", "main");
    assert!(matches!(res, Err(Error::NextIntParsingFailed(_, _, _))));
    Ok(())
}
