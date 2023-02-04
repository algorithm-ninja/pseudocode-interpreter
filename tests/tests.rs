use common::{compile_line, run_program};
use ntest::timeout;
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
fn variable_declarations() -> Result<(), Error<TextAst>> {
    for global in [true, false] {
        compile_line("variable i: integer", global)?;
        compile_line("variable b: bool", global)?;
        compile_line("variable f: float", global)?;
        compile_line("variable s: string", global)?;
        compile_line("variable arr: integer[]", global)?;
        compile_line("variable arr: integer[][]", global)?;
        compile_line("variable t: (integer, integer)", global)?;
        compile_line("variable t: ((float, bool), integer)", global)?;
        compile_line("variable t: ((float, bool[]), string)", global)?;

        assert!(compile_line("variable i", global).is_err());
        assert!(compile_line("arr: integer[]", global).is_err());
        assert!(compile_line("integer a", global).is_err());
        assert!(compile_line("int a", global).is_err());
        assert!(compile_line("variable x: (integer)", global).is_err());
        assert!(compile_line("variable x: float, y: float", global).is_err());
        assert!(compile_line("variable (x, y): (float, float)", global).is_err());
    }
    Ok(())
}

#[test]
fn string_escape() -> Result<(), Error<TextAst>> {
    let src = r#"
    function main()
        output("")
        output("\"")
        output("\\")
        output("\n")
        output("\t")
        output("'")
    end function
    "#;
    let stdout = run_program(src, "", "main")?;
    assert_eq!(stdout, vec!["", "\"", "\\", "\n", "\t", "'"]);
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
        output(min([1,2,3,4], [1,2,3,3]))
        output(max([1,2,3,4], [1,2,3,3]))
        output(min(\"qwertyuiop\", \"aqwertyuiop\"))

        output(\"Case #\" + to_string(12) + \": \")

    end function",
        "",
        "main",
    )?;

    assert_eq!(
        stdout,
        vec![
            "5",
            "10",
            "-1.5",
            "5.5",
            "[1,2,3,3]",
            "[1,2,3,4]",
            "aqwertyuiop",
            "Case #12: "
        ]
    );
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

const READ_STDIN_SOURCE: &str = "
variable n: integer

function main()
    n <- next_int()
    for i: integer in [1...n] do
        variable x: integer <- 0

        if has_int() then
            if not has_string() then
                output(\"FAIL\")
                return
            end if

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

#[test]
fn comment_placement() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    // comment 1
    function main()
        // comment 2
        //without space
        variable not_a_comment: string <- \"//nope\"
        for i in [-1...1) do
            output(not_a_comment)
            // comment 3
        end for
    end function

    ",
        "",
        "main",
    )?;

    assert_eq!(stdout, vec!["//nope", "//nope"]);
    Ok(())
}

#[test]
fn bool_ops() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function assert(b: bool)
        if b then
            output(\"OK\")
        else
            output(\"FAIL\")
        end if
    end function

    function main()
        assert(true == true)
        assert(false == false)
        assert(true != false)

        assert(true > false)
        assert(true <= true)
        assert(false >= false)
        assert(false < true)

        assert(not (false or false))
        assert(true and true)
        assert((true and false) == false)

        variable v: bool[] <- [false, true]
        assert(to_string(v[0]) == \"false\")
        assert(to_string(v[1]) == \"true\")

        assert(min(true, true) == true)
        assert(min(true, false) == false)
        assert(max(true, false) == true)
    end function
    ",
        "",
        "main",
    )?;

    stdout
        .iter()
        .enumerate()
        .for_each(|(i, x)| assert_eq!(x, "OK", "test {i} failed"));
    Ok(())
}

#[test]
fn float_ops() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    variable eps: float
    function assert_close(a: float, b: float)
        if a >= b - eps and a <= b + eps then
            output(\"OK\")
        else
            output(\"FAIL\")
        end if
    end function

    function sum(n: integer, vals: float[]) -> float
        variable res: float <- 0.0
        for i in [0...n) do
            res <- res + vals[i]
        end for
        return res
    end function

    function main()
        eps <- 0.000000001
        assert_close(0.1, 0.1)
        assert_close(0.1 + 0.2, 0.3)
        assert_close(1000.0 / 2.0 * 2.0, 1000.0)
        assert_close(-1.0 / 1.0, -1.0)

        variable nums: float[] <- repeat(0.1, 10000)

        assert_close(sum(10000, nums), 1000.0)
    end function
    ",
        "",
        "main",
    )?;

    stdout
        .iter()
        .enumerate()
        .for_each(|(i, x)| assert_eq!(x, "OK", "test {i} failed"));
    Ok(())
}

#[test]
fn array_access() -> Result<(), Error<TextAst>> {
    for array_length in [-100, -1, 0, 1, 2, 3, 10, 100] {
        for index in -5..array_length + 5 {
            let code = format!(
                "
            variable arr: integer[]
            function main()
                arr <- repeat(1, {array_length})
                arr[{index}] <- 42
                output(arr[{index}])
            end function
            "
            );

            let res = run_program(&code, "", "main");

            if array_length < 0 {
                assert!(
                    matches!(res, Err(Error::RepeatNegativeAmount(_, _, _))),
                    "{res:?}"
                );
            } else if index < 0 || index >= array_length {
                assert!(
                    matches!(res, Err(Error::ArrayOutOfBounds(_, _, _, _))),
                    "{res:?}"
                );
            } else {
                assert!(matches!(res, Ok(_)), "{res:?}");
                assert_eq!(res.unwrap(), vec!["42"]);
            }
        }
    }
    Ok(())
}

#[test]
fn tuple_access() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    variable a: (integer, integer)
    type T: (bool, float, integer)

    function f() -> (integer, integer)
        return (a.1, 43)
    end function

    function main()
        variable b: (integer, integer)
        variable c: T <- (true, 0.5, 1)
        a.0 <- 42
        (b.0, a.1) <- (a.0, c.2)
        b.1 <- f().1

        variable arr: integer[] <- repeat(0, 5)
        if c.0 then
            arr[a.1] <- 2
        else
            arr[a.1] <- -1
        end if

        output(c)
        output((b.0, a.1))
        for i in [0...arr[a.1]] do
            output(c.0)
        end for
    end function
    ",
        "",
        "main",
    )?;

    assert_eq!(
        stdout,
        vec!["(true,0.5,1)", "(42,1)", "true", "true", "true"]
    );
    Ok(())
}

#[test]
#[timeout(250)]
fn parser_backtracking_performance_1() {
    let res = run_program(
        "
    function main()
        [[[[[[[[[[[[[[[[[[[[[[[[[[[
    end function",
        "",
        "main",
    );
    assert!(matches!(res, Err(Error::ParseError(_, _, _))));
}

#[test]
#[timeout(250)]
fn parser_backtracking_performance_2() {
    let res = run_program(
        "
    function main()
        {{{{{{{{{{{{{{{{{{{{{{{{{{{
    end function",
        "",
        "main",
    );
    assert!(matches!(res, Err(Error::ParseError(_, _, _))));
}
