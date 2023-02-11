use common::run_program;
use pseudocode_interpreter::{error::Error, parse::TextAst};

mod common;

#[test]
fn reverse_array() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function reverse(n: integer, v: integer[]) -> integer[]
        for i in [0...n/2) do
            variable j: integer
            j <- n - 1 - i
            (v[i], v[j]) <- (v[j], v[i])
        end for
        return v
    end function
    function main()
        variable a: integer[] <- [1,2,3,4,5,6,7,8,9]
        a <- reverse(9, a)
        output(a)
    end function
    ",
        "",
    )?;

    assert_eq!(stdout, vec!["[9,8,7,6,5,4,3,2,1]"]);
    Ok(())
}

#[test]
fn count_sums() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function count_sums(n: integer) -> integer
        if n == 1 then
            return 1
        end if
        variable answer: integer
        answer <- 1
        for i in [1...n) do
            answer <- answer + count_sums(i)
        end for
        return answer
    end function
    function main()
        output(count_sums(4))
    end function
    ",
        "",
    )?;

    assert_eq!(stdout, vec!["8"]);
    Ok(())
}

#[test]
fn subsequence_sum() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function subsequence_sum(n: integer, v: integer[], x: integer, sum: integer) -> integer
        if n == 0 then
            if sum == x then
                return 1
            end if
            return 0
        end if
        if subsequence_sum(n-1, v, x, sum) == 1 then
            return 1
        end if
        if subsequence_sum(n-1, v, x, sum + v[n-1]) == 1 then
            return 1
        end if
        return 0
    end function
    function main()
        output(subsequence_sum(5, [1,2,3,4,5], -1, 0))
        output(subsequence_sum(5, [1,2,3,4,5], 9, 0))
        output(subsequence_sum(0, repeat(0, 0), 9, 0))
    end function
    ",
        "",
    )?;

    assert_eq!(stdout, vec!["0", "1", "0"]);
    Ok(())
}

#[test]
fn program_19() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    variable ctr: integer
    function main()
        ctr <- 0
        test()
        output(ctr)
    end function
    function test()
        variable conta: integer
        variable alfa: integer
        variable beta: integer
        conta <- 0
        alfa <- 0
        beta <- 0
        while conta < 29 do
            ctr <- ctr + 1
            if conta mod 3 == 1 then
                alfa <- alfa + 2
            else
                beta <= beta + 1
            end if
            conta <- conta + 2
        end while
    end function
    ",
        "",
    )?;

    assert_eq!(stdout, vec!["15"]);
    Ok(())
}

#[test]
fn program_20() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function main()
        output(secret(24, 3))
    end function
    function mystery(a: integer, b: integer) -> integer
        if 2 * a > b then
            return b
        else
            return a
        end if
    end function
    function secret(a: integer, b: integer) -> integer
        if a + b > mystery(a, b) then
            return a
        else
            return mystery(a, b)
        end if
    end function
    ",
        "",
    )?;

    assert_eq!(stdout, vec!["24"]);
    Ok(())
}

#[test]
fn program_21() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function main()
        variable v: integer[]
        variable w: integer[]
        v <- [4,2,6,3,5,8,9,0,7,1]
        w <- [0,0,0,0,0,0,0,0,0,0]
        for i in [0 ... 10) do
            w[v[i]] <- i
        end for
        for i in [0 ... 10) do
            output(w[i])
        end for
    end function
    ",
        "",
    )?;

    assert_eq!(stdout.last(), Some(&"6".to_owned()));
    Ok(())
}

#[test]
fn program_22() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function f(x: integer) -> integer
        variable i: integer
        while x > 0 do
            if x mod 2 == 0 then
                x <- x / 2
            else
                x <- x - 1
            end if
            i <- i + 1
        end while
        return i
    end function

    function main()
        output(f(7))
    end function
    ",
        "",
    )?;

    assert_eq!(stdout, vec!["5"]);
    Ok(())
}

#[test]
fn program_23() -> Result<(), Error<TextAst>> {
    let stdout = run_program(
        "
    function f(n: integer) -> integer
        if n < 10 then
            return f(n + 1) + 3
        else
            if n == 10 then
                return 7
            else
                return f(n-2)-1
            end if
        end if
    end function

    function main()
        output(f(13))
    end function
    ",
        "",
    )?;

    assert_eq!(stdout, vec!["8"]);
    Ok(())
}
