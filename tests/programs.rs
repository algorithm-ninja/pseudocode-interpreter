use common::run_program;
use pseudocode_interpreter::{error::Error, parse::TextAst};

mod common;

/// Basic safety test, asserting that the given program runs without errors
/// and without crashing the interpreter. For some added validation, fails
/// if the program prints "ASSERT"
macro_rules! test_program {
    ($name:ident) => {
        #[test]
        fn $name() -> Result<(), Error<TextAst>> {
            let stdout = run_program(
                include_str!(concat!("programs/", stringify!($name), ".srs")),
                "",
                "main",
            )?;
            assert!(!stdout.contains(&"ASSERT".to_owned()));
            Ok(())
        }
    };
}

test_program!(tiny);
test_program!(tiny_return);
test_program!(fib_exp);
test_program!(fib_iter);
test_program!(call_void_function);
test_program!(swap);
