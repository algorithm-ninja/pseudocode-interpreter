use std::ops::Range;

use gloo_worker::{HandlerId, Worker, WorkerScope};
use serde::{Deserialize, Serialize};

use pseudocode_interpreter::{compile, error, eval, parse};

// TODO(veluca); we want persistent state at some point.
pub struct PseudocodeEvaluator {}

#[derive(Serialize, Deserialize)]
pub enum Action {
    Eval { source: String, input: String },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Error {
    location: Range<usize>,
    message: String,
}

fn error_with_location(err: error::Error<parse::TextAst>) -> Error {
    Error {
        location: err.get_error_location(),
        message: format!("{err}"),
    }
}

fn run_eval<F>(
    source: String,
    input: String,
    callback: F,
) -> Result<(), error::Error<parse::TextAst>>
where
    F: Fn(String),
{
    let a = parse::parse(&source)?;
    let compiled = compile::compile(&a)?;
    let mut state = eval::ProgramState::new(compiled, &input)?;

    let mut output_len = 0;
    let mut check_new_output = |state: &eval::ProgramState<parse::TextAst>| {
        if output_len != state.stdout().len() {
            output_len = state.stdout().len();
            callback(itertools::join(state.stdout().iter(), "\n"));
        }
    };

    while !state.eval_step()? {
        check_new_output(&state);
    }
    state.evaluate_fun("main", &[])?;
    while !state.eval_step()? {
        check_new_output(&state);
    }
    Ok(())
}

impl Worker for PseudocodeEvaluator {
    type Input = Action;
    type Message = ();
    // TODO(veluca): this will need to be changed, and some support for updating output
    // incrementally will be needed.
    type Output = Result<String, Vec<Error>>;

    fn create(_scope: &WorkerScope<Self>) -> Self {
        Self {}
    }

    fn update(&mut self, _scope: &WorkerScope<Self>, _msg: Self::Message) {}

    fn received(&mut self, scope: &WorkerScope<Self>, msg: Self::Input, id: HandlerId) {
        match msg {
            Action::Eval { source, input } => {
                if let Err(e) = run_eval(source, input, |out| scope.respond(id, Ok(out))) {
                    scope.respond(id, Err(vec![error_with_location(e)]));
                }
            }
        };
    }
}
