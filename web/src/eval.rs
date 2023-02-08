use std::{ops::Range, sync::Mutex};

use gloo_worker::{HandlerId, Spawnable, Worker, WorkerBridge, WorkerScope};
use log::warn;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};

use pseudocode_interpreter::{compile, error, eval, parse};
use send_wrapper::SendWrapper;
use yew::UseStateHandle;

// TODO(veluca); we want persistent state at some point.
pub struct PseudocodeEvaluator {}

#[derive(Serialize, Deserialize)]
pub enum WorkerCommand {
    Eval { source: String, input: String },
}

#[derive(Serialize, Deserialize)]
pub enum WorkerAnswer {
    Clear,
    AppendOutput(String),
    Done,
    AddError(Error),
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
        for i in output_len..state.stdout().len() {
            callback(state.stdout()[i].clone());
        }
        output_len = state.stdout().len();
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
    type Input = WorkerCommand;
    type Message = ();
    type Output = WorkerAnswer;

    fn create(_scope: &WorkerScope<Self>) -> Self {
        Self {}
    }

    fn update(&mut self, _scope: &WorkerScope<Self>, _msg: Self::Message) {}

    fn received(&mut self, scope: &WorkerScope<Self>, msg: Self::Input, id: HandlerId) {
        match msg {
            WorkerCommand::Eval { source, input } => {
                scope.respond(id, WorkerAnswer::Clear);
                if let Err(e) = run_eval(source, input, |out| {
                    scope.respond(id, WorkerAnswer::AppendOutput(out))
                }) {
                    scope.respond(id, WorkerAnswer::AddError(error_with_location(e)))
                }
                scope.respond(id, WorkerAnswer::Done);
            }
        };
    }
}

pub struct EvalBridge {
    worker: SendWrapper<WorkerBridge<PseudocodeEvaluator>>,
    // TODO(veluca): deal with large outputs.
    output: String,
    output_state: SendWrapper<Option<UseStateHandle<String>>>,
    on_done: SendWrapper<Option<Box<dyn Fn() + 'static>>>,
}

impl EvalBridge {
    fn update_output<F: FnOnce(&str) -> String>(&mut self, cb: F) {
        let new_output = cb(&self.output);
        self.output = new_output;
        self.output_state.as_ref().unwrap().set(self.output.clone())
    }
    fn handle_answer(&mut self, answer: WorkerAnswer) {
        match answer {
            WorkerAnswer::Clear => self.update_output(|_| "".into()),
            WorkerAnswer::Done => {
                if let Some(x) = &*self.on_done {
                    x()
                }
            }
            WorkerAnswer::AddError(err) => {
                warn!("{:?}", err);
                self.update_output(|_| "an error occurred".into())
            }

            WorkerAnswer::AppendOutput(out) => {
                self.update_output(|current_output| {
                    if current_output == "" {
                        out
                    } else {
                        format!("{current_output}\n{out}")
                    }
                });
            }
        }
    }
}

fn eval_bridge() -> &'static Mutex<EvalBridge> {
    static INSTANCE: OnceCell<Mutex<EvalBridge>> = OnceCell::new();
    INSTANCE.get_or_init(|| {
        Mutex::new(EvalBridge {
            worker: SendWrapper::new(
                PseudocodeEvaluator::spawner()
                    .callback(|x| eval_bridge().lock().unwrap().handle_answer(x))
                    .spawn("./worker.js"),
            ),
            output: String::new(),
            output_state: SendWrapper::new(None),
            on_done: SendWrapper::new(None),
        })
    })
}

pub fn set_output_state(output_state: UseStateHandle<String>) {
    *eval_bridge().lock().unwrap().output_state = Some(output_state)
}

pub fn set_done_callback<F: Fn() + 'static>(on_done: F) {
    *eval_bridge().lock().unwrap().on_done = Some(Box::new(on_done))
}

pub fn send_worker_command(command: WorkerCommand) {
    eval_bridge().lock().unwrap().worker.send(command)
}
