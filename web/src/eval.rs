use std::{ops::Range, sync::Mutex, vec};

use gloo_worker::{HandlerId, Spawnable, Worker, WorkerBridge, WorkerScope};
use js_sys::{Array, Object};
use log::info;
use monaco::{
    api::TextModel,
    sys::{
        editor::{get_model_markers, set_model_markers},
        MarkerSeverity,
    },
};
use once_cell::sync::OnceCell;
use ouroboros::self_referencing;
use serde::{Deserialize, Serialize};

use pseudocode_interpreter::{
    ast::Program,
    compile::compile,
    error,
    eval::{self, ProgramState},
    parse::{self, TextAst},
};
use send_wrapper::SendWrapper;
use yew::UseStateHandle;

use crate::{app::CurrentAction};

#[derive(Serialize, Deserialize, Debug)]
pub enum WorkerCommand {
    StartEval { source: String, input: String },
    Advance { count: usize },
    GoBack { count: usize },
    JumpTo { position: usize },
    Parse { source: String },
    GetCurrentDebugInfo,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct VariableValue {
    location: (usize, usize),
    value: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ExpressionValue {
    location: ((usize, usize), (usize, usize)),
    value: String,
}

#[derive(Serialize, Deserialize)]
pub enum WorkerAnswer {
    ClearOutput,
    ClearErrors,
    AppendOutput(String),
    Done,
    AddError(Error),
    AddQuietError(Error),
    CommandDone,
    DebugInfo {
        variable_values: Vec<VariableValue>,
        expression_values: Vec<ExpressionValue>,
    },
    InvalidateDebugInfo,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Error {
    location: ((usize, usize), (usize, usize)),
    message: String,
}

fn get_line_and_char(src: &str, mut pos: usize) -> (usize, usize) {
    let mut candidate = (0, 0);
    for (n, l) in src.lines().enumerate() {
        candidate = (n + 1, pos + 1);
        if l.len() >= pos {
            break;
        }
        pos -= l.len() + 1;
    }
    candidate
}

fn range_to_monaco_location(src: &str, range: Range<usize>) -> ((usize, usize), (usize, usize)) {
    (
        get_line_and_char(src, range.start),
        get_line_and_char(src, range.end),
    )
}

fn error_with_location(src: &str, err: error::Error<parse::TextAst>) -> Error {
    Error {
        location: range_to_monaco_location(src, err.get_error_location()),
        message: format!("{err}"),
    }
}

#[self_referencing]
struct EvalState {
    program: Program<TextAst>,
    #[borrows(program)]
    #[not_covariant]
    state: eval::ProgramState<'this, TextAst>,

    output_len: usize,
    source: String,
}

pub const NUM_STEPS: usize = 1000;

impl EvalState {
    fn advance<F>(&mut self, count: usize, callback: F) -> Result<(), Error>
    where
        F: Fn(WorkerAnswer),
    {
        callback(WorkerAnswer::InvalidateDebugInfo);
        let mut run = || {
            for _ in 0..count {
                if self.with_state_mut(|state| state.eval_step())? {
                    callback(WorkerAnswer::Done);
                    return Ok(());
                } else {
                    let new_len = self.with_state(|state| state.stdout().len());
                    for i in *self.borrow_output_len()..new_len {
                        callback(WorkerAnswer::AppendOutput(
                            self.with_state(|state| state.stdout()[i].clone()),
                        ));
                    }
                    self.with_output_len_mut(|len| *len = new_len);
                }
            }
            Ok(())
        };
        run().map_err(|e| error_with_location(self.borrow_source(), e))
    }

    fn current_debug_info(&mut self) -> WorkerAnswer {
        let stack_frames = self.with_state(|state| state.stack_frames());
        let mut variable_values = vec![];
        let mut expression_values = vec![];
        for frame in stack_frames {
            for (k, v) in frame.variables {
                let loc = self.with_program(|p| p.var(k)).ident.info.end;
                let val = format!("{}", v);
                variable_values.push(VariableValue {
                    location: get_line_and_char(self.borrow_source(), loc),
                    value: val,
                });
            }
            for (k, v) in frame.temporaries {
                let start = get_line_and_char(self.borrow_source(), k.info.start);
                let end = get_line_and_char(self.borrow_source(), k.info.end);
                let val = format!("{:?}", v);
                expression_values.push(ExpressionValue {
                    location: (start, end),
                    value: val,
                });
            }
        }
        WorkerAnswer::DebugInfo {
            variable_values,
            expression_values,
        }
    }
}

fn make_eval_state(source: String, input: &str) -> Result<EvalState, Error> {
    let make = || {
        EvalStateTryBuilder {
            program: parse::parse(&source)?,
            state_builder: |program| {
                let compiled = compile(program)?;
                ProgramState::new(compiled, input)
            },
            output_len: 0,
            source: source.clone(),
        }
        .try_build()
    };
    make().map_err(|e| error_with_location(&source, e))
}

pub struct PseudocodeEvaluator {
    eval_state: Option<EvalState>,
}

impl Worker for PseudocodeEvaluator {
    type Input = WorkerCommand;
    type Message = ();
    type Output = WorkerAnswer;

    fn create(_scope: &WorkerScope<Self>) -> Self {
        Self { eval_state: None }
    }

    fn update(&mut self, _scope: &WorkerScope<Self>, _msg: Self::Message) {}

    fn received(&mut self, scope: &WorkerScope<Self>, msg: Self::Input, id: HandlerId) {
        match msg {
            WorkerCommand::Parse { source } => {
                scope.respond(id, WorkerAnswer::ClearErrors);
                if let Err(e) = make_eval_state(source, "") {
                    scope.respond(id, WorkerAnswer::AddQuietError(e));
                }
            }
            WorkerCommand::StartEval { source, input } => {
                scope.respond(id, WorkerAnswer::ClearOutput);
                scope.respond(id, WorkerAnswer::ClearErrors);
                match make_eval_state(source, &input) {
                    Err(e) => {
                        scope.respond(id, WorkerAnswer::AddError(e));
                        scope.respond(id, WorkerAnswer::Done);
                    }
                    Ok(state) => {
                        self.eval_state = Some(state);
                    }
                }
            }
            WorkerCommand::Advance { count } => {
                if let Err(e) = self
                    .eval_state
                    .as_mut()
                    .unwrap()
                    .advance(count, |e| scope.respond(id, e))
                {
                    scope.respond(id, WorkerAnswer::AddError(e));
                    scope.respond(id, WorkerAnswer::Done);
                }
            }
            WorkerCommand::GoBack { count: _ } => {
                // TODO(veluca): actually go back
            }
            WorkerCommand::JumpTo { position: _ } => {
                // TODO(veluca): actually go to the position
            }
            WorkerCommand::GetCurrentDebugInfo => {
                scope.respond(id, self.eval_state.as_mut().unwrap().current_debug_info());
            }
        };
        scope.respond(id, WorkerAnswer::CommandDone)
    }
}

pub struct EvalBridge {
    worker: SendWrapper<WorkerBridge<PseudocodeEvaluator>>,
    // TODO(veluca): deal with large outputs.
    output: String,
    output_state: SendWrapper<Option<UseStateHandle<String>>>,
    on_done: SendWrapper<Option<Box<dyn Fn() + 'static>>>,
    text_model: SendWrapper<Option<TextModel>>,
    action: CurrentAction,
    has_fresh_debug_info: bool,
}

const MARKER_OWNER: &str = "srs";

impl EvalBridge {
    fn update_output<F: FnOnce(&str) -> String>(&mut self, cb: F) {
        let new_output = cb(&self.output);
        self.output = new_output;
        self.output_state.as_ref().unwrap().set(self.output.clone())
    }

    fn get_model_markers(&mut self) -> Array {
        let filter = Object::new();
        js_sys::Reflect::set(&filter, &"owner".into(), &MARKER_OWNER.into()).unwrap();
        get_model_markers(&filter)
    }

    fn set_variable_values(&mut self, variable_values: Vec<VariableValue>) {
        info!("{:?}", variable_values);
        //
    }

    fn set_expression_values(&mut self, expression_values: Vec<ExpressionValue>) {
        info!("{:?}", expression_values);
        //
    }

    fn handle_answer(&mut self, answer: WorkerAnswer) {
        let add_error = |bridge: &mut Self, err: Error| {
            let markers = bridge.get_model_markers();
            let new_marker = Object::new();
            js_sys::Reflect::set(
                &new_marker,
                &"startLineNumber".into(),
                &err.location.0 .0.into(),
            )
            .unwrap();
            js_sys::Reflect::set(
                &new_marker,
                &"startColumn".into(),
                &err.location.0 .1.into(),
            )
            .unwrap();
            js_sys::Reflect::set(
                &new_marker,
                &"endLineNumber".into(),
                &err.location.1 .0.into(),
            )
            .unwrap();
            js_sys::Reflect::set(&new_marker, &"endColumn".into(), &err.location.1 .1.into())
                .unwrap();
            js_sys::Reflect::set(&new_marker, &"message".into(), &err.message.into()).unwrap();
            js_sys::Reflect::set(
                &new_marker,
                &"severity".into(),
                &MarkerSeverity::Error.to_value().into(),
            )
            .unwrap();

            markers.push(&new_marker);

            set_model_markers(
                bridge.text_model.as_ref().unwrap().as_ref(),
                MARKER_OWNER,
                &markers,
            );
        };
        match answer {
            WorkerAnswer::CommandDone => match self.action {
                // Continue execution until termination if running.
                CurrentAction::Running => self
                    .worker
                    .send(WorkerCommand::Advance { count: NUM_STEPS }),
                CurrentAction::Debugging => {
                    if !self.has_fresh_debug_info {
                        self.has_fresh_debug_info = true;
                        self.worker.send(WorkerCommand::GetCurrentDebugInfo);
                    }
                }
                _ => {}
            },
            WorkerAnswer::ClearOutput => {
                self.update_output(|_| "".into());
            }
            WorkerAnswer::ClearErrors => {
                set_model_markers(
                    self.text_model.as_ref().unwrap().as_ref(),
                    MARKER_OWNER,
                    &Array::new(),
                );
            }
            WorkerAnswer::Done => {
                if let Some(x) = &*self.on_done {
                    x()
                }
            }
            WorkerAnswer::AddError(err) => {
                self.update_output(|current_output| {
                    let msg = "an error occurred";
                    if current_output.is_empty() {
                        msg.into()
                    } else {
                        format!("{current_output}\n{msg}")
                    }
                });
                add_error(self, err);
            }
            WorkerAnswer::AddQuietError(err) => {
                add_error(self, err);
            }
            WorkerAnswer::AppendOutput(out) => {
                self.update_output(|current_output| {
                    if current_output.is_empty() {
                        out
                    } else {
                        format!("{current_output}\n{out}")
                    }
                });
            }
            WorkerAnswer::DebugInfo {
                variable_values,
                expression_values,
            } => {
                self.set_variable_values(variable_values);
                self.set_expression_values(expression_values);
            }
            WorkerAnswer::InvalidateDebugInfo => {
                self.has_fresh_debug_info = false;
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
            text_model: SendWrapper::new(None),
            action: CurrentAction::Editing,
            has_fresh_debug_info: false,
        })
    })
}

pub fn set_output_state(output_state: UseStateHandle<String>) {
    *eval_bridge().lock().unwrap().output_state = Some(output_state)
}

pub fn set_done_callback<F: Fn() + 'static>(on_done: F) {
    *eval_bridge().lock().unwrap().on_done = Some(Box::new(on_done))
}

pub fn set_text_model(text_model: TextModel) {
    *eval_bridge().lock().unwrap().text_model = Some(text_model)
}

pub fn set_action(action: CurrentAction) {
    eval_bridge().lock().unwrap().action = action
}

pub fn send_worker_command(command: WorkerCommand) {
    eval_bridge().lock().unwrap().worker.send(command)
}
