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
    eval::ProgramState,
    parse::{self, TextAst},
};
use send_wrapper::SendWrapper;

use web_sys::console;
use yew::UseStateHandle;

use crate::app::CurrentAction;

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
enum DecorationKind {
    After(String),
    Hover(String),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Decoration {
    location: ((usize, usize), (usize, usize)),
    kind: DecorationKind,
}

impl Decoration {
    fn into_model_delta_decoration(self) -> Object {
        let range = Object::new();
        js_sys::Reflect::set(
            &range,
            &"startLineNumber".into(),
            &self.location.0 .0.into(),
        )
        .unwrap();
        js_sys::Reflect::set(&range, &"startColumn".into(), &self.location.0 .1.into()).unwrap();
        js_sys::Reflect::set(&range, &"endLineNumber".into(), &self.location.1 .0.into()).unwrap();
        js_sys::Reflect::set(&range, &"endColumn".into(), &self.location.1 .1.into()).unwrap();

        let options = Object::new();
        js_sys::Reflect::set(&options, &"showIfCollapsed".into(), &true.into()).unwrap();
        match self.kind {
            DecorationKind::After(s) => {
                let after = Object::new();
                js_sys::Reflect::set(&after, &"content".into(), &s.into()).unwrap();
                js_sys::Reflect::set(&after, &"inlineClassName".into(), &"variable_value".into())
                    .unwrap();
                js_sys::Reflect::set(&options, &"after".into(), &after).unwrap();
            }
            DecorationKind::Hover(s) => {
                let hover = Object::new();
                js_sys::Reflect::set(&hover, &"value".into(), &s.into()).unwrap();
                js_sys::Reflect::set(&options, &"hoverMessage".into(), &hover).unwrap();
                js_sys::Reflect::set(
                    &options,
                    &"inlineClassName".into(),
                    &"evaluated_expr".into(),
                )
                .unwrap();
            }
        }

        let decoration = Object::new();
        js_sys::Reflect::set(&decoration, &"options".into(), &options).unwrap();
        js_sys::Reflect::set(&decoration, &"range".into(), &range).unwrap();
        decoration
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub enum WorkerAnswer {
    ClearOutput,
    ClearErrors,
    AppendOutput(String),
    Done,
    AddError(Error),
    AddQuietError(Error),
    CommandDone,
    DebugInfo(Vec<Decoration>),
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
    states: Vec<Option<ProgramState<'this, TextAst>>>,
    source: String,
    current_step: usize,
}

pub const NUM_STEPS: usize = 10000;
pub const NUM_STEPS_CACHE: usize = 1024;

impl EvalState {
    fn with_current_state<'outer, Ret>(
        &'outer self,
        callback: impl for<'this> FnOnce(&'outer ProgramState<'this, TextAst>) -> Ret,
    ) -> Ret {
        let cur = *self.borrow_current_step();
        self.with_states(move |steps| callback(steps[cur].as_ref().unwrap()))
    }

    fn advance<F>(&mut self, mut count: usize, callback: F) -> Result<(), Error>
    where
        F: Fn(WorkerAnswer),
    {
        callback(WorkerAnswer::InvalidateDebugInfo);

        let current = *self.borrow_current_step();
        let cached = (current + count) / NUM_STEPS_CACHE * NUM_STEPS_CACHE;
        if cached < self.with_states(|s| s.len()) && cached >= current {
            count -= cached - current;
            self.with_current_step_mut(|s| *s = cached);
            callback(WorkerAnswer::ClearOutput);
            self.with_current_state(|state| {
                for out in state.stdout().iter() {
                    callback(WorkerAnswer::AppendOutput(out.clone()));
                }
            });
        }

        let mut run = || {
            let current_output_len = self.with_current_state(|s| s.stdout().len());
            for _ in 0..count {
                let cur = *self.borrow_current_step();
                let done = self.with_states_mut(|states| {
                    let mut state = if cur % NUM_STEPS_CACHE == 0 {
                        states[cur].as_ref().unwrap().clone()
                    } else {
                        states[cur].take().unwrap()
                    };
                    let done = state.eval_step()?;
                    if states.len() == cur + 1 {
                        states.push(Some(state));
                    } else {
                        states[cur + 1] = Some(state);
                    }
                    Ok(done)
                })?;
                self.with_current_step_mut(|s| *s += 1);
                if done {
                    callback(WorkerAnswer::Done);
                    break;
                }
            }
            let new_output_len = self.with_current_state(|s| s.stdout().len());
            for i in current_output_len..new_output_len {
                callback(WorkerAnswer::AppendOutput(
                    self.with_current_state(|state| state.stdout()[i].clone()),
                ));
            }
            Ok(())
        };
        run().map_err(|e| error_with_location(self.borrow_source(), e))
    }

    fn go_back<F>(&mut self, count: usize, callback: F) -> Result<(), Error>
    where
        F: Fn(WorkerAnswer),
    {
        let current = *self.borrow_current_step();
        let target = current.saturating_sub(count);
        let cached = target / NUM_STEPS_CACHE * NUM_STEPS_CACHE;
        self.with_current_step_mut(|x| *x = cached);
        callback(WorkerAnswer::ClearOutput);
        self.with_current_state(|state| {
            for out in state.stdout().iter() {
                callback(WorkerAnswer::AppendOutput(out.clone()));
            }
        });
        self.advance(target - cached, callback)
    }

    fn current_debug_info(&mut self) -> WorkerAnswer {
        let stack_frames = self.with_current_state(|state| state.stack_frames());
        let mut decorations = vec![];
        for frame in stack_frames {
            for (k, v) in frame.variables {
                let loc = self.with_program(|p| p.var(k)).ident.info.end;
                let loc = get_line_and_char(self.borrow_source(), loc);
                let val = format!("{}", v);
                decorations.push(Decoration {
                    location: (loc, loc),
                    kind: DecorationKind::After(val),
                });
            }
            for (k, v) in frame.temporaries {
                let start = get_line_and_char(self.borrow_source(), k.info.start);
                let end = get_line_and_char(self.borrow_source(), k.info.end);
                let val = self.with_current_state(|state| state.format_expr_value(&v));
                decorations.push(Decoration {
                    location: (start, end),
                    kind: DecorationKind::Hover(val),
                });
            }
        }
        WorkerAnswer::DebugInfo(decorations)
    }
}

fn make_eval_state(source: String, input: &str) -> Result<EvalState, Error> {
    let make = || {
        EvalStateTryBuilder {
            program: parse::parse(&source)?,
            states_builder: |program| {
                let compiled = compile(program)?;
                Ok(vec![Some(ProgramState::new(compiled, input)?)])
            },
            source: source.clone(),
            current_step: 0,
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
            WorkerCommand::GoBack { count } => {
                if let Err(e) = self
                    .eval_state
                    .as_mut()
                    .unwrap()
                    .go_back(count, |e| scope.respond(id, e))
                {
                    scope.respond(id, WorkerAnswer::AddError(e));
                    scope.respond(id, WorkerAnswer::Done);
                }
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

type DoneCallback = Box<dyn Fn(bool, &str) + 'static>;

pub struct EvalBridge {
    worker: SendWrapper<WorkerBridge<PseudocodeEvaluator>>,
    // TODO(veluca): deal with large outputs.
    output: String,
    output_model: SendWrapper<Option<TextModel>>,
    on_done: SendWrapper<Option<DoneCallback>>,
    text_model: SendWrapper<Option<TextModel>>,
    action: CurrentAction,
    action_state: SendWrapper<Option<UseStateHandle<CurrentAction>>>,
    has_fresh_debug_info: bool,
    current_decorations: SendWrapper<Array>,
    has_errors: bool,
}

const MARKER_OWNER: &str = "srs";

impl EvalBridge {
    fn update_output<F: FnOnce(&str) -> String>(&mut self, cb: F) {
        let new_output = cb(&self.output);
        self.output = new_output;
        self.output_model.as_ref().unwrap().set_value(&self.output);
    }

    fn get_model_markers(&mut self) -> Array {
        let filter = Object::new();
        js_sys::Reflect::set(&filter, &"owner".into(), &MARKER_OWNER.into()).unwrap();
        get_model_markers(&filter)
    }

    fn add_error(&mut self, err: Error) {
        let markers = self.get_model_markers();
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
        js_sys::Reflect::set(&new_marker, &"endColumn".into(), &err.location.1 .1.into()).unwrap();
        js_sys::Reflect::set(&new_marker, &"message".into(), &err.message.into()).unwrap();
        js_sys::Reflect::set(
            &new_marker,
            &"severity".into(),
            &MarkerSeverity::Error.to_value().into(),
        )
        .unwrap();

        markers.push(&new_marker);

        set_model_markers(
            self.text_model.as_ref().unwrap().as_ref(),
            MARKER_OWNER,
            &markers,
        );
        self.has_errors = true;
    }

    fn set_decorations(&mut self, decorations: Vec<Decoration>) {
        info!("{:?}", decorations);
        let new_decorations: Array = decorations
            .into_iter()
            .map(Decoration::into_model_delta_decoration)
            .collect();
        self.current_decorations = SendWrapper::new(
            self.text_model
                .as_ref()
                .unwrap()
                .as_ref()
                .delta_decorations(&self.current_decorations, &new_decorations, None),
        );
        console::log(
            &self
                .text_model
                .as_ref()
                .unwrap()
                .as_ref()
                .get_all_decorations(None, Some(false)),
        );
    }

    fn handle_answer(&mut self, answer: WorkerAnswer) {
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
                CurrentAction::Editing => {
                    // Signal that execution stopped
                    if let Some(x) = &*self.on_done {
                        x(self.has_errors, &self.output)
                    }
                }
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
                self.has_errors = false;
            }
            WorkerAnswer::Done => {
                if self.action == CurrentAction::Running {
                    self.action = CurrentAction::Editing;
                    self.action_state
                        .as_ref()
                        .unwrap()
                        .set(CurrentAction::Editing);
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
                self.add_error(err);
            }
            WorkerAnswer::AddQuietError(err) => {
                self.add_error(err);
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
            WorkerAnswer::DebugInfo(decorations) => {
                self.set_decorations(decorations);
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
            output_model: SendWrapper::new(None),
            on_done: SendWrapper::new(None),
            text_model: SendWrapper::new(None),
            action: CurrentAction::Editing,
            action_state: SendWrapper::new(None),
            has_fresh_debug_info: false,
            current_decorations: SendWrapper::new(Array::new()),
            has_errors: false,
        })
    })
}

pub fn set_output_model(output_model: TextModel) {
    *eval_bridge().lock().unwrap().output_model = Some(output_model)
}

pub fn set_action_state(action_state: UseStateHandle<CurrentAction>) {
    *eval_bridge().lock().unwrap().action_state = Some(action_state)
}

pub fn set_done_callback<F: Fn(bool, &str) + 'static>(on_done: F) {
    *eval_bridge().lock().unwrap().on_done = Some(Box::new(on_done))
}

pub fn set_text_model(text_model: TextModel) {
    *eval_bridge().lock().unwrap().text_model = Some(text_model)
}

pub fn set_action(action: CurrentAction) {
    eval_bridge().lock().unwrap().action = action;
    if action == CurrentAction::Editing {
        eval_bridge().lock().unwrap().set_decorations(vec![])
    }
}

pub fn send_worker_command(command: WorkerCommand) {
    eval_bridge().lock().unwrap().worker.send(command)
}
