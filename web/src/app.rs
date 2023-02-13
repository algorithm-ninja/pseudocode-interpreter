use futures::StreamExt;
use gloo_storage::{LocalStorage, SessionStorage, Storage};
use gloo_timers::future::IntervalStream;
use log::{info, warn};
use monaco::api::TextModel;
use yew::prelude::*;
use yewprint::Spinner;

use crate::{
    debugger::DebuggerBar,
    editor::Editor,
    eval::{self, set_action, WorkerCommand},
    filemanager::FileManager,
    filestorage,
    io::{Input, Output},
    terry::{self, use_terry, TerryData},
    topbar::Topbar,
};

#[derive(PartialEq, Clone, Debug, Copy)]
pub enum CurrentAction {
    Editing,
    Running,
    Debugging,
}

#[derive(PartialEq, Clone)]
pub struct GlobalState {
    pub dark_theme: UseStateHandle<bool>,
    pub action: UseStateHandle<CurrentAction>,
    pub current_task: UseStateHandle<String>,
    pub terry: UseStateHandle<TerryData>,
    pub text_model: UseStateHandle<TextModel>,
    pub input_model: UseStateHandle<TextModel>,
    pub output_model: UseStateHandle<TextModel>,
}

impl GlobalState {
    pub fn set_action(&self, action: CurrentAction) {
        self.action.set(action);
        set_action(action);
    }

    pub fn start_eval_with_callback<T>(&self, debugging: bool, on_finish: T)
    where
        T: Fn(bool, &str) + 'static,
    {
        let code = self.text_model.get_value();
        let input = self.input_model.get_value();
        info!("input: {}", &input);
        info!("code: {}", &code);
        if debugging {
            self.set_action(CurrentAction::Debugging);
        } else {
            self.set_action(CurrentAction::Running);
        }
        eval::set_done_callback(on_finish);
        eval::send_worker_command(WorkerCommand::StartEval {
            source: code,
            input,
        });
    }

    pub fn start_eval(&self, debugging: bool) {
        self.start_eval_with_callback(debugging, |_, _| {});
    }
}

#[derive(PartialEq, Properties)]
struct LoadedAppProps {
    terry: UseStateHandle<Option<TerryData>>,
}

const DEFAULT_SOURCE: &str = "function main()\n\tvariable i: integer\n\toutput(1)\nend function";

#[function_component]
fn LoadedApp(terry: &LoadedAppProps) -> Html {
    let terry = use_state_eq(move || terry.terry.as_ref().unwrap().clone());
    let dark_theme = use_state(|| LocalStorage::get("dark-theme").unwrap_or(true));
    let action = use_state(|| CurrentAction::Editing);
    let first_task = terry.contest.tasks[0].name.clone();
    let current_task = use_state(move || SessionStorage::get("current-task").unwrap_or(first_task));
    let text_model = use_state_eq(|| {
        TextModel::create(&DEFAULT_SOURCE, Some(crate::monaco_srs::ID), None).unwrap()
    });
    let input_model = use_state_eq(|| TextModel::create("", None, None).unwrap());
    let output_model = use_state_eq(|| TextModel::create("", None, None).unwrap());

    filestorage::set_token(&terry.token);

    {
        let text_model = text_model.clone();
        use_effect_with_deps(
            move |task| {
                let task: &str = task.as_ref();
                info!("Switched to task {task}");
                SessionStorage::set("current-task", task).unwrap();
                filestorage::set_current_task(task);
                let source = filestorage::load().unwrap_or(DEFAULT_SOURCE.to_owned());
                text_model.set_value(&source);
            },
            current_task.clone(),
        );
    }

    {
        let terry = terry.clone();
        wasm_bindgen_futures::spawn_local(async move {
            let terry = terry.clone();
            IntervalStream::new(300_000 /* 5 min */)
                .for_each(|_| {
                    let terry = terry.clone();
                    async {
                        if let Err(e) = terry::refresh_terry(terry).await {
                            warn!("Refreshing terry failed: {e}");
                        }
                    }
                })
                .await;
        });
    }

    let global_state = GlobalState {
        dark_theme,
        action,
        current_task,
        terry,
        text_model,
        input_model,
        output_model,
    };

    eval::set_action_state(global_state.action.clone());

    html! {
        <div class={classes!(if *global_state.dark_theme {"bp3-dark"} else {""})} id="main">
            <Topbar global_state={global_state.clone()} />
            <FileManager global_state={global_state.clone()} />
            <Editor global_state={global_state.clone()} />
            <Input global_state={global_state.clone()} />
            <Output global_state={global_state.clone()}  />
            <DebuggerBar />
        </div>
    }
}

#[function_component]
pub fn App() -> yew::Html {
    let terry = use_terry();

    if terry.is_some() {
        html! {
            <LoadedApp terry={terry} />
        }
    } else {
        html! {
            <div id="main" class={classes!{"loading"}}>
                <div id="loading_spinner">
                    <Spinner />
                    { "Loading... Make sure you are logged in" }
                </div>
            </div>
        }
    }
}
