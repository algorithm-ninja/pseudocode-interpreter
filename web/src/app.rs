use gloo_storage::{LocalStorage, Storage};
use log::info;
use monaco::api::TextModel;
use web_sys::HtmlInputElement;
use yew::prelude::*;
use yewprint::Spinner;

use crate::{
    debugger::DebuggerBar,
    editor::Editor,
    eval::{self, set_action, WorkerCommand},
    filemanager::FileManager,
    io::{Input, Output},
    terry::{use_terry, TerryData},
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
    pub input_textarea: NodeRef,
    pub current_output: UseStateHandle<String>,
}

impl GlobalState {
    pub fn set_action(&self, action: CurrentAction) {
        self.action.set(action);
        set_action(action);
    }

    pub fn start_eval(&self, debugging: bool) {
        let code = self.text_model.get_value();
        let input = self
            .input_textarea
            .cast::<HtmlInputElement>()
            .unwrap()
            .value();
        info!("input: {}", &input);
        info!("code: {}", &code);
        let action = self.action.clone();
        if debugging {
            self.set_action(CurrentAction::Debugging);
            eval::set_done_callback(move || {});
        } else {
            self.set_action(CurrentAction::Running);
            eval::set_done_callback(move || action.set(CurrentAction::Editing));
        }
        eval::send_worker_command(WorkerCommand::StartEval {
            source: code,
            input,
        });
    }
}

#[derive(PartialEq, Properties)]
struct LoadedAppProps {
    terry: UseStateHandle<Option<TerryData>>,
}

#[function_component]
fn LoadedApp(terry: &LoadedAppProps) -> Html {
    let terry = use_state_eq(move || terry.terry.as_ref().unwrap().clone());
    let dark_theme = use_state(|| LocalStorage::get("dark-theme").unwrap_or(true));
    let action = use_state(|| CurrentAction::Editing);
    let first_task = terry.contest.tasks[0].name.clone();
    let current_task = use_state(move || first_task);
    let text_model = use_state_eq(|| {
        TextModel::create(
            "function main()\n\toutput(1)\nend function",
            Some(crate::monaco_srs::ID),
            None,
        )
        .unwrap()
    });

    let global_state = GlobalState {
        dark_theme,
        action,
        current_task,
        terry,
        text_model,
        input_textarea: use_node_ref(),
        current_output: use_state_eq(String::new),
    };

    eval::set_output_state(global_state.current_output.clone());

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
