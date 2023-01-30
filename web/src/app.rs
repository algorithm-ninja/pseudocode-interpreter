use yew::prelude::*;
use yewprint::Spinner;

use crate::{
    debugger::DebuggerBar,
    editor::Editor,
    filemanager::FileManager,
    io::{Input, Output},
    terry::{use_terry, TerryData},
    topbar::Topbar,
};

#[derive(PartialEq)]
pub struct GlobalState {
    pub dark_theme: UseStateHandle<bool>,
    pub debugging: UseStateHandle<bool>,
    pub current_task: UseStateHandle<String>,
    pub terry: UseStateHandle<TerryData>,
}

#[derive(PartialEq, Properties)]
struct LoadedAppProps {
    terry: UseStateHandle<Option<TerryData>>,
}

#[function_component]
fn LoadedApp(terry: &LoadedAppProps) -> Html {
    let terry = use_state_eq(move || terry.terry.as_ref().unwrap().clone());
    let dark_theme = use_state(|| true);
    let debugging = use_state(|| false);
    let first_task = terry.contest.tasks[0].name.clone();
    let current_task = use_state(move || first_task);

    let global_state = GlobalState {
        dark_theme,
        debugging,
        current_task,
        terry,
    };

    html! {
        <div class={classes!(if *global_state.dark_theme {"bp3-dark"} else {""})} id="main">
            <Topbar global_state={global_state} />
            <FileManager />
            <Editor />
            <Input />
            <Output />
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
