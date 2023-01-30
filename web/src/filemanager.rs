use yew::prelude::*;

use crate::app::{CurrentAction, GlobalState};

use yewprint::{Button, ButtonGroup, Icon, Intent};

#[derive(Properties, PartialEq)]
pub struct FileManagerProps {
    pub global_state: GlobalState,
}

#[function_component]
pub fn FileManager(props: &FileManagerProps) -> yew::Html {
    let global_state = &props.global_state;

    let start_program = {
        let action = global_state.action.clone();
        move |_| {
            // TODO(veluca): actually do something.
            action.set(CurrentAction::Running);
        }
    };

    let stop_program = {
        let action = global_state.action.clone();
        move |_| {
            // TODO(veluca): actually do something.
            action.set(CurrentAction::Editing);
        }
    };

    let debug_program = {
        let action = global_state.action.clone();
        move |_| {
            // TODO(veluca): actually do something.
            action.set(CurrentAction::Debugging);
        }
    };

    html! {
        <div id="filemanager">
            // TODO(veluca): do something with debugging controls and submit button.
            <Button icon={Icon::SendMessage} intent={Intent::Success} class={classes!("submitbutton")}
                    disabled={*global_state.action != CurrentAction::Editing}>{"Submit"}</Button>
            <div id="maincontrols">
                <ButtonGroup>
                    <Button icon={Icon::Play} intent={Intent::Primary}
                            onclick={start_program}
                            disabled={*global_state.action != CurrentAction::Editing}>{"Run"}</Button>
                    <Button icon={Icon::Stop}
                            onclick={stop_program}
                            disabled={*global_state.action == CurrentAction::Editing}>{"Stop"}</Button>
                    <Button icon={Icon::Stopwatch} intent={Intent::Warning}
                            onclick={debug_program}
                            disabled={*global_state.action != CurrentAction::Editing}>{"Debug"}</Button>
                </ButtonGroup>
            </div>
            <div id="debuggingcontrols">
                <ButtonGroup>
                    <Button icon={Icon::FastBackward}
                            disabled={*global_state.action != CurrentAction::Debugging}></Button>
                    <Button icon={Icon::StepBackward}
                            disabled={*global_state.action != CurrentAction::Debugging}></Button>
                    <Button icon={Icon::StepForward}
                            disabled={*global_state.action != CurrentAction::Debugging}></Button>
                    <Button icon={Icon::FastForward}
                            disabled={*global_state.action != CurrentAction::Debugging}></Button>
                </ButtonGroup>
            </div>
            <div id="fileview">{"file manager here"}</div>
        </div>
    }
}
