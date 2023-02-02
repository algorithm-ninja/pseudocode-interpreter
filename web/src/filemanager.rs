use gloo_utils::window;
use yew::prelude::*;

use crate::app::{CurrentAction, GlobalState};

use yewprint::{
    id_tree::{InsertBehavior, Node, TreeBuilder},
    Button, ButtonGroup, Icon, Intent, NodeData, Tree, TreeData,
};

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

    let mut tree = TreeBuilder::new().build();

    let root_id = tree
        .insert(
            Node::new(NodeData {
                data: (),
                ..Default::default()
            }),
            InsertBehavior::AsRoot,
        )
        .unwrap();

    let statement_id = tree
        .insert(
            Node::new(NodeData {
                icon: Icon::AlignJustify,
                label: "Statement".into(),
                ..Default::default()
            }),
            InsertBehavior::UnderNode(&root_id),
        )
        .unwrap();

    let current_task = global_state.current_task.clone();
    let onclick = move |(node_id, _)| {
        if node_id == statement_id {
            window()
                .open_with_url(&format!("/task/{}", *current_task))
                .unwrap();
        }
    };

    let tree: TreeData<()> = tree.into();

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
            <div id="fileview">
                <Tree<()>
                    tree={tree}
                    onclick={onclick}
                />
            </div>
        </div>
    }
}
