use std::collections::{HashMap, HashSet};

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

    let template_id = tree
        .insert(
            Node::new(NodeData {
                icon: Icon::Document,
                label: format!("Solution template").into(),
                ..Default::default()
            }),
            InsertBehavior::UnderNode(&root_id),
        )
        .unwrap();

    let example_id = tree
        .insert(
            Node::new(NodeData {
                icon: Icon::Document,
                label: format!("Example input").into(),
                ..Default::default()
            }),
            InsertBehavior::UnderNode(&root_id),
        )
        .unwrap();

    let mut submissions_sources = HashMap::new();
    let mut submissions_inputs = HashMap::new();

    for (id, info) in global_state
        .terry
        .tasks
        .get(&*global_state.current_task)
        .unwrap()
        .submissions
        .iter()
        .enumerate()
    {
        let sub_node = tree
            .insert(
                Node::new(NodeData {
                    icon: Icon::FolderOpen,
                    label: format!("Submission {} ({:.0} points)", id + 1, info.score).into(),
                    is_expanded: true,
                    ..Default::default()
                }),
                InsertBehavior::UnderNode(&root_id),
            )
            .unwrap();
        submissions_sources.insert(
            tree.insert(
                Node::new(NodeData {
                    icon: Icon::Document,
                    label: format!("Source file").into(),
                    ..Default::default()
                }),
                InsertBehavior::UnderNode(&sub_node),
            )
            .unwrap(),
            id,
        );
        submissions_inputs.insert(
            tree.insert(
                Node::new(NodeData {
                    icon: Icon::Document,
                    label: format!("Input file").into(),
                    ..Default::default()
                }),
                InsertBehavior::UnderNode(&sub_node),
            )
            .unwrap(),
            id,
        );
    }

    let current_task = global_state.current_task.clone();
    let onclick = move |(node_id, _)| {
        if node_id == statement_id {
            window()
                .open_with_url(&format!("/task/{}", *current_task))
                .unwrap();
        }
        if node_id == template_id {
            // TODO(veluca): download template
        }
        if node_id == example_id {
            // TODO(veluca): download example
        }
        if let Some(_) = submissions_sources.get(&node_id) {
            // TODO(veluca): download input
        }
        if let Some(_) = submissions_inputs.get(&node_id) {
            // TODO(veluca): download input
        }
    };

    // TODO(veluca): saved files

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
