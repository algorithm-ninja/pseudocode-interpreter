use std::collections::HashMap;

use gloo_utils::window;
use log::{info, warn};
use yew::prelude::*;

use crate::{
    app::{CurrentAction, GlobalState},
    eval::{send_worker_command, WorkerCommand},
    terry,
};

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
    let global_state = props.global_state.clone();

    let start_program = {
        let global_state = global_state.clone();
        move |_| {
            global_state.start_eval(false);
        }
    };

    let stop_program = {
        let global_state = global_state.clone();
        move |_| {
            global_state.set_action(CurrentAction::Editing);
        }
    };

    let debug_program = {
        let global_state = global_state.clone();
        move |_| {
            global_state.start_eval(true);
        }
    };

    let mut tree = TreeBuilder::new().build();

    // Yewprint's tree will only fire a single callback per element per render.
    // Our click events don't change the tree state, so we need some artificial
    // state to force a re-render and allow multiple clicks per element.
    let yewprint_workaround_state = use_state(|| ());

    let root_id = tree
        .insert(
            Node::new(NodeData {
                data: *yewprint_workaround_state,
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
                label: "Solution template".to_string().into(),
                ..Default::default()
            }),
            InsertBehavior::UnderNode(&root_id),
        )
        .unwrap();

    let example_id = tree
        .insert(
            Node::new(NodeData {
                icon: Icon::Document,
                label: "Example input".to_string().into(),
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
                    label: "Source file".to_string().into(),
                    ..Default::default()
                }),
                InsertBehavior::UnderNode(&sub_node),
            )
            .unwrap(),
            info.source.path.clone().unwrap(),
        );
        submissions_inputs.insert(
            tree.insert(
                Node::new(NodeData {
                    icon: Icon::Document,
                    label: "Input file".to_string().into(),
                    ..Default::default()
                }),
                InsertBehavior::UnderNode(&sub_node),
            )
            .unwrap(),
            info.input.path.clone(),
        );
    }

    let current_task = global_state.current_task.clone();
    let text_model = global_state.text_model.clone();
    let input_model = global_state.input_model.clone();
    let terry = global_state.terry.clone();

    let onclick = move |(node_id, _)| {
        // Force a re-render of the tree
        yewprint_workaround_state.set(());

        if node_id == statement_id {
            window()
                .open_with_url(&format!("/task/{}", *current_task))
                .unwrap();
        }
        if node_id == template_id
            && gloo_dialogs::confirm("This will overwrite the current source code. Continue?")
        {
            let text_model = text_model.clone();
            let current_task = current_task.clone();
            let terry = terry.clone();
            wasm_bindgen_futures::spawn_local(async move {
                let path = format!("{}.srs", &*current_task);
                match terry::download_statement_file(terry.clone(), &current_task, &path).await {
                    Err(e) => warn!("Downloading source failed: {e}"),
                    Ok(source) if !source.is_empty() => text_model.set_value(&source),
                    _ => {}
                };
            });
        }
        if node_id == example_id
            && gloo_dialogs::confirm("This will overwrite the current input. Continue?")
        {
            let current_task = current_task.clone();
            let input_model = input_model.clone();
            let terry = terry.clone();
            wasm_bindgen_futures::spawn_local(async move {
                let path = format!("{}_input_example.txt", &*current_task);
                match terry::download_statement_file(terry.clone(), &current_task, &path).await {
                    Err(e) => warn!("Downloading input failed: {e}"),
                    Ok(source) if !source.is_empty() => input_model.set_value(&source),
                    _ => {}
                };
            });
        }
        if let Some(path) = submissions_sources.get(&node_id) {
            if gloo_dialogs::confirm("This will overwrite the current source code. Continue?") {
                let path = path.clone();
                let text_model = text_model.clone();
                wasm_bindgen_futures::spawn_local(async move {
                    match terry::download_file(&path).await {
                        Err(e) => warn!("Downloading source failed: {e}"),
                        Ok(source) => text_model.set_value(&source),
                    };
                });
            }
        }
        if let Some(path) = submissions_inputs.get(&node_id) {
            if gloo_dialogs::confirm("This will overwrite the current input. Continue?") {
                let path = path.clone();
                let input_model = input_model.clone();
                wasm_bindgen_futures::spawn_local(async move {
                    match terry::download_file(&path).await {
                        Err(e) => warn!("Downloading input failed: {e}"),
                        Ok(source) => input_model.set_value(&source),
                    };
                });
            }
        }
    };

    const STEP_SIZE: usize = 500;

    let fastbw = move |_| send_worker_command(WorkerCommand::GoBack { count: STEP_SIZE });
    let stepbw = move |_| send_worker_command(WorkerCommand::GoBack { count: 1 });
    let stepfw = move |_| send_worker_command(WorkerCommand::Advance { count: 1 });
    let fastfw = move |_| send_worker_command(WorkerCommand::Advance { count: STEP_SIZE });

    let sub_in_progress = use_state(|| false);

    let submit = {
        let global_state = global_state.clone();
        let sub_in_progress = sub_in_progress.clone();

        move |_| {
            let global_state = global_state.clone();
            let sub_in_progress = sub_in_progress.clone();

            info!("Submit");
            sub_in_progress.set(true);

            let task: String = (*global_state.current_task).to_owned();

            wasm_bindgen_futures::spawn_local(async move {
                let input = terry::download_input(global_state.terry.clone(), &task).await;

                let (input, input_info) = match input {
                    Ok(input) => input,
                    Err(err) => {
                        warn!("Downloading input failed: {err:?}");
                        sub_in_progress.set(false);
                        return;
                    }
                };

                global_state.input_model.set_value(&input);

                let terry = global_state.terry.clone();
                let source = global_state.text_model.get_value();
                global_state.start_eval_with_callback(false, move |has_errors, output| {
                    let terry = terry.clone();
                    let task = task.clone();
                    let sub_in_progress = sub_in_progress.clone();
                    let output = output.to_owned();
                    let source = source.clone();
                    let input_info = input_info.clone();

                    wasm_bindgen_futures::spawn_local(async move {
                        info!("has_errors: {has_errors}. Result: {output}");

                        if !has_errors {
                            if let Err(e) = terry::submit(
                                terry.clone(),
                                &task,
                                &input_info.id,
                                &source,
                                &output,
                            )
                            .await
                            {
                                warn!("Submission failed: {e}");
                            }
                        }

                        sub_in_progress.set(false);
                    });
                });
            });
        }
    };

    let tree: TreeData<()> = tree.into();

    html! {
        <div id="filemanager">
            <Button icon={Icon::SendMessage} intent={Intent::Success} class={classes!("submitbutton")}
                    onclick={submit}
                    loading={*sub_in_progress}
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
                            onclick={fastbw}
                            disabled={*global_state.action != CurrentAction::Debugging}></Button>
                    <Button icon={Icon::StepBackward}
                            onclick={stepbw}
                            disabled={*global_state.action != CurrentAction::Debugging}></Button>
                    <Button icon={Icon::StepForward}
                            onclick={stepfw}
                            disabled={*global_state.action != CurrentAction::Debugging}></Button>
                    <Button icon={Icon::FastForward}
                            onclick={fastfw}
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
