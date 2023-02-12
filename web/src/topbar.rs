use gloo_storage::{LocalStorage, Storage};
use gloo_utils::window;
use yew::prelude::*;
use yewprint::{Button, Icon, Intent, Tag};

use crate::app::GlobalState;

#[derive(Properties, PartialEq)]
pub struct TopbarProps {
    pub global_state: GlobalState,
}

#[function_component]
pub fn Topbar(props: &TopbarProps) -> yew::Html {
    let global_state = &props.global_state;

    let toggle_dark_theme = {
        let dark_theme = global_state.dark_theme.clone();
        move |_| {
            let val = !*dark_theme;
            dark_theme.set(val);
            LocalStorage::set("dark-theme", val).unwrap();
        }
    };

    let terry = &global_state.terry;

    let end_time = terry.end_time;
    let get_remaining = move || {
        let now = chrono::Local::now();
        let remaining = (end_time - now).num_seconds();
        format!(
            "{}:{:02}:{:02}",
            remaining / 3600,
            (remaining / 60) % 60,
            remaining % 60
        )
    };

    let remaining = use_state(get_remaining);

    let timer = use_state(|| None);

    {
        let remaining = remaining.clone();
        if timer.is_none() {
            timer.set(Some(gloo_timers::callback::Interval::new(500, move || {
                remaining.set(get_remaining())
            })));
        }
    }

    let task_buttons = terry
        .contest
        .tasks
        .iter()
        .map(|task| &task.name)
        .map(|task_name| {
            let ct = global_state.current_task.clone();
            let intent = if *task_name == *ct {
                Some(Intent::Primary)
            } else {
                None
            };
            let score = format!("{:.0}", terry.tasks.get(task_name).unwrap().score);
            let score_intent = match (
                terry.tasks.get(task_name).unwrap().score,
                terry.tasks.get(task_name).unwrap().max_score,
            ) {
                (x, _) if x == 0.0 => Intent::Danger,
                (x, y) if x == y => Intent::Success,
                _ => Intent::Warning,
            };
            let tn = task_name.clone();
            let onclick = move |_| {
                ct.set(tn.clone());
            };
            html! {
                <Button key={&task_name[..]}
                        intent={intent}
                        onclick={onclick}>
                    { task_name }
                    { " " }
                    <Tag intent={score_intent}>
                        { score }
                    </Tag>
                </Button>
            }
        })
        .collect::<Html>();

    html! {
        <div id="topbar">
            <div id="topbar1">
                <div id="contestname"> { format!("{} ({} {})", terry.contest.name, terry.name, terry.surname ) } </div>
                <div id="countdown"> { &*remaining } </div>
                <Button
                        onclick={|_| { window().open_with_url("./Pseudocodice.pdf").unwrap(); }}
                        icon={Icon::Book}> {"Pseudocode guide"}
                </Button>
                <Button
                        onclick={|_| { window().open_with_url("https://forms.gle/nTr77Qo1EVs6sUzg7").unwrap(); }}
                        icon={Icon::Annotation}> {"Feedback"}
                </Button>
                <Button onclick={toggle_dark_theme}
                        icon={if *global_state.dark_theme {Icon::Flash} else {Icon::Moon}}>
                </Button>
            </div>
            <div id="tasklist">
                { task_buttons }
            </div>
        </div>
    }
}
