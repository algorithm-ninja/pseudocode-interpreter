use log::info;
use monaco::{
    api::{CodeEditorOptions, TextModel},
    sys::editor::{BuiltinTheme, IEditorOptionsRenderValidationDecorations, IStandaloneCodeEditor},
    yew::{CodeEditor, CodeEditorLink},
};
use wasm_bindgen::{prelude::Closure, JsCast};
use yew::prelude::*;

use crate::{
    app::{CurrentAction, GlobalState},
    eval::{send_worker_command, set_text_model},
    monaco_srs,
};

#[derive(PartialEq, Properties)]
pub struct EditorProps {
    pub global_state: GlobalState,
}

#[function_component]
pub fn Editor(props: &EditorProps) -> yew::Html {
    let global_state = props.global_state.clone();
    let editor_link = use_state(|| -> Option<CodeEditorLink> { None });

    let text_model = (*global_state.text_model).clone();
    use_effect_with_deps(
        move |text_model| {
            let text_model = (*text_model).clone();
            set_text_model(text_model.clone());
            std::mem::forget(text_model.clone().on_did_change_content(move |_| {
                send_worker_command(crate::eval::WorkerCommand::Parse {
                    source: text_model.get_value(),
                })
            }))
        },
        text_model,
    );

    let on_editor_created = {
        let js_closure = {
            let action = global_state.action.clone();
            let global_state = global_state.clone();
            Closure::<dyn Fn()>::new(move || {
                if *action != CurrentAction::Editing {
                    return;
                }
                global_state.run();
            })
        };
        let editor_link = editor_link.clone();

        use_callback(
            move |my_editor_link: CodeEditorLink, ()| {
                my_editor_link.with_editor(|editor| {
                    let keycode = monaco::sys::KeyCode::Enter.to_value()
                        | (monaco::sys::KeyMod::ctrl_cmd() as u32);
                    let raw_editor: &IStandaloneCodeEditor = editor.as_ref();
                    raw_editor.add_command(
                        keycode.into(),
                        js_closure.as_ref().unchecked_ref(),
                        None,
                    );
                });

                editor_link.set(Some(my_editor_link.clone()));
            },
            (),
        )
    };

    use_effect_with_deps(
        move |(link, action)| {
            if let Some(link) = link.as_ref() {
                link.with_editor(|editor| {
                    let raw_editor: &IStandaloneCodeEditor = editor.as_ref();
                    let opt = CodeEditorOptions::default().to_sys_options();
                    opt.set_read_only(Some(**action != CurrentAction::Editing));
                    raw_editor.update_options_editor(&opt);
                });
            }
        },
        (editor_link, global_state.action),
    );

    html! {
        <div id="editor">
            <EditorWrapper dark_theme={*global_state.dark_theme} {on_editor_created}
                text_model={(*global_state.text_model).clone()} />
        </div>
    }
}

#[derive(PartialEq, Properties)]
struct EditorWrapperProps {
    dark_theme: bool,
    on_editor_created: Callback<CodeEditorLink>,
    text_model: TextModel,
}

#[function_component]
fn EditorWrapper(props: &EditorWrapperProps) -> yew::Html {
    let EditorWrapperProps {
        dark_theme,
        on_editor_created,
        text_model,
    } = props;

    let options = CodeEditorOptions::default()
        .with_language(monaco_srs::ID.to_string())
        .with_builtin_theme(if *dark_theme {
            BuiltinTheme::VsDark
        } else {
            BuiltinTheme::Vs
        })
        .with_automatic_layout(true);

    let options = options.to_sys_options();
    options.set_read_only(Some(false));
    options.set_render_validation_decorations(Some(IEditorOptionsRenderValidationDecorations::On));

    html! {
        <div id="editor">
            <CodeEditor options={ options } {on_editor_created}
                        model={text_model.clone()} />
        </div>
    }
}
