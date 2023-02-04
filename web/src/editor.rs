use log::info;
use monaco::{
    api::CodeEditorOptions,
    sys::editor::{BuiltinTheme, IStandaloneCodeEditor},
    yew::{CodeEditor, CodeEditorLink},
};
use wasm_bindgen::{prelude::Closure, JsCast};
use yew::prelude::*;

use crate::{
    app::{CurrentAction, GlobalState},
    monaco_srs,
};

#[derive(PartialEq, Properties)]
pub struct EditorProps {
    pub global_state: GlobalState,
}

#[function_component]
pub fn Editor(props: &EditorProps) -> yew::Html {
    let global_state = &props.global_state;

    let on_editor_created = {
        let text_model = global_state.text_model.clone();
        let code = global_state.current_code.clone();

        let js_closure = {
            let code = code.clone();
            let text_model = text_model.clone();
            let action = global_state.action.clone();
            Closure::<dyn Fn()>::new(move || {
                if *action != CurrentAction::Editing {
                    return;
                }
                action.set(CurrentAction::Running);
                info!("{}", text_model.get_value());
                code.set(text_model.get_value());
            })
        };

        use_callback(
            move |editor_link: CodeEditorLink, ()| {
                editor_link.with_editor(|editor| {
                    let keycode = monaco::sys::KeyCode::Enter.to_value()
                        | (monaco::sys::KeyMod::ctrl_cmd() as u32);
                    let raw_editor: &IStandaloneCodeEditor = editor.as_ref();
                    raw_editor.add_command(
                        keycode.into(),
                        js_closure.as_ref().unchecked_ref(),
                        None,
                    );
                });
            },
            (),
        )
    };

    let options = CodeEditorOptions::default()
        .with_language(monaco_srs::ID.to_string())
        .with_builtin_theme(if *global_state.dark_theme {
            BuiltinTheme::VsDark
        } else {
            BuiltinTheme::Vs
        })
        .with_automatic_layout(true);

    let options = options.to_sys_options();

    options.set_read_only(Some(*global_state.action != CurrentAction::Editing));

    html! {
        <div id="editor">
            <CodeEditor options={ options } {on_editor_created}
                        model={(*global_state.text_model).clone()} />
        </div>
    }
}
