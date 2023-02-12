use js_sys::Object;
use monaco::{api::CodeEditorOptions, sys::editor::{BuiltinTheme, LineNumbersType, IEditorMinimapOptions}, yew::CodeEditor};
use wasm_bindgen::JsCast;
use yew::prelude::*;

use crate::{
    app::{CurrentAction, GlobalState},
    eval::set_output_model,
};

#[derive(Properties, PartialEq)]
pub struct IoProps {
    pub global_state: GlobalState,
}

#[function_component]
pub fn Input(props: &IoProps) -> yew::Html {
    // TODO(veluca): make ctrl-enter also work on the input.

    let dark_theme = props.global_state.dark_theme.clone();
    let model = props.global_state.input_model.clone();

    let options = CodeEditorOptions::default()
        .with_builtin_theme(if *dark_theme {
            BuiltinTheme::VsDark
        } else {
            BuiltinTheme::Vs
        })
        .with_scroll_beyond_last_line(false)
        .with_automatic_layout(true);

    let options = options.to_sys_options();
    options.set_read_only(Some(*props.global_state.action != CurrentAction::Editing));
    options.set_line_numbers(Some(LineNumbersType::Off));
    let minimap: IEditorMinimapOptions = Object::new().unchecked_into();
    minimap.set_enabled(Some(false));
    options.set_minimap(Some(&minimap));

    html! {
        <div id="input">
            <div class="io-section-name"> { "Input" } </div>
            <div class="editor">
                <CodeEditor {options} model={(*model).clone()} />
            </div>
        </div>
    }
}

#[function_component]
pub fn Output(props: &IoProps) -> yew::Html {
    let dark_theme = props.global_state.dark_theme.clone();
    let model = props.global_state.output_model.clone();

    let options = CodeEditorOptions::default()
        .with_builtin_theme(if *dark_theme {
            BuiltinTheme::VsDark
        } else {
            BuiltinTheme::Vs
        })
        .with_scroll_beyond_last_line(false)
        .with_automatic_layout(true);

    let options = options.to_sys_options();
    options.set_read_only(Some(true));
    options.set_line_numbers(Some(LineNumbersType::Off));
    let minimap: IEditorMinimapOptions = Object::new().unchecked_into();
    minimap.set_enabled(Some(false));
    options.set_minimap(Some(&minimap));

    let text_model = (*model).clone();
    use_effect_with_deps(
        move |text_model| {
            set_output_model((*text_model).clone());
        },
        text_model,
    );

    html! {
        <div id="output">
            <div class="io-section-name"> { "Output" } </div>
            <div class="editor">
                <CodeEditor {options} model={(*model).clone()} />
            </div>
        </div>
    }
}
