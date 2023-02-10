use yew::prelude::*;

use crate::app::{CurrentAction, GlobalState};

#[derive(Properties, PartialEq)]
pub struct IoProps {
    pub global_state: GlobalState,
}

#[function_component]
pub fn Input(props: &IoProps) -> yew::Html {
    // TODO(veluca): make ctrl-enter also work on the input.
    html! {
        <div id="input">
            <div class="io-section-name"> { "Input" } </div>
            <textarea placeholder="input" ref={props.global_state.input_textarea.clone()}
                      class={classes!{"bp3-input"}}
                      disabled={*props.global_state.action != CurrentAction::Editing} >
                { &*props.global_state.input }
            </textarea>
        </div>
    }
}

#[function_component]
pub fn Output(props: &IoProps) -> yew::Html {
    html! {
        <div id="output">
            <div class="io-section-name"> { "Output" } </div>
            <pre>{&*props.global_state.current_output}</pre>
        </div>
    }
}
