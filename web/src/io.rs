use yew::prelude::*;

#[function_component]
pub fn Input() -> yew::Html {
    html! {
        <div id="input">
            {"input here"}
        </div>
    }
}

#[function_component]
pub fn Output() -> yew::Html {
    html! {
        <div id="output">
            {"output here"}
        </div>
    }
}
