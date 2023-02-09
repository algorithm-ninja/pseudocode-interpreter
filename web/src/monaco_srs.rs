use js_sys::Object;
use monaco::sys::languages::{CommentRule, ILanguageExtensionPoint, LanguageConfiguration};
use wasm_bindgen::{prelude::wasm_bindgen, JsCast, JsValue};

// TODO(veluca): actually customize syntax.

pub const ID: &str = "srs";

pub fn register_srs() {
    monaco::sys::languages::register(&language());
    monaco::sys::languages::set_language_configuration(ID, &language_configuration());
    monaco::sys::languages::set_monarch_tokens_provider(ID, &tokens_provider());
}

fn language() -> ILanguageExtensionPoint {
    let lang: ILanguageExtensionPoint = Object::new().unchecked_into();
    lang.set_id(ID);
    lang
}

fn language_configuration() -> LanguageConfiguration {
    let cfg: LanguageConfiguration = Object::new().unchecked_into();

    let c: CommentRule = Object::new().unchecked_into();
    c.set_block_comment(None);
    c.set_line_comment(Some("//"));
    cfg.set_comments(Some(&c));

    cfg
}

#[wasm_bindgen(module = "/js/tokens_provider.js")]
extern "C" {
    fn tokens_provider() -> JsValue;
}
