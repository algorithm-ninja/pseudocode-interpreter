use js_sys::Object;
use monaco::sys::languages::{ILanguageExtensionPoint, LanguageConfiguration};
use wasm_bindgen::JsCast;

// TODO(veluca): actually customize syntax.

pub const ID: &str = "srs";

pub fn register_srs() {
    monaco::sys::languages::register(&language());
    monaco::sys::languages::set_language_configuration(ID, &language_configuration());
}

fn language() -> ILanguageExtensionPoint {
    let lang: ILanguageExtensionPoint = Object::new().unchecked_into();
    lang.set_id(ID);
    lang
}

fn language_configuration() -> LanguageConfiguration {
    let cfg: LanguageConfiguration = Object::new().unchecked_into();
    cfg
}
