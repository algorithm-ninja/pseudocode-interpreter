use log::Level;
use web::app::App;
use web::monaco_srs::register_srs;

fn main() {
    console_log::init_with_level(Level::Info).unwrap();
    register_srs();
    yew::Renderer::<App>::new().render();
}
