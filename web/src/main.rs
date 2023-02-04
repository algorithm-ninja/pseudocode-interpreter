mod app;
mod debugger;
mod editor;
mod filemanager;
mod io;
mod monaco_srs;
mod terry;
mod topbar;

use app::App;
use log::Level;

fn main() {
    console_log::init_with_level(Level::Info).unwrap();
    monaco_srs::register_srs();
    yew::Renderer::<App>::new().render();
}
