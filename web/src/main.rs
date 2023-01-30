mod app;
mod debugger;
mod editor;
mod filemanager;
mod io;
mod terry;
mod topbar;

use app::App;
use log::Level;

fn main() {
    console_log::init_with_level(Level::Info).unwrap();
    yew::Renderer::<App>::new().render();
}
