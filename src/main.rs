mod app;
mod export;
mod gui;
mod io;
mod rotated_text;
mod tree;
mod ui;

use app::{AppConfig, FigTreeApp};
use clap::Parser;

fn main() {
    let _ = env_logger::builder().format_timestamp(None).try_init();

    let config = AppConfig::parse();
    if let Err(err) = FigTreeApp::run(&config) {
        eprintln!("error: {err}");
        std::process::exit(1);
    }
}
