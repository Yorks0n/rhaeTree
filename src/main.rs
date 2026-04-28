mod app;
mod export;
mod gui;
mod io;
mod macos_file_open;
mod tree;
mod ui;

use app::{AppConfig, FigTreeApp};
use clap::Parser;

fn main() {
    let _ = env_logger::builder().format_timestamp(None).try_init();

    // Must run before eframe::run_native takes over the main thread:
    // swizzles -[NSApplication setDelegate:] so winit's delegate gets
    // application:openURLs: patched onto its class before AppKit ever
    // queries respondsToSelector:. No-op on non-macOS.
    macos_file_open::install_early_hook();

    let config = AppConfig::parse();
    if let Err(err) = FigTreeApp::run(&config) {
        eprintln!("error: {err}");
        std::process::exit(1);
    }
}
