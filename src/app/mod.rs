use std::fmt;
use std::path::PathBuf;

use anyhow::{anyhow, Result};
use clap::{Parser, ValueEnum};
use eframe::{egui, NativeOptions};
use log::{error, info, warn};

#[cfg(target_os = "macos")]
use crate::macos_file_open;
use crate::{gui::FigTreeGui, io, ui};

#[derive(Debug, Clone, Parser)]
#[command(
    name = "rhaeTree",
    about = "Rust prototype GUI that loads phylogenetic trees via egui."
)]
pub struct AppConfig {
    /// Tree file to load (Newick or Nexus formats).
    #[arg(value_name = "TREE_FILE")]
    pub tree_path: Option<PathBuf>,

    /// Optional export path for rendered graphics (not yet implemented).
    #[arg(short, long, value_name = "OUTPUT")]
    pub output: Option<PathBuf>,

    /// Target export format
    #[arg(long, default_value_t = ExportFormat::Png)]
    pub export_format: ExportFormat,

    /// Canvas width in pixels
    #[arg(long, default_value_t = 1100)]
    pub width: u32,

    /// Canvas height in pixels
    #[arg(long, default_value_t = 760)]
    pub height: u32,

    /// Toggle fast-render mode (reserved for future optimisations)
    #[arg(long)]
    pub fast_mode: bool,

    /// Run without launching the GUI; output summary to stdout instead.
    #[arg(long)]
    pub headless: bool,

    /// Force launch of the egui window even when a tree file is provided.
    #[arg(long)]
    pub gui: bool,

    /// Ignore display detection safeguards and attempt to launch the GUI anyway.
    #[arg(long)]
    pub force_gui: bool,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, ValueEnum)]
pub enum ExportFormat {
    Png,
    Jpeg,
    Pdf,
    Svg,
}

impl fmt::Display for ExportFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExportFormat::Png => write!(f, "png"),
            ExportFormat::Jpeg => write!(f, "jpeg"),
            ExportFormat::Pdf => write!(f, "pdf"),
            ExportFormat::Svg => write!(f, "svg"),
        }
    }
}

#[cfg(target_os = "macos")]
mod macos {
    use std::ffi::c_uint;

    #[link(name = "CoreGraphics", kind = "framework")]
    extern "C" {
        fn CGMainDisplayID() -> c_uint;
        fn CGDisplayPixelsWide(display: c_uint) -> usize;
    }

    pub unsafe fn primary_display_width() -> Option<usize> {
        let id = CGMainDisplayID();
        if id == 0 {
            return None;
        }
        Some(CGDisplayPixelsWide(id))
    }
}

pub struct FigTreeApp;

impl FigTreeApp {
    pub fn run(config: &AppConfig) -> Result<()> {
        // Default to GUI mode unless explicitly headless
        let wants_gui = !config.headless || config.gui || config.force_gui;

        if !wants_gui {
            return Self::run_headless(config);
        }

        if !config.force_gui && !Self::display_available() {
            warn!("GUI requested but no display was detected; falling back to headless mode.");
            return Self::run_headless(config);
        }

        // Touch wgpu so Metal/GL backends are linked even without explicit usage elsewhere.
        // let _ = wgpu::Backends::all();

        let mut native_options = NativeOptions::default();
        info!(
            "Launching egui window ({}x{}).",
            config.width, config.height
        );
        native_options.viewport = egui::ViewportBuilder::default()
            .with_title("rhaeTree")
            .with_inner_size(egui::vec2(config.width as f32, config.height as f32));

        let initial_config = config.clone();
        #[cfg(target_os = "macos")]
        macos_file_open::install();

        match eframe::run_native(
            "rhaeTree",
            native_options,
            Box::new(move |cc| Ok(Box::new(FigTreeGui::new(cc, initial_config)))),
        ) {
            Ok(result) => Ok(result),
            Err(err) => {
                error!("Failed to launch egui window: {}", err);
                if config.tree_path.is_some() {
                    warn!("Falling back to headless mode.");
                    Self::run_headless(config)
                } else {
                    Err(anyhow!(err.to_string()))
                }
            }
        }
    }

    fn display_available() -> bool {
        #[cfg(target_os = "macos")]
        {
            unsafe {
                macos::primary_display_width()
                    .map(|width| width > 0)
                    .unwrap_or(false)
            }
        }
        #[cfg(any(
            target_os = "linux",
            target_os = "freebsd",
            target_os = "dragonfly",
            target_os = "netbsd"
        ))]
        {
            std::env::var("DISPLAY").is_ok() || std::env::var("WAYLAND_DISPLAY").is_ok()
        }
        #[cfg(target_os = "windows")]
        {
            true
        }
        #[cfg(not(any(
            target_os = "macos",
            target_os = "linux",
            target_os = "freebsd",
            target_os = "dragonfly",
            target_os = "netbsd",
            target_os = "windows"
        )))]
        {
            false
        }
    }

    fn default_example_path() -> Option<PathBuf> {
        const MANIFEST_DIR: &str = env!("CARGO_MANIFEST_DIR");
        let candidate = PathBuf::from(MANIFEST_DIR).join("../examples/AtSOS1.fasttree.tre");
        if candidate.exists() {
            Some(candidate)
        } else {
            None
        }
    }

    fn run_headless(config: &AppConfig) -> Result<()> {
        let tree_path = config
            .tree_path
            .clone()
            .or_else(Self::default_example_path)
            .ok_or_else(|| {
                anyhow!("headless mode requires TREE_FILE argument or an accessible example tree")
            })?;

        if config.tree_path.is_none() {
            info!("Using example tree at {}", tree_path.display());
        }
        let bundle = io::load_trees(&tree_path)?;
        ui::render_preview(&bundle, config);

        if let Some(dest) = &config.output {
            ui::export(&bundle, config, dest)?;
        }

        Ok(())
    }
}
