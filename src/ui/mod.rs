use std::path::Path;

use anyhow::Result;

use crate::app::{AppConfig, ExportFormat};
use crate::tree::TreeBundle;

pub fn render_preview(bundle: &TreeBundle, config: &AppConfig) {
    println!(
        "Loaded {:?} file with {} tree(s).",
        bundle.format,
        bundle.trees.len()
    );

    if bundle.trees.is_empty() {
        return;
    }

    println!(
        "Previewing first {} tree(s) at {}x{} px (fast mode: {}).",
        bundle.trees.len().min(3),
        config.width,
        config.height,
        config.fast_mode
    );

    for tree in bundle.trees.iter().take(3) {
        let preview = if tree.newick.len() > 64 {
            format!("{}...", &tree.newick[..64])
        } else {
            tree.newick.clone()
        };

        match &tree.label {
            Some(label) => println!("- {} => {}", label, preview),
            None => println!("- tree #{} => {}", tree.id + 1, preview),
        }
    }

    if bundle.trees.len() > 3 {
        println!("... ({} more tree(s) omitted)", bundle.trees.len() - 3);
    }
}

pub fn export(bundle: &TreeBundle, config: &AppConfig, output: &Path) -> Result<()> {
    println!(
        "[stub] Exporting {} tree(s) to {} as {:?} ({}x{}).",
        bundle.trees.len(),
        output.display(),
        config.export_format,
        config.width,
        config.height
    );
    if config.export_format == ExportFormat::Pdf {
        println!("PDF export is not implemented yet; integrate a renderer (egui, vello, etc.)");
    }
    Ok(())
}
