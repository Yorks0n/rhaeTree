use std::path::Path;

#[cfg(not(target_os = "windows"))]
use cairo::{Context, PdfSurface};

use crate::export::svg::build_export_scene;
#[cfg(not(target_os = "windows"))]
use crate::tree::cairo_renderer::render_scene_to_context;
use crate::tree::layout::TreeLayout;
use crate::tree::painter::TreePainter;
use crate::tree::Tree;

#[cfg(not(target_os = "windows"))]
pub fn export_pdf(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    path: &Path,
    width: f32,
    height: f32,
) -> Result<(), String> {
    let scene = build_export_scene(tree, layout, painter, width, height);

    // Cairo PDF surface uses points (1/72 in), while export scene is in px at 96 DPI.
    let pt_per_px = 72.0_f32 / 96.0_f32;
    let width_pt = (width * pt_per_px).max(1.0);
    let height_pt = (height * pt_per_px).max(1.0);

    let surface = PdfSurface::new(
        width_pt as f64,
        height_pt as f64,
        path.to_str()
            .ok_or_else(|| format!("Invalid UTF-8 path: {}", path.display()))?,
    )
    .map_err(|e| format!("Failed to create PDF surface: {e}"))?;
    let cr = Context::new(&surface).map_err(|e| format!("Failed to create Cairo context: {e}"))?;
    cr.set_antialias(cairo::Antialias::Best);

    render_scene_to_context(&cr, &scene, pt_per_px);

    cr.show_page()
        .map_err(|e| format!("Failed to finalize PDF page: {e}"))?;
    surface.flush();
    surface.finish();
    Ok(())
}

#[cfg(target_os = "windows")]
pub fn export_pdf(
    _tree: &Tree,
    _layout: &TreeLayout,
    _painter: &TreePainter,
    _path: &Path,
    _width: f32,
    _height: f32,
) -> Result<(), String> {
    Err("PDF export is currently unavailable on Windows builds.".to_string())
}
