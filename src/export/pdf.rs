use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

use printpdf::{Mm, PdfDocument, Svg, SvgTransform};

use crate::export::svg::{build_export_scene, scene_to_svg_document};
use crate::tree::layout::TreeLayout;
use crate::tree::painter::TreePainter;
use crate::tree::Tree;

pub fn export_pdf(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    path: &Path,
    width: f32,
    height: f32,
) -> Result<(), String> {
    let scene = build_export_scene(tree, layout, painter, width, height);
    let document = scene_to_svg_document(&scene);
    let svg_string = document.to_string();

    let width_mm = width * 25.4 / 96.0;
    let height_mm = height * 25.4 / 96.0;
    let (doc, page, layer) =
        PdfDocument::new("FigTree Export", Mm(width_mm), Mm(height_mm), "Layer 1");

    let current_layer = doc.get_page(page).get_layer(layer);
    let pdf_svg = Svg::parse(&svg_string).map_err(|e| format!("Failed to parse SVG: {e:?}"))?;
    pdf_svg.add_to_layer(
        &current_layer,
        SvgTransform {
            translate_x: None,
            translate_y: None,
            rotate: None,
            scale_x: None,
            scale_y: None,
            dpi: Some(96.0),
        },
    );

    let file = File::create(path).map_err(|e| format!("Failed to create PDF file: {e}"))?;
    doc.save(&mut BufWriter::new(file))
        .map_err(|e| format!("Failed to save PDF: {e}"))
}
