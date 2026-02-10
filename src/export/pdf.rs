use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

use eframe::egui;
use font_kit::family_name::FamilyName;
use font_kit::properties::Properties;
use font_kit::source::SystemSource;
use printpdf::{BuiltinFont, Color, Mm, PdfDocument, Pt, Rgb, Svg, SvgTransform, TextMatrix};

use crate::export::svg::{build_export_scene, scene_to_svg_document};
use crate::tree::layout::TreeLayout;
use crate::tree::painter::TreePainter;
use crate::tree::scene_graph::{ScenePrimitive, TreeSceneGraph};
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
    let mut shapes_only = TreeSceneGraph {
        size: scene.size,
        primitives: scene
            .primitives
            .iter()
            .filter(|primitive| !matches!(primitive, ScenePrimitive::Text { .. }))
            .cloned()
            .collect(),
        tip_label_hits: Vec::new(),
    };
    // Keep layer ordering for non-text primitives intact.
    let document = scene_to_svg_document(&shapes_only);
    shapes_only.primitives.clear();
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

    draw_text_primitives_to_pdf(&doc, &current_layer, &scene, height)?;

    let file = File::create(path).map_err(|e| format!("Failed to create PDF file: {e}"))?;
    doc.save(&mut BufWriter::new(file))
        .map_err(|e| format!("Failed to save PDF: {e}"))
}

fn draw_text_primitives_to_pdf(
    doc: &printpdf::PdfDocumentReference,
    layer: &printpdf::PdfLayerReference,
    scene: &TreeSceneGraph,
    page_height_px: f32,
) -> Result<(), String> {
    let font = load_pdf_font(doc)?;

    for primitive in &scene.primitives {
        let ScenePrimitive::Text {
            text,
            anchor,
            angle,
            align,
            size,
            color,
        } = primitive
        else {
            continue;
        };

        if text.is_empty() || color.a() == 0 {
            continue;
        }

        let font_size_px = (*size).max(1.0);
        let est_width_px = estimate_text_width_px(text, font_size_px);
        let x_px = anchor_x_for_alignment(anchor.x, *align, est_width_px);
        // SVG text is middle-anchored in y; printpdf positions by baseline.
        let y_px = anchor.y + font_size_px * 0.35;

        let x_pt = px_to_pt(x_px);
        let y_pt = px_to_pt(page_height_px - y_px);
        let font_size_pt = px_to_pt(font_size_px);

        layer.begin_text_section();
        layer.set_fill_color(to_pdf_color(*color));
        layer.set_font(&font, font_size_pt);
        layer.set_text_matrix(TextMatrix::TranslateRotate(
            Pt(x_pt),
            Pt(y_pt),
            -angle.to_degrees(),
        ));
        layer.write_text(text.clone(), &font);
        layer.end_text_section();
    }

    Ok(())
}

fn load_pdf_font(
    doc: &printpdf::PdfDocumentReference,
) -> Result<printpdf::IndirectFontRef, String> {
    if let Some(bytes) = load_system_sans_font_bytes() {
        let mut cursor = std::io::Cursor::new(bytes);
        if let Ok(font) = doc.add_external_font(&mut cursor) {
            return Ok(font);
        }
    }
    doc.add_builtin_font(BuiltinFont::Helvetica)
        .map_err(|e| format!("Failed to load PDF font: {e}"))
}

fn load_system_sans_font_bytes() -> Option<Vec<u8>> {
    let source = SystemSource::new();
    let handle = source
        .select_best_match(&[FamilyName::SansSerif], &Properties::new())
        .ok()?;
    let font = handle.load().ok()?;
    let bytes = font.copy_font_data()?;
    Some(bytes.as_ref().clone())
}

fn anchor_x_for_alignment(x: f32, align: egui::Align2, text_width: f32) -> f32 {
    match align {
        egui::Align2::LEFT_CENTER => x,
        egui::Align2::RIGHT_CENTER => x - text_width,
        _ => x - text_width * 0.5,
    }
}

fn estimate_text_width_px(text: &str, font_size_px: f32) -> f32 {
    let count = text.chars().count() as f32;
    count * font_size_px * 0.56
}

fn px_to_pt(px: f32) -> f32 {
    px * 72.0 / 96.0
}

fn to_pdf_color(color: egui::Color32) -> Color {
    Color::Rgb(Rgb::new(
        color.r() as f32 / 255.0,
        color.g() as f32 / 255.0,
        color.b() as f32 / 255.0,
        None,
    ))
}
