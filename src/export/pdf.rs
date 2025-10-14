use eframe::egui::{self, pos2};
use font_kit::handle::Handle;
use font_kit::source::SystemSource;
use printpdf::path::PaintMode;
use printpdf::*;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

use crate::tree::layout::{TreeLayout, TreeLayoutType};
use crate::tree::painter::TreePainter;
use crate::tree::{NodeId, Tree};

/// Label type for different text elements
#[derive(Debug, Clone, PartialEq)]
enum LabelType {
    Tip,
    Node,
    Branch,
}

/// Text element for PDF rendering
#[derive(Debug, Clone)]
struct TextElement {
    text: String,
    x: f32,
    y: f32,
    font_name: String,
    font_size: f32,
    rotation: f32,
    color: eframe::egui::Color32,
    text_anchor: String,
    label_type: LabelType,
}

#[derive(Debug, Clone)]
struct HighlightPolygon {
    points: Vec<(f32, f32)>,
    color: eframe::egui::Color32,
}

/// Find system font by name
fn find_font(font_name: &str) -> Result<String, String> {
    // Try common font names first
    let font_to_try = if font_name.is_empty() {
        "Helvetica"
    } else {
        font_name
    };

    // Try to find system font
    match SystemSource::new().select_by_postscript_name(font_to_try) {
        Ok(handle) => {
            match handle {
                Handle::Path { path, .. } => Ok(path.to_str().unwrap_or("").to_string()),
                _ => {
                    // Font is in memory, try fallback
                    Err(format!(
                        "Font {} is in memory, using built-in font",
                        font_name
                    ))
                }
            }
        }
        Err(_) => {
            // Font not found, will use built-in
            Err(format!("Font {} not found, using built-in font", font_name))
        }
    }
}

/// Create font cache for all text elements
fn create_font_cache(
    doc: &PdfDocumentReference,
    texts: &[TextElement],
) -> Result<HashMap<String, IndirectFontRef>, String> {
    let mut result = HashMap::new();

    for te in texts {
        if !result.contains_key(&te.font_name) {
            // Try to find and load system font
            let font = match find_font(&te.font_name) {
                Ok(font_path) => {
                    // Load external font
                    match File::open(&font_path) {
                        Ok(file) => {
                            match doc.add_external_font(file) {
                                Ok(font) => font,
                                Err(_) => {
                                    // Fall back to built-in font
                                    doc.add_builtin_font(BuiltinFont::Helvetica).map_err(|e| {
                                        format!("Failed to load built-in font: {}", e)
                                    })?
                                }
                            }
                        }
                        Err(_) => {
                            // Fall back to built-in font
                            doc.add_builtin_font(BuiltinFont::Helvetica)
                                .map_err(|e| format!("Failed to load built-in font: {}", e))?
                        }
                    }
                }
                Err(_) => {
                    // Use built-in font as fallback
                    doc.add_builtin_font(BuiltinFont::Helvetica)
                        .map_err(|e| format!("Failed to load built-in font: {}", e))?
                }
            };

            result.insert(te.font_name.clone(), font);
        }
    }

    Ok(result)
}

/// Generate SVG without text elements
fn generate_svg_without_text(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    width: f32,
    height: f32,
) -> (String, Vec<TextElement>, Vec<HighlightPolygon>) {
    use ::svg::node::element::{Circle, Group, Line, Path as SvgPath, Polygon};
    use ::svg::Document;

    let mut text_elements = Vec::new();

    // Calculate margins
    let margin_x = (width * 0.05).max(20.0).min(60.0);
    let margin_y = (height * 0.05).max(20.0).min(40.0);

    // Calculate inner dimensions
    let inner_width = width - 2.0 * margin_x;
    let inner_height = height - 2.0 * margin_y;

    // Calculate scale factors
    let scale_x = if layout.width <= f32::EPSILON {
        inner_width.max(1.0)
    } else {
        inner_width.max(1.0) / layout.width
    };

    let scale_y = if layout.height <= f32::EPSILON {
        inner_height.max(1.0)
    } else {
        inner_height.max(1.0) / layout.height
    };

    let tip_extensions = if painter.align_tip_labels {
        calculate_tip_extensions(tree)
    } else {
        HashMap::new()
    };

    // Pre-compute transform so radial/donut layouts match the interactive viewer
    let radial_transform = compute_radial_transform(
        tree,
        layout,
        painter,
        &tip_extensions,
        margin_x,
        margin_y,
        inner_width,
        inner_height,
    );

    // Coordinate transformation function
    let to_svg_coords = |pos: (f32, f32)| -> (f32, f32) {
        if let Some(transform) = &radial_transform {
            let mapped = transform.map(pos);
            (mapped.x, mapped.y)
        } else {
            let x = margin_x + pos.0 * scale_x;
            let y = margin_y + pos.1 * scale_y;
            (x, y)
        }
    };

    // Create SVG document
    let mut document = Document::new()
        .set("width", width)
        .set("height", height)
        .set("viewBox", (0, 0, width as i32, height as i32));

    // Create group for tree elements
    let mut tree_group = Group::new().set("id", "tree");
    let mut highlight_polygons: Vec<HighlightPolygon> = Vec::new();

    match layout.layout_type {
        TreeLayoutType::Radial => {
            if let Some(transform) = &radial_transform {
                let radial_polygons =
                    painter
                        .radial_highlight_polygons_mapped(tree, layout, |pos| transform.map(pos));

                for (points, color) in radial_polygons {
                    if points.len() < 3 {
                        continue;
                    }

                    let coords = points
                        .iter()
                        .map(|p| format!("{},{}", p.x, p.y))
                        .collect::<Vec<_>>()
                        .join(" ");

                    let polygon = Polygon::new()
                        .set("points", coords)
                        .set("fill", color_to_hex(color))
                        .set("fill-opacity", color_opacity(color))
                        .set("stroke", "none");
                    tree_group = tree_group.add(polygon);

                    highlight_polygons.push(HighlightPolygon {
                        points: points.iter().map(|p| (p.x, p.y)).collect(),
                        color,
                    });
                }
            } else {
                let (group, polys) =
                    add_standard_highlights_pdf(tree_group, tree, layout, painter, &to_svg_coords);
                tree_group = group;
                highlight_polygons.extend(polys);
            }
        }
        _ => {
            let (group, polys) =
                add_standard_highlights_pdf(tree_group, tree, layout, painter, &to_svg_coords);
            tree_group = group;
            highlight_polygons.extend(polys);
        }
    }

    // Draw branches
    if !layout.continuous_branches.is_empty() {
        for branch in &layout.continuous_branches {
            if branch.points.len() >= 2 {
                let mut path_data = String::new();
                for (i, point) in branch.points.iter().enumerate() {
                    let (x, y) = to_svg_coords(*point);
                    if i == 0 {
                        path_data.push_str(&format!("M {} {} ", x, y));
                    } else {
                        path_data.push_str(&format!("L {} {} ", x, y));
                    }
                }

                // Check for branch color override
                let branch_color = painter
                    .branch_color_override(branch.child)
                    .unwrap_or(painter.branch_stroke.color);

                let branch_path = SvgPath::new()
                    .set("d", path_data)
                    .set("fill", "none")
                    .set("stroke", color_to_hex(branch_color))
                    .set("stroke-width", painter.branch_stroke.width);

                tree_group = tree_group.add(branch_path);
            }
        }
    } else {
        for (parent, child) in &layout.edges {
            let parent_pos = to_svg_coords(layout.positions[*parent]);
            let child_pos = to_svg_coords(layout.positions[*child]);

            // Check for branch color override
            let branch_color = painter
                .branch_color_override(*child)
                .unwrap_or(painter.branch_stroke.color);

            let line = Line::new()
                .set("x1", parent_pos.0)
                .set("y1", parent_pos.1)
                .set("x2", child_pos.0)
                .set("y2", child_pos.1)
                .set("stroke", color_to_hex(branch_color))
                .set("stroke-width", painter.branch_stroke.width);

            tree_group = tree_group.add(line);
        }
    }

    // Get external nodes
    let external_nodes: Vec<NodeId> = tree.external_nodes().iter().map(|n| n.id).collect();
    let external_set: HashSet<NodeId> = external_nodes.iter().copied().collect();

    // Draw nodes
    for (node_id, pos) in layout.positions.iter().enumerate() {
        let is_external = external_set.contains(&node_id);

        let should_draw = if is_external {
            painter.show_tip_shapes
        } else {
            painter.show_node_shapes
        };

        if should_draw {
            let (x, y) = to_svg_coords(*pos);

            let color = if is_external {
                painter.leaf_color
            } else {
                painter.internal_node_color
            };

            let circle = Circle::new()
                .set("cx", x)
                .set("cy", y)
                .set("r", painter.node_radius)
                .set("fill", color_to_hex(color));

            tree_group = tree_group.add(circle);
        }
    }

    // Collect text elements instead of adding them to SVG
    // Tip labels
    if painter.show_tip_labels {
        for node_id in &external_nodes {
            if let Some(node) = tree.nodes.get(*node_id) {
                if let Some(name) = &node.name {
                    let pos = layout.positions[*node_id];
                    let (x, y) = to_svg_coords(pos);

                    let label_color = painter
                        .tip_label_color_override(*node_id)
                        .unwrap_or(painter.label_color);

                    // Calculate rotation and position based on layout type
                    let (rotation_angle_deg, text_x, text_y, text_anchor) = match layout.layout_type
                    {
                        TreeLayoutType::Circular => {
                            let angle = calculate_circular_label_angle(node, layout);
                            let angle_deg = angle.to_degrees();
                            let normalized = if angle_deg < 0.0 {
                                angle_deg + 360.0
                            } else {
                                angle_deg
                            };

                            let label_offset = 12.0;
                            let offset_x = label_offset * angle.cos();
                            let offset_y = label_offset * angle.sin();

                            if normalized > 90.0 && normalized < 270.0 {
                                (angle_deg + 180.0, x + offset_x, y + offset_y, "end")
                            } else {
                                (angle_deg, x + offset_x, y + offset_y, "start")
                            }
                        }
                        TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                            // Calculate angle using original layout coordinates for correct angles
                            let angle = if let Some(parent_id) = node.parent {
                                let parent_pos = layout.positions[parent_id];
                                let node_pos = layout.positions[*node_id];
                                let dx = node_pos.0 - parent_pos.0;
                                let dy = node_pos.1 - parent_pos.1;
                                // Use angle in original coordinate system
                                dy.atan2(dx)
                            } else {
                                0.0
                            };

                            let angle_deg = angle.to_degrees();
                            let normalized = if angle_deg < 0.0 {
                                angle_deg + 360.0
                            } else {
                                angle_deg
                            };

                            let label_offset = 8.0;
                            let offset_x = label_offset * angle.cos();
                            let offset_y = label_offset * angle.sin();

                            if normalized > 90.0 && normalized < 270.0 {
                                (angle_deg + 180.0, x + offset_x, y + offset_y, "end")
                            } else {
                                (angle_deg, x + offset_x, y + offset_y, "start")
                            }
                        }
                        _ => (0.0, x + painter.node_radius + 4.0, y, "start"),
                    };

                    text_elements.push(TextElement {
                        text: name.clone(),
                        x: text_x,
                        y: text_y,
                        font_name: "Helvetica".to_string(),
                        font_size: painter.tip_label_font_size,
                        rotation: rotation_angle_deg,
                        color: label_color,
                        text_anchor: text_anchor.to_string(),
                        label_type: LabelType::Tip,
                    });
                }
            }
        }
    }

    // Node labels (internal nodes)
    if painter.show_node_labels {
        for (node_id, pos) in layout.positions.iter().enumerate() {
            let is_external = external_set.contains(&node_id);

            if !is_external {
                if let Some(node) = tree.nodes.get(node_id) {
                    if let Some(name) = &node.name {
                        let (x, y) = to_svg_coords(*pos);

                        text_elements.push(TextElement {
                            text: name.clone(),
                            x: x + 8.0,
                            y,
                            font_name: "Helvetica".to_string(),
                            font_size: 10.0,
                            rotation: 0.0,
                            color: painter.label_color,
                            text_anchor: "start".to_string(),
                            label_type: LabelType::Node,
                        });
                    }
                }
            }
        }
    }

    // Branch labels
    if painter.show_branch_labels {
        for (parent_id, child_id) in &layout.edges {
            if let Some(child_node) = tree.nodes.get(*child_id) {
                if let Some(length) = child_node.length {
                    let parent_pos = to_svg_coords(layout.positions[*parent_id]);
                    let child_pos = to_svg_coords(layout.positions[*child_id]);

                    let (mid_x, mid_y) = match layout.layout_type {
                        TreeLayoutType::Rectangular | TreeLayoutType::Phylogram => {
                            let mid_x = (parent_pos.0 + child_pos.0) * 0.5;
                            let mid_y = child_pos.1 - 12.0;
                            (mid_x, mid_y)
                        }
                        _ => {
                            let mid_x = (parent_pos.0 + child_pos.0) * 0.5;
                            let mid_y = (parent_pos.1 + child_pos.1) * 0.5 - 12.0;
                            (mid_x, mid_y)
                        }
                    };

                    text_elements.push(TextElement {
                        text: format!("{:.3}", length),
                        x: mid_x,
                        y: mid_y,
                        font_name: "Helvetica".to_string(),
                        font_size: 9.0,
                        rotation: 0.0,
                        color: painter.branch_label_color,
                        text_anchor: "middle".to_string(),
                        label_type: LabelType::Branch,
                    });
                }
            }
        }
    }

    document = document.add(tree_group);

    // Return SVG string without text elements
    let svg_string = format!("{}", document);
    (svg_string, text_elements, highlight_polygons)
}

fn add_standard_highlights_pdf(
    mut tree_group: ::svg::node::element::Group,
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    to_svg_coords: &dyn Fn((f32, f32)) -> (f32, f32),
) -> (::svg::node::element::Group, Vec<HighlightPolygon>) {
    let mut polygons = Vec::new();

    for shape in painter.compute_highlight_shapes(tree, layout) {
        match shape {
            crate::tree::painter::HighlightShape::Rect {
                top_left,
                bottom_right,
                color,
            } => {
                let (x1, y1) = to_svg_coords((top_left.x, top_left.y));
                let (x2, y2) = to_svg_coords((bottom_right.x, bottom_right.y));
                let min_x = x1.min(x2);
                let max_x = x1.max(x2);
                let min_y = y1.min(y2);
                let max_y = y1.max(y2);

                if (max_x - min_x).abs() < f32::EPSILON || (max_y - min_y).abs() < f32::EPSILON {
                    continue;
                }

                let rect = ::svg::node::element::Rectangle::new()
                    .set("x", min_x)
                    .set("y", min_y)
                    .set("width", max_x - min_x)
                    .set("height", max_y - min_y)
                    .set("fill", color_to_hex(color))
                    .set("fill-opacity", color_opacity(color))
                    .set("stroke", "none");
                tree_group = tree_group.add(rect);

                polygons.push(HighlightPolygon {
                    points: vec![
                        (min_x, min_y),
                        (max_x, min_y),
                        (max_x, max_y),
                        (min_x, max_y),
                    ],
                    color,
                });
            }
            crate::tree::painter::HighlightShape::Sector {
                center,
                inner_radius,
                outer_radius,
                start_angle,
                end_angle,
                color,
            } => {
                let points = sector_polygon_points(
                    center,
                    inner_radius,
                    outer_radius,
                    start_angle,
                    end_angle,
                    to_svg_coords,
                );

                if points.len() >= 3 {
                    let coords = points
                        .iter()
                        .map(|(x, y)| format!("{},{}", x, y))
                        .collect::<Vec<_>>()
                        .join(" ");

                    let polygon = ::svg::node::element::Polygon::new()
                        .set("points", coords)
                        .set("fill", color_to_hex(color))
                        .set("fill-opacity", color_opacity(color))
                        .set("stroke", "none");
                    tree_group = tree_group.add(polygon);

                    polygons.push(HighlightPolygon { points, color });
                }
            }
            crate::tree::painter::HighlightShape::Polygon { points, color } => {
                if points.len() < 3 {
                    continue;
                }
                let mapped_points: Vec<(f32, f32)> =
                    points.iter().map(|p| to_svg_coords((p.x, p.y))).collect();
                if mapped_points.len() < 3 {
                    continue;
                }

                let coords = mapped_points
                    .iter()
                    .map(|(x, y)| format!("{},{}", x, y))
                    .collect::<Vec<_>>()
                    .join(" ");

                let polygon = ::svg::node::element::Polygon::new()
                    .set("points", coords)
                    .set("fill", color_to_hex(color))
                    .set("fill-opacity", color_opacity(color))
                    .set("stroke", "none");
                tree_group = tree_group.add(polygon);

                polygons.push(HighlightPolygon {
                    points: mapped_points,
                    color,
                });
            }
        }
    }

    (tree_group, polygons)
}

fn sector_polygon_points(
    center: eframe::egui::Pos2,
    inner_radius: f32,
    outer_radius: f32,
    start_angle: f32,
    end_angle: f32,
    to_svg_coords: &dyn Fn((f32, f32)) -> (f32, f32),
) -> Vec<(f32, f32)> {
    if outer_radius <= inner_radius {
        return Vec::new();
    }

    let angle_span = (end_angle - start_angle).abs();
    let steps = ((angle_span / (std::f32::consts::PI / 24.0)).ceil() as usize).max(16);

    let mut outer_points = Vec::with_capacity(steps + 1);
    for i in 0..=steps {
        let t = i as f32 / steps as f32;
        let angle = start_angle + (end_angle - start_angle) * t;
        let world = (
            center.x + outer_radius * angle.cos(),
            center.y + outer_radius * angle.sin(),
        );
        outer_points.push(to_svg_coords(world));
    }

    let mut inner_points = Vec::new();
    if inner_radius > f32::EPSILON {
        for i in (0..=steps).rev() {
            let t = i as f32 / steps as f32;
            let angle = start_angle + (end_angle - start_angle) * t;
            let world = (
                center.x + inner_radius * angle.cos(),
                center.y + inner_radius * angle.sin(),
            );
            inner_points.push(to_svg_coords(world));
        }
    } else {
        inner_points.push(to_svg_coords((center.x, center.y)));
    }

    outer_points
        .into_iter()
        .chain(inner_points.into_iter())
        .collect()
}

fn draw_background(layer: &PdfLayerReference, width_mm: f32, height_mm: f32) {
    let mut polygon = Polygon::default();
    polygon.mode = PaintMode::Fill;
    polygon.rings.push(vec![
        (Point::new(Mm(0.0), Mm(0.0)), false),
        (Point::new(Mm(width_mm), Mm(0.0)), false),
        (Point::new(Mm(width_mm), Mm(height_mm)), false),
        (Point::new(Mm(0.0), Mm(height_mm)), false),
    ]);

    layer.set_fill_color(Color::Rgb(Rgb::new(1.0, 1.0, 1.0, None)));
    layer.add_polygon(polygon);
}

fn draw_highlight_polygons_pdf(
    layer: &PdfLayerReference,
    highlights: &[HighlightPolygon],
    _width: f32,
    height: f32,
) {
    if highlights.is_empty() {
        return;
    }

    for highlight in highlights {
        if highlight.points.len() < 3 {
            continue;
        }

        let [r, g, b, _] = highlight.color.to_srgba_unmultiplied();
        let alpha = color_opacity(highlight.color);
        let inv_alpha = 1.0 - alpha;
        let blended_r = inv_alpha * 255.0 + alpha * r as f32;
        let blended_g = inv_alpha * 255.0 + alpha * g as f32;
        let blended_b = inv_alpha * 255.0 + alpha * b as f32;
        let fill_color = Color::Rgb(Rgb::new(
            blended_r / 255.0,
            blended_g / 255.0,
            blended_b / 255.0,
            None,
        ));
        layer.set_fill_color(fill_color);

        let mut ring: Vec<(Point, bool)> = Vec::with_capacity(highlight.points.len());
        for (x, y) in &highlight.points {
            let x_mm = x * 25.4 / 96.0;
            let y_mm = convert_y(*y, height) * 25.4 / 96.0;
            ring.push((Point::new(Mm(x_mm), Mm(y_mm)), false));
        }

        if ring.len() < 3 {
            continue;
        }

        let mut polygon = Polygon::default();
        polygon.mode = PaintMode::Fill;
        polygon.rings.push(ring);
        layer.add_polygon(polygon);
    }
}

/// Convert Y coordinate from SVG (top-left origin) to PDF (bottom-left origin)
fn convert_y(y: f32, page_height: f32) -> f32 {
    page_height - y
}

/// Convert rotation angle for PDF (flipped Y axis)
fn convert_angle(angle: f32) -> f32 {
    -angle
}

/// Export tree to PDF format using SVG and text separation approach
pub fn export_pdf(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    path: &Path,
    width: f32,
    height: f32,
) -> Result<(), String> {
    // Generate SVG without text and collect text/highlight elements
    let (svg_string, text_elements, highlight_polygons) =
        generate_svg_without_text(tree, layout, painter, width, height);

    // Convert dimensions from pixels to mm (assuming 96 DPI)
    let width_mm = width * 25.4 / 96.0;
    let height_mm = height * 25.4 / 96.0;

    // Create PDF document
    let (doc, page1, layer1) =
        PdfDocument::new("FigTree Export", Mm(width_mm), Mm(height_mm), "Layer 1");

    let current_layer = doc.get_page(page1).get_layer(layer1);

    // Draw background and highlights before vector content
    draw_background(&current_layer, width_mm, height_mm);
    draw_highlight_polygons_pdf(&current_layer, &highlight_polygons, width, height);

    // Parse and add SVG (without text) to PDF
    let pdf_svg = Svg::parse(&svg_string).map_err(|e| format!("Failed to parse SVG: {:?}", e))?;

    let svg_transform = SvgTransform {
        translate_x: None,
        translate_y: None,
        scale_x: None,
        scale_y: None,
        rotate: None,
        dpi: Some(96.0),
    };

    pdf_svg.add_to_layer(&current_layer, svg_transform);

    // Create font cache
    let font_cache = create_font_cache(&doc, &text_elements)?;

    // Render text elements separately
    for text_elem in &text_elements {
        let font = font_cache
            .get(&text_elem.font_name)
            .ok_or_else(|| format!("Font {} not found in cache", text_elem.font_name))?;

        current_layer.begin_text_section();

        // Set font and size
        current_layer.set_font(font, text_elem.font_size);

        // Set text color
        current_layer.set_fill_color(egui_color_to_pdf(text_elem.color));

        // Convert coordinates from SVG to PDF space
        let x_mm = text_elem.x * 25.4 / 96.0;
        let y_mm = convert_y(text_elem.y, height) * 25.4 / 96.0;

        // Convert to points for text rendering
        let x_pt = x_mm * 72.0 / 25.4;
        let y_pt = y_mm * 72.0 / 25.4;

        // Adjust Y position based on label type
        let font_height_pt = text_elem.font_size * 72.0 / 96.0;
        let y_pt_centered = match text_elem.label_type {
            LabelType::Tip => {
                // For tip labels, move down by half the font height
                y_pt - font_height_pt * 0.5
            }
            _ => {
                // For node and branch labels, center vertically
                y_pt + font_height_pt * 0.35
            }
        };

        // Apply text transformation matrix
        if text_elem.rotation.abs() > 0.1 {
            // TextMatrix::TranslateRotate expects degrees, not radians
            // PDF Y-axis is flipped, so negate the angle
            let rotation_deg = -text_elem.rotation;

            // Adjust position based on text anchor for rotated text
            let (adjusted_x_pt, adjusted_y_pt) = if text_elem.text_anchor == "end" {
                // For "end" anchor, we need to shift the text back by its width
                // Calculate text width approximation with better character width estimate
                let char_width = text_elem.font_size * 0.75; // Better approximation for Helvetica
                let text_width_pt = text_elem.text.len() as f32 * char_width * 72.0 / 96.0;

                // Add extra spacing to avoid overlap
                let spacing_buffer = 4.0; // 4 points buffer

                // Use the original angle in radians for position calculation
                let angle_rad = text_elem.rotation.to_radians();
                let cos_angle = angle_rad.cos();
                let sin_angle = angle_rad.sin();

                // Shift the text position back along its rotation axis with buffer
                (
                    x_pt - (text_width_pt + spacing_buffer) * cos_angle,
                    y_pt_centered + (text_width_pt + spacing_buffer) * sin_angle, // Note: + because Y is flipped in PDF
                )
            } else if text_elem.text_anchor == "middle" {
                // For "middle" anchor, shift by half the text width
                let char_width = text_elem.font_size * 0.75; // Better approximation for Helvetica
                let text_width_pt = text_elem.text.len() as f32 * char_width * 72.0 / 96.0;

                let angle_rad = text_elem.rotation.to_radians();
                let cos_angle = angle_rad.cos();
                let sin_angle = angle_rad.sin();

                (
                    x_pt - (text_width_pt * 0.5) * cos_angle,
                    y_pt_centered + (text_width_pt * 0.5) * sin_angle, // Note: + because Y is flipped
                )
            } else {
                // "start" anchor - no horizontal adjustment needed, but apply vertical centering
                (x_pt, y_pt_centered)
            };

            current_layer.set_text_matrix(TextMatrix::TranslateRotate(
                Pt(adjusted_x_pt),
                Pt(adjusted_y_pt),
                rotation_deg,
            ));
        } else {
            // Non-rotated text - apply vertical centering
            current_layer.set_text_matrix(TextMatrix::Translate(Pt(x_pt), Pt(y_pt_centered)));
        }

        // Write text
        current_layer.write_text(&text_elem.text, font);

        current_layer.end_text_section();
    }

    // Save to file
    doc.save(&mut BufWriter::new(
        File::create(path).map_err(|e| format!("Failed to create PDF file: {}", e))?,
    ))
    .map_err(|e| format!("Failed to save PDF: {}", e))
}

fn color_to_hex(color: eframe::egui::Color32) -> String {
    let [r, g, b, _a] = color.to_srgba_unmultiplied();
    format!("#{:02x}{:02x}{:02x}", r, g, b)
}

fn color_opacity(color: eframe::egui::Color32) -> f32 {
    (color.a() as f32) / 255.0
}

fn calculate_tip_extensions(tree: &Tree) -> HashMap<NodeId, f64> {
    fn calculate_distances_from_root(
        node_id: NodeId,
        current_distance: f64,
        tree: &Tree,
        distances: &mut HashMap<NodeId, f64>,
    ) {
        distances.insert(node_id, current_distance);

        let node = &tree.nodes[node_id];
        for &child_id in &node.children {
            let branch_length = tree.nodes[child_id].length.unwrap_or(1.0);
            calculate_distances_from_root(
                child_id,
                current_distance + branch_length,
                tree,
                distances,
            );
        }
    }

    let mut distances = HashMap::new();
    if let Some(root_id) = tree.root {
        calculate_distances_from_root(root_id, 0.0, tree, &mut distances);
    }

    let max_distance = tree
        .nodes
        .iter()
        .filter(|node| node.is_leaf())
        .filter_map(|node| distances.get(&node.id))
        .fold(0.0f64, |acc, &dist| acc.max(dist));

    let mut extensions = HashMap::new();
    for node in &tree.nodes {
        if node.is_leaf() {
            if let Some(&distance) = distances.get(&node.id) {
                let extension = max_distance - distance;
                if extension > 1e-6 {
                    extensions.insert(node.id, extension);
                }
            }
        }
    }

    extensions
}

struct RadialTransform {
    center_x: f32,
    center_y: f32,
    layout_center_x: f32,
    layout_center_y: f32,
    scale: f32,
}

impl RadialTransform {
    fn map(&self, pos: (f32, f32)) -> egui::Pos2 {
        let x = self.center_x + (pos.0 - self.layout_center_x) * self.scale;
        let y = self.center_y + (pos.1 - self.layout_center_y) * self.scale;
        pos2(x, y)
    }
}

fn compute_radial_transform(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    tip_extensions: &std::collections::HashMap<NodeId, f64>,
    margin_x: f32,
    margin_y: f32,
    inner_width: f32,
    inner_height: f32,
) -> Option<RadialTransform> {
    if !matches!(
        layout.layout_type,
        TreeLayoutType::Circular | TreeLayoutType::Radial | TreeLayoutType::Daylight
    ) {
        return None;
    }

    let label_margin = if painter.align_tip_labels && !tip_extensions.is_empty() {
        painter.tip_label_font_size * 8.0
    } else {
        0.0
    };

    let available_radius = (inner_width.min(inner_height) * 0.45 - label_margin).max(1.0);

    let layout_center = if matches!(layout.layout_type, TreeLayoutType::Circular) {
        if let Some(root_id) = tree.root {
            layout.positions[root_id]
        } else {
            (layout.width * 0.5, layout.height * 0.5)
        }
    } else {
        (layout.width * 0.5, layout.height * 0.5)
    };

    let mut max_radius = 0.0f32;
    for node in &tree.nodes {
        let pos = layout.positions[node.id];
        let dx = pos.0 - layout_center.0;
        let dy = pos.1 - layout_center.1;
        let radius = (dx * dx + dy * dy).sqrt();
        max_radius = max_radius.max(radius);
    }

    let mut layout_radius = max_radius;
    if painter.align_tip_labels && !tip_extensions.is_empty() {
        let max_extension = tip_extensions
            .values()
            .fold(0.0f64, |acc, &value| acc.max(value)) as f32;
        layout_radius += max_extension;
    }

    let layout_radius = layout_radius.max(1e-6);
    let scale = available_radius / layout_radius;

    let center_x = margin_x + inner_width * 0.5;
    let center_y = margin_y + inner_height * 0.5;

    Some(RadialTransform {
        center_x,
        center_y,
        layout_center_x: layout_center.0,
        layout_center_y: layout_center.1,
        scale,
    })
}

fn egui_color_to_pdf(color: eframe::egui::Color32) -> Color {
    Color::Rgb(Rgb::new(
        color.r() as f32 / 255.0,
        color.g() as f32 / 255.0,
        color.b() as f32 / 255.0,
        None,
    ))
}

/// Calculate label angle for Circular layout
fn calculate_circular_label_angle(node: &crate::tree::TreeNode, layout: &TreeLayout) -> f32 {
    if let Some(_parent_id) = node.parent {
        // Try to find the continuous branch to get the shoulder point
        if let Some(branch) = layout
            .continuous_branches
            .iter()
            .find(|b| b.child == node.id)
        {
            if branch.points.len() >= 2 {
                let child_pos = branch.points[0]; // Tip node position
                let shoulder_pos = branch.points[1]; // Shoulder (corner) position

                let dx = child_pos.0 - shoulder_pos.0;
                let dy = child_pos.1 - shoulder_pos.1;
                return dy.atan2(dx);
            }
        }

        // Fallback: use parent to child direction
        if let Some(parent_id) = node.parent {
            let parent_pos = layout.positions[parent_id];
            let child_pos = layout.positions[node.id];
            let dx = child_pos.0 - parent_pos.0;
            let dy = child_pos.1 - parent_pos.1;
            dy.atan2(dx)
        } else {
            0.0
        }
    } else {
        // Root node: use direction from layout center to node
        let center_x = layout.width * 0.5;
        let center_y = layout.height * 0.5;
        let node_pos = layout.positions[node.id];
        let dx = node_pos.0 - center_x;
        let dy = node_pos.1 - center_y;
        dy.atan2(dx)
    }
}
