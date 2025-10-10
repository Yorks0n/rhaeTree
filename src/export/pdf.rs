use std::path::Path;
use printpdf::*;

use crate::tree::layout::{TreeLayout, TreeLayoutType};
use crate::tree::painter::TreePainter;
use crate::tree::{NodeId, Tree};

/// Export tree to PDF format
pub fn export_pdf(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    path: &Path,
    width: f32,
    height: f32,
) -> Result<(), String> {
    // Convert dimensions from pixels to mm (assuming 96 DPI)
    let width_mm = width * 25.4 / 96.0;
    let height_mm = height * 25.4 / 96.0;

    // Create PDF document
    let (doc, page1, layer1) = PdfDocument::new(
        "FigTree Export",
        Mm(width_mm),
        Mm(height_mm),
        "Layer 1",
    );

    let current_layer = doc.get_page(page1).get_layer(layer1);

    // Calculate margins
    let margin_x_px = (width * 0.05).max(20.0).min(60.0);
    let margin_y_px = (height * 0.05).max(20.0).min(40.0);

    // Calculate inner dimensions
    let inner_width_px = width - 2.0 * margin_x_px;
    let inner_height_px = height - 2.0 * margin_y_px;

    // Calculate scale factors
    let scale_x = if layout.width <= f32::EPSILON {
        inner_width_px.max(1.0)
    } else {
        inner_width_px.max(1.0) / layout.width
    };

    let scale_y = if layout.height <= f32::EPSILON {
        inner_height_px.max(1.0)
    } else {
        inner_height_px.max(1.0) / layout.height
    };

    // For Radial/Circular/Daylight layouts, calculate the actual layout radius
    // This matches the logic in TreePainter::paint_tree
    let layout_radius_for_radial = if matches!(
        layout.layout_type,
        TreeLayoutType::Circular | TreeLayoutType::Radial | TreeLayoutType::Daylight
    ) {
        // Calculate the maximum radius from center to any leaf node
        let mut max_radius = 0.0f32;
        for node in &tree.nodes {
            if node.is_leaf() {
                let pos = layout.positions[node.id];
                let center_x = layout.width * 0.5;
                let center_y = layout.height * 0.5;
                let dx = pos.0 - center_x;
                let dy = pos.1 - center_y;
                let radius = (dx * dx + dy * dy).sqrt();
                max_radius = max_radius.max(radius);
            }
        }
        max_radius.max(1e-6)
    } else {
        (layout.width.max(layout.height) * 0.5).max(1e-6)
    };

    // Coordinate transformation function (pixels to mm, with Y-axis flip)
    let to_pdf_coords = |pos: (f32, f32)| -> (f32, f32) {
        let (x_px, y_px) = match layout.layout_type {
            TreeLayoutType::Circular | TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                let available_radius = inner_width_px.min(inner_height_px) * 0.45;
                let scale = available_radius / layout_radius_for_radial;

                let center_x = margin_x_px + inner_width_px * 0.5;
                let center_y = margin_y_px + inner_height_px * 0.5;

                let x = center_x + (pos.0 - layout.width * 0.5) * scale;
                let y = center_y + (pos.1 - layout.height * 0.5) * scale;

                (x, y)
            }
            _ => {
                let x = margin_x_px + pos.0 * scale_x;
                let y = margin_y_px + pos.1 * scale_y;
                (x, y)
            }
        };

        // Convert to mm and flip Y axis (PDF origin is bottom-left)
        let x_mm = x_px * 25.4 / 96.0;
        let y_mm = (height - y_px) * 25.4 / 96.0;

        (x_mm, y_mm)
    };

    // Set branch color and width
    let branch_color = egui_color_to_pdf(painter.branch_stroke.color);
    current_layer.set_outline_color(branch_color);
    current_layer.set_outline_thickness(painter.branch_stroke.width);

    // Draw branches
    if !layout.continuous_branches.is_empty() {
        for branch in &layout.continuous_branches {
            if branch.points.len() >= 2 {
                let mut points = Vec::new();
                for point in &branch.points {
                    let (x, y) = to_pdf_coords(*point);
                    points.push((Point::new(Mm(x), Mm(y)), false));
                }

                let line = Line {
                    points,
                    is_closed: false,
                };
                current_layer.add_line(line);
            }
        }
    } else {
        for (parent, child) in &layout.edges {
            let (x1, y1) = to_pdf_coords(layout.positions[*parent]);
            let (x2, y2) = to_pdf_coords(layout.positions[*child]);

            let line = Line {
                points: vec![
                    (Point::new(Mm(x1), Mm(y1)), false),
                    (Point::new(Mm(x2), Mm(y2)), false),
                ],
                is_closed: false,
            };
            current_layer.add_line(line);
        }
    }

    // Get external nodes for labels
    let external_nodes: Vec<NodeId> = tree.external_nodes().iter().map(|n| n.id).collect();

    // Draw tip labels
    if painter.show_tip_labels {
        // Load a built-in font
        let font = doc.add_builtin_font(BuiltinFont::Helvetica)
            .map_err(|e| format!("Failed to load font: {}", e))?;

        for node_id in &external_nodes {
            if let Some(node) = tree.nodes.get(*node_id) {
                if let Some(name) = &node.name {
                    let (x, y) = to_pdf_coords(layout.positions[*node_id]);

                    let label_color = painter.tip_label_color_override(*node_id)
                        .unwrap_or(painter.label_color);

                    current_layer.set_fill_color(egui_color_to_pdf(label_color));

                    // Calculate rotation angle based on layout type (same as SVG)
                    let (rotation_angle_deg, text_x, text_y) = match layout.layout_type {
                        TreeLayoutType::Circular => {
                            let angle = calculate_circular_label_angle(node, layout);
                            let angle_deg = angle.to_degrees();
                            let normalized = if angle_deg < 0.0 { angle_deg + 360.0 } else { angle_deg };

                            let label_offset = 12.0;
                            let node_pos_world = layout.positions[*node_id];
                            let offset_x = label_offset * angle.cos();
                            let offset_y = label_offset * angle.sin();

                            let label_pos_world = (node_pos_world.0 + offset_x, node_pos_world.1 + offset_y);
                            let (label_x, label_y) = to_pdf_coords(label_pos_world);

                            if normalized > 90.0 && normalized < 270.0 {
                                (angle_deg + 180.0, label_x, label_y)
                            } else {
                                (angle_deg, label_x, label_y)
                            }
                        }
                        TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                            let angle = if let Some(parent_id) = node.parent {
                                let parent_pos = layout.positions[parent_id];
                                let node_pos = layout.positions[*node_id];
                                let dx = node_pos.0 - parent_pos.0;
                                let dy = node_pos.1 - parent_pos.1;
                                dy.atan2(dx)
                            } else {
                                0.0
                            };

                            let angle_deg = angle.to_degrees();
                            let normalized = if angle_deg < 0.0 { angle_deg + 360.0 } else { angle_deg };

                            let label_offset = 8.0;
                            let node_pos_world = layout.positions[*node_id];
                            let offset_x = label_offset * angle.cos();
                            let offset_y = label_offset * angle.sin();

                            let label_pos_world = (node_pos_world.0 + offset_x, node_pos_world.1 + offset_y);
                            let (label_x, label_y) = to_pdf_coords(label_pos_world);

                            if normalized > 90.0 && normalized < 270.0 {
                                (angle_deg + 180.0, label_x, label_y)
                            } else {
                                (angle_deg, label_x, label_y)
                            }
                        }
                        _ => {
                            let offset_mm = (painter.node_radius + 4.0) * 25.4 / 96.0;
                            (0.0, x + offset_mm, y)
                        }
                    };

                    // Apply rotation using CTM if needed
                    if rotation_angle_deg.abs() > 0.1 {
                        // Save graphics state before transformation
                        current_layer.save_graphics_state();

                        // Translate to text position, rotate, then place text at origin
                        // PDF rotation is counterclockwise, but our angles are already correct
                        // Note: PDF Y-axis is bottom-up, so rotation direction matches
                        current_layer.set_ctm(CurTransMat::TranslateRotate(
                            Pt(text_x * 72.0 / 25.4),  // Convert mm to points
                            Pt(text_y * 72.0 / 25.4),  // Convert mm to points
                            -rotation_angle_deg,        // Negative for PDF coordinate system
                        ));

                        // Place text at origin (already transformed by CTM)
                        current_layer.use_text(
                            name,
                            painter.tip_label_font_size,
                            Mm(0.0),
                            Mm(0.0),
                            &font,
                        );

                        // Restore graphics state
                        current_layer.restore_graphics_state();
                    } else {
                        // No rotation needed
                        current_layer.use_text(
                            name,
                            painter.tip_label_font_size,
                            Mm(text_x),
                            Mm(text_y),
                            &font,
                        );
                    }
                }
            }
        }
    }

    // Save to file
    doc.save(&mut std::io::BufWriter::new(
        std::fs::File::create(path)
            .map_err(|e| format!("Failed to create PDF file: {}", e))?
    ))
    .map_err(|e| format!("Failed to save PDF: {}", e))
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
/// This matches the logic in TreePainter::paint_circular_tip_label
fn calculate_circular_label_angle(node: &crate::tree::TreeNode, layout: &TreeLayout) -> f32 {
    if let Some(parent_id) = node.parent {
        // Try to find the continuous branch to get the shoulder point
        if let Some(branch) = layout.continuous_branches.iter().find(|b| b.child == node.id) {
            if branch.points.len() >= 2 {
                let child_pos = branch.points[0];  // Tip node position
                let shoulder_pos = branch.points[1];  // Shoulder (corner) position

                let dx = child_pos.0 - shoulder_pos.0;
                let dy = child_pos.1 - shoulder_pos.1;
                return dy.atan2(dx);
            }
        }

        // Fallback: use parent to child direction
        let parent_pos = layout.positions[parent_id];
        let child_pos = layout.positions[node.id];
        let dx = child_pos.0 - parent_pos.0;
        let dy = child_pos.1 - parent_pos.1;
        dy.atan2(dx)
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
