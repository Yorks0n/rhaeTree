use std::collections::HashSet;
use std::path::Path;
use svg::node::element::{Circle, Group, Line, Path as SvgPath, Rectangle, Text};
use svg::Document;

use crate::tree::layout::{TreeLayout, TreeLayoutType};
use crate::tree::painter::TreePainter;
use crate::tree::{NodeId, Tree};

/// Calculate tip extensions for align_tip_labels
fn calculate_tip_extensions(tree: &Tree) -> std::collections::HashMap<NodeId, f64> {
    let mut distances = std::collections::HashMap::new();

    // Calculate distance from root to each node
    fn calculate_distances_from_root(
        node_id: NodeId,
        current_distance: f64,
        tree: &Tree,
        distances: &mut std::collections::HashMap<NodeId, f64>,
    ) {
        distances.insert(node_id, current_distance);

        let node = &tree.nodes[node_id];
        for &child_id in &node.children {
            let branch_length = tree.nodes[child_id].length.unwrap_or(1.0);
            calculate_distances_from_root(child_id, current_distance + branch_length, tree, distances);
        }
    }

    // Start from root
    if let Some(root_id) = tree.root {
        calculate_distances_from_root(root_id, 0.0, tree, &mut distances);
    }

    // Find maximum distance to any tip
    let max_distance = tree
        .nodes
        .iter()
        .filter(|node| node.is_leaf())
        .filter_map(|node| distances.get(&node.id))
        .fold(0.0f64, |a, &b| a.max(b));

    // Calculate extension needed for each tip
    let mut extensions = std::collections::HashMap::new();
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

/// Calculate a nice tick span for scale bar
fn nice_tick_span(total: f32) -> Option<f32> {
    if total <= f32::EPSILON {
        return None;
    }

    let magnitude = 10.0f32.powf((total.log10()).floor());
    let normalized = total / magnitude;

    let nice = if normalized < 2.0 {
        0.5
    } else if normalized < 5.0 {
        1.0
    } else {
        2.0
    };

    Some(nice * magnitude)
}

/// Export tree to SVG format
pub fn export_svg(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    path: &Path,
    width: f32,
    height: f32,
) -> Result<(), String> {
    // Calculate tip extensions for align_tip_labels
    let tip_extensions = if painter.align_tip_labels {
        calculate_tip_extensions(tree)
    } else {
        std::collections::HashMap::new()
    };

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

    // Coordinate transformation function
    let to_svg_coords = |pos: (f32, f32)| -> (f32, f32) {
        match layout.layout_type {
            TreeLayoutType::Circular | TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                let available_radius = inner_width.min(inner_height) * 0.45;
                let scale = available_radius / layout_radius_for_radial;

                let center_x = margin_x + inner_width * 0.5;
                let center_y = margin_y + inner_height * 0.5;

                let x = center_x + (pos.0 - layout.width * 0.5) * scale;
                let y = center_y + (pos.1 - layout.height * 0.5) * scale;

                (x, y)
            }
            _ => {
                let x = margin_x + pos.0 * scale_x;
                let y = margin_y + pos.1 * scale_y;
                (x, y)
            }
        }
    };

    // Create SVG document
    let mut document = Document::new()
        .set("width", width)
        .set("height", height)
        .set("viewBox", (0, 0, width as i32, height as i32));

    // Add white background
    let background = Rectangle::new()
        .set("width", "100%")
        .set("height", "100%")
        .set("fill", "white");
    document = document.add(background);

    // Create group for tree elements
    let mut tree_group = Group::new().set("id", "tree");

    // Draw branches
    if !layout.continuous_branches.is_empty() {
        // Use continuous branches for better rendering
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
                let branch_color = painter.branch_color_override(branch.child)
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
        // Fallback to edges
        for (parent, child) in &layout.edges {
            let parent_pos = to_svg_coords(layout.positions[*parent]);
            let child_pos = to_svg_coords(layout.positions[*child]);

            // Check for branch color override
            let branch_color = painter.branch_color_override(*child)
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

    // Prepare external nodes list
    let external_nodes: Vec<NodeId> = tree.external_nodes().iter().map(|n| n.id).collect();
    let external_set: HashSet<NodeId> = external_nodes.iter().copied().collect();

    // Draw tip extension lines (dashed) BEFORE nodes so they don't cover the shapes
    if painter.align_tip_labels && !tip_extensions.is_empty() {
        for (tip_id, &extension) in &tip_extensions {
            if extension < 1e-6 {
                continue;
            }

            let tip_pos = layout.positions[*tip_id];
            let (tip_x, tip_y) = to_svg_coords(tip_pos);

            // Calculate extended position based on layout type
            let (ext_x, ext_y) = match layout.layout_type {
                TreeLayoutType::Rectangular
                | TreeLayoutType::Slanted
                | TreeLayoutType::Cladogram
                | TreeLayoutType::Phylogram => {
                    // For rectangular layouts, extend horizontally
                    let extended_pos = (tip_pos.0 + extension as f32, tip_pos.1);
                    to_svg_coords(extended_pos)
                }
                TreeLayoutType::Circular => {
                    // For circular layout, extend along branch direction
                    if let Some(branch) = layout.continuous_branches.iter().find(|b| b.child == *tip_id) {
                        if branch.points.len() >= 2 {
                            let child_pos = branch.points[0];
                            let shoulder_pos = branch.points[1];
                            let dx = child_pos.0 - shoulder_pos.0;
                            let dy = child_pos.1 - shoulder_pos.1;
                            let len = (dx * dx + dy * dy).sqrt().max(1e-6);
                            let dir_x = dx / len;
                            let dir_y = dy / len;
                            let extended_pos = (
                                tip_pos.0 + dir_x * extension as f32,
                                tip_pos.1 + dir_y * extension as f32,
                            );
                            to_svg_coords(extended_pos)
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                    // For radial layouts, extend along radial direction
                    if let Some(parent_id) = tree.nodes[*tip_id].parent {
                        let parent_pos = layout.positions[parent_id];
                        let dx = tip_pos.0 - parent_pos.0;
                        let dy = tip_pos.1 - parent_pos.1;
                        let len = (dx * dx + dy * dy).sqrt().max(1e-6);
                        let dir_x = dx / len;
                        let dir_y = dy / len;
                        let extended_pos = (
                            tip_pos.0 + dir_x * extension as f32,
                            tip_pos.1 + dir_y * extension as f32,
                        );
                        to_svg_coords(extended_pos)
                    } else {
                        continue;
                    }
                }
            };

            // Draw dashed line
            let dashed_line = Line::new()
                .set("x1", tip_x)
                .set("y1", tip_y)
                .set("x2", ext_x)
                .set("y2", ext_y)
                .set("stroke", "#969696")
                .set("stroke-width", painter.branch_stroke.width * 0.8)
                .set("stroke-dasharray", "5,3");

            tree_group = tree_group.add(dashed_line);
        }
    }

    // Draw nodes (only if enabled in settings)

    for (node_id, pos) in layout.positions.iter().enumerate() {
        let is_external = external_set.contains(&node_id);

        // Check if we should draw this node based on painter settings
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

    // Draw node labels (internal nodes)
    if painter.show_node_labels {
        for (node_id, pos) in layout.positions.iter().enumerate() {
            let is_external = external_set.contains(&node_id);

            // Only draw labels for internal nodes
            if !is_external {
                if let Some(node) = tree.nodes.get(node_id) {
                    if let Some(name) = &node.name {
                        let (x, y) = to_svg_coords(*pos);

                        let text_content = svg::node::Text::new(name.clone());
                        let text = Text::new("")
                            .set("x", x + 8.0)
                            .set("y", y)
                            .set("font-size", 10.0)
                            .set("fill", color_to_hex(painter.label_color))
                            .set("dominant-baseline", "middle")
                            .set("text-anchor", "start")
                            .add(text_content);

                        tree_group = tree_group.add(text);
                    }
                }
            }
        }
    }

    // Draw branch labels
    if painter.show_branch_labels {
        for (parent_id, child_id) in &layout.edges {
            if let Some(child_node) = tree.nodes.get(*child_id) {
                if let Some(length) = child_node.length {
                    let parent_pos = to_svg_coords(layout.positions[*parent_id]);
                    let child_pos = to_svg_coords(layout.positions[*child_id]);

                    // Calculate midpoint position
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

                    let label_text = format!("{:.3}", length);
                    let text_content = svg::node::Text::new(label_text);
                    let text = Text::new("")
                        .set("x", mid_x)
                        .set("y", mid_y)
                        .set("font-size", 9.0)
                        .set("fill", color_to_hex(painter.branch_label_color))
                        .set("dominant-baseline", "auto")
                        .set("text-anchor", "middle")
                        .add(text_content);

                    tree_group = tree_group.add(text);
                }
            }
        }
    }

    // Draw tip labels
    if painter.show_tip_labels {
        for node_id in &external_nodes {
            if let Some(node) = tree.nodes.get(*node_id) {
                if let Some(name) = &node.name {
                    // Calculate label position (extended if align_tip_labels is enabled)
                    let tip_pos = layout.positions[*node_id];
                    let label_pos_world = if painter.align_tip_labels {
                        if let Some(&extension) = tip_extensions.get(node_id) {
                            match layout.layout_type {
                                TreeLayoutType::Rectangular
                                | TreeLayoutType::Slanted
                                | TreeLayoutType::Cladogram
                                | TreeLayoutType::Phylogram => {
                                    (tip_pos.0 + extension as f32, tip_pos.1)
                                }
                                TreeLayoutType::Circular => {
                                    if let Some(branch) = layout.continuous_branches.iter().find(|b| b.child == *node_id) {
                                        if branch.points.len() >= 2 {
                                            let child_pos = branch.points[0];
                                            let shoulder_pos = branch.points[1];
                                            let dx = child_pos.0 - shoulder_pos.0;
                                            let dy = child_pos.1 - shoulder_pos.1;
                                            let len = (dx * dx + dy * dy).sqrt().max(1e-6);
                                            (tip_pos.0 + (dx / len) * extension as f32,
                                             tip_pos.1 + (dy / len) * extension as f32)
                                        } else {
                                            tip_pos
                                        }
                                    } else {
                                        tip_pos
                                    }
                                }
                                TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                                    if let Some(parent_id) = node.parent {
                                        let parent_pos = layout.positions[parent_id];
                                        let dx = tip_pos.0 - parent_pos.0;
                                        let dy = tip_pos.1 - parent_pos.1;
                                        let len = (dx * dx + dy * dy).sqrt().max(1e-6);
                                        (tip_pos.0 + (dx / len) * extension as f32,
                                         tip_pos.1 + (dy / len) * extension as f32)
                                    } else {
                                        tip_pos
                                    }
                                }
                            }
                        } else {
                            tip_pos
                        }
                    } else {
                        tip_pos
                    };

                    let (x, y) = to_svg_coords(label_pos_world);

                    let label_color = painter.tip_label_color_override(*node_id)
                        .unwrap_or(painter.label_color);

                    // Calculate rotation angle based on layout type
                    let (rotation_angle_deg, text_x, text_y, text_anchor) = match layout.layout_type {
                        TreeLayoutType::Circular => {
                            // Use the same logic as TreePainter::paint_circular_tip_label
                            let angle = calculate_circular_label_angle(node, layout);
                            let angle_deg = angle.to_degrees();
                            let normalized = if angle_deg < 0.0 { angle_deg + 360.0 } else { angle_deg };

                            let label_offset = 12.0;
                            let offset_x = label_offset * angle.cos();
                            let offset_y = label_offset * angle.sin();

                            if normalized > 90.0 && normalized < 270.0 {
                                // Flip text for readability
                                (angle_deg + 180.0, x + offset_x, y + offset_y, "end")
                            } else {
                                (angle_deg, x + offset_x, y + offset_y, "start")
                            }
                        }
                        TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                            // Use the same logic as TreePainter::paint_radial_tip_label
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
                            let offset_x = label_offset * angle.cos();
                            let offset_y = label_offset * angle.sin();

                            if normalized > 90.0 && normalized < 270.0 {
                                (angle_deg + 180.0, x + offset_x, y + offset_y, "end")
                            } else {
                                (angle_deg, x + offset_x, y + offset_y, "start")
                            }
                        }
                        _ => {
                            // No rotation for other layouts
                            (0.0, x + painter.node_radius + 4.0, y, "start")
                        }
                    };

                    let text_content = svg::node::Text::new(name.clone());
                    let mut text = Text::new("")
                        .set("x", text_x)
                        .set("y", text_y)
                        .set("font-size", painter.tip_label_font_size)
                        .set("fill", color_to_hex(label_color))
                        .set("dominant-baseline", "middle")
                        .set("text-anchor", text_anchor);

                    // Add rotation if needed
                    if rotation_angle_deg.abs() > 0.1 {
                        text = text.set("transform", format!("rotate({} {} {})", rotation_angle_deg, text_x, text_y));
                    }

                    text = text.add(text_content);
                    tree_group = tree_group.add(text);
                }
            }
        }
    }

    document = document.add(tree_group);

    // Draw scale bar if enabled
    if painter.show_scale_bar && layout.width > f32::EPSILON {
        if let Some(tick) = nice_tick_span(layout.width) {
            let bar_pixels = tick * scale_x;
            let baseline_y = height - margin_y + 10.0;
            let start_x = margin_x + 20.0;
            let end_x = start_x + bar_pixels;

            // Create scale bar group
            let mut scale_bar_group = Group::new().set("id", "scale_bar");

            // Horizontal line
            let scale_line = Line::new()
                .set("x1", start_x)
                .set("y1", baseline_y)
                .set("x2", end_x)
                .set("y2", baseline_y)
                .set("stroke", "#d2d7dc")
                .set("stroke-width", 2.0);
            scale_bar_group = scale_bar_group.add(scale_line);

            // Left vertical tick
            let left_tick = Line::new()
                .set("x1", start_x)
                .set("y1", baseline_y - 8.0)
                .set("x2", start_x)
                .set("y2", baseline_y)
                .set("stroke", "#d2d7dc")
                .set("stroke-width", 2.0);
            scale_bar_group = scale_bar_group.add(left_tick);

            // Right vertical tick
            let right_tick = Line::new()
                .set("x1", end_x)
                .set("y1", baseline_y - 8.0)
                .set("x2", end_x)
                .set("y2", baseline_y)
                .set("stroke", "#d2d7dc")
                .set("stroke-width", 2.0);
            scale_bar_group = scale_bar_group.add(right_tick);

            // Scale text
            let scale_text_content = svg::node::Text::new(format!("Scale: {:.3}", tick));
            let scale_text = Text::new("")
                .set("x", (start_x + end_x) * 0.5)
                .set("y", baseline_y - 12.0)
                .set("font-size", 11.0)
                .set("fill", "#dce1e6")
                .set("text-anchor", "middle")
                .set("dominant-baseline", "auto")
                .add(scale_text_content);
            scale_bar_group = scale_bar_group.add(scale_text);

            document = document.add(scale_bar_group);
        }
    }

    // Write to file
    svg::save(path, &document)
        .map_err(|e| format!("Failed to save SVG: {}", e))
}

fn color_to_hex(color: eframe::egui::Color32) -> String {
    format!("#{:02x}{:02x}{:02x}", color.r(), color.g(), color.b())
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
