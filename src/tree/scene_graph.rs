use std::collections::{HashMap, HashSet};

use eframe::egui::{self, Color32, Pos2, Rect, Vec2};

use crate::tree::layout::{TreeLayout, TreeLayoutType};
use crate::tree::painter::{HighlightShape, ShapeColorMode, ShapeSizeMode, ShapeType, TipLabelHit, TreePainter};
use crate::tree::viewer::SelectionMode;
use crate::tree::{NodeId, Tree, TreeNode};

#[derive(Clone, Copy)]
pub struct StrokeStyle {
    pub width: f32,
    pub color: Color32,
    pub dash: Option<(f32, f32)>,
}

#[derive(Clone)]
pub enum ScenePrimitive {
    FillRect {
        rect: Rect,
        color: Color32,
    },
    FillCircle {
        center: Pos2,
        radius: f32,
        color: Color32,
    },
    FillPolygon {
        points: Vec<Pos2>,
        color: Color32,
    },
    FillSector {
        center: Pos2,
        inner_radius: f32,
        outer_radius: f32,
        start_angle: f32,
        end_angle: f32,
        color: Color32,
    },
    StrokeLine {
        from: Pos2,
        to: Pos2,
        style: StrokeStyle,
    },
    StrokePolyline {
        points: Vec<Pos2>,
        style: StrokeStyle,
    },
    StrokeCircularBranch {
        child: Pos2,
        center: Pos2,
        radius: f32,
        start_angle: f32,
        end_angle: f32,
        style: StrokeStyle,
    },
    Text {
        text: String,
        anchor: Pos2,
        angle: f32,
        align: egui::Align2,
        size: f32,
        color: Color32,
    },
}

pub struct TreeSceneGraph {
    pub size: Vec2,
    pub primitives: Vec<ScenePrimitive>,
    pub tip_label_hits: Vec<TipLabelHit>,
}

#[allow(clippy::too_many_arguments)]
pub fn build_tree_scene(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    selected_nodes: &HashSet<NodeId>,
    selected_tips: &HashSet<NodeId>,
    rect: Rect,
    canvas_inner: Rect,
    transform_inner: Rect,
    stroke_scale: f32,
    selection_mode: Option<SelectionMode>,
) -> TreeSceneGraph {
    let to_screen = painter.create_to_screen_transform(tree, layout, transform_inner);
    let to_local = |p: Pos2| Pos2::new(p.x - rect.left(), p.y - rect.top());
    let mut primitives = Vec::new();

    primitives.push(ScenePrimitive::FillRect {
        rect: Rect::from_min_size(Pos2::ZERO, rect.size()),
        color: painter.background_color,
    });
    primitives.push(ScenePrimitive::FillRect {
        rect: Rect::from_min_max(to_local(canvas_inner.min), to_local(canvas_inner.max)),
        color: painter.canvas_color,
    });

    for shape in painter.compute_highlight_shapes(tree, layout) {
        match shape {
            HighlightShape::Rect {
                top_left,
                bottom_right,
                color,
            } => {
                let a = to_local(to_screen((top_left.x, top_left.y)));
                let b = to_local(to_screen((bottom_right.x, bottom_right.y)));
                primitives.push(ScenePrimitive::FillRect {
                    rect: Rect::from_two_pos(a, b),
                    color,
                });
            }
            HighlightShape::Polygon { points, color } => {
                let mapped: Vec<Pos2> = points
                    .into_iter()
                    .map(|p| to_local(to_screen((p.x, p.y))))
                    .collect();
                primitives.push(ScenePrimitive::FillPolygon {
                    points: mapped,
                    color,
                });
            }
            HighlightShape::Sector {
                center,
                inner_radius,
                outer_radius,
                start_angle,
                end_angle,
                color,
            } => {
                let center_w = (center.x, center.y);
                let c = to_local(to_screen(center_w));
                // Radii are in layout space; convert to current screen space so
                // circular highlights match tree geometry after zoom/fit transforms.
                let inner_r = (to_local(to_screen((center_w.0 + inner_radius, center_w.1))) - c)
                    .length();
                let outer_r = (to_local(to_screen((center_w.0 + outer_radius, center_w.1))) - c)
                    .length();
                primitives.push(ScenePrimitive::FillSector {
                    center: c,
                    inner_radius: inner_r,
                    outer_radius: outer_r,
                    start_angle,
                    end_angle,
                    color,
                });
            }
        }
    }

    let branch_color = painter.branch_stroke.color;
    let selected_branch_color = painter.tip_selection_color;
    let branch_width = (painter.branch_stroke.width * stroke_scale).max(1.0);
    let highlight_width = (painter.branch_highlight_stroke.width * stroke_scale).max(branch_width * 1.8);

    if matches!(layout.layout_type, TreeLayoutType::Circular) && !layout.arc_segments.is_empty() {
        for (parent, child) in &layout.edges {
            let is_selected = selected_nodes.contains(child)
                && !matches!(selection_mode, Some(SelectionMode::Taxa));
            let base_color = painter
                .branch_color_override(*child)
                .unwrap_or(branch_color);
            if let Some(arc) = layout
                .arc_segments
                .iter()
                .find(|a| a.child == *child && a.parent == *parent)
            {
                if is_selected {
                    primitives.push(ScenePrimitive::StrokeCircularBranch {
                        child: to_local(to_screen(layout.positions[*child])),
                        center: to_local(to_screen(arc.center)),
                        radius: (to_local(to_screen((arc.center.0 + arc.radius, arc.center.1)))
                            - to_local(to_screen(arc.center)))
                        .length(),
                        start_angle: arc.start_angle,
                        end_angle: arc.end_angle,
                        style: StrokeStyle {
                            width: highlight_width,
                            color: selected_branch_color,
                            dash: None,
                        },
                    });
                }
                primitives.push(ScenePrimitive::StrokeCircularBranch {
                    child: to_local(to_screen(layout.positions[*child])),
                    center: to_local(to_screen(arc.center)),
                    radius: (to_local(to_screen((arc.center.0 + arc.radius, arc.center.1)))
                        - to_local(to_screen(arc.center)))
                    .length(),
                    start_angle: arc.start_angle,
                    end_angle: arc.end_angle,
                    style: StrokeStyle {
                        width: branch_width,
                        color: base_color,
                        dash: None,
                    },
                });
            } else {
                if is_selected {
                    primitives.push(ScenePrimitive::StrokeLine {
                        from: to_local(to_screen(layout.positions[*parent])),
                        to: to_local(to_screen(layout.positions[*child])),
                        style: StrokeStyle {
                            width: highlight_width,
                            color: selected_branch_color,
                            dash: None,
                        },
                    });
                }
                primitives.push(ScenePrimitive::StrokeLine {
                    from: to_local(to_screen(layout.positions[*parent])),
                    to: to_local(to_screen(layout.positions[*child])),
                    style: StrokeStyle {
                        width: branch_width,
                        color: base_color,
                        dash: None,
                    },
                });
            }
        }
    } else if !layout.continuous_branches.is_empty() {
        for branch in &layout.continuous_branches {
            let is_selected = selected_nodes.contains(&branch.child)
                && !matches!(selection_mode, Some(SelectionMode::Taxa));
            let base_color = painter
                .branch_color_override(branch.child)
                .unwrap_or(branch_color);
            let points: Vec<Pos2> = branch
                .points
                .iter()
                .map(|&p| to_local(to_screen(p)))
                .collect();
            if is_selected {
                primitives.push(ScenePrimitive::StrokePolyline {
                    points: points.clone(),
                    style: StrokeStyle {
                        width: highlight_width,
                        color: selected_branch_color,
                        dash: None,
                    },
                });
            }
            primitives.push(ScenePrimitive::StrokePolyline {
                points,
                style: StrokeStyle {
                    width: branch_width,
                    color: base_color,
                    dash: None,
                },
            });
        }
    } else {
        for (parent, child) in &layout.edges {
            let is_selected = selected_nodes.contains(child)
                && !matches!(selection_mode, Some(SelectionMode::Taxa));
            let base_color = painter
                .branch_color_override(*child)
                .unwrap_or(branch_color);
            if is_selected {
                primitives.push(ScenePrimitive::StrokeLine {
                    from: to_local(to_screen(layout.positions[*parent])),
                    to: to_local(to_screen(layout.positions[*child])),
                    style: StrokeStyle {
                        width: highlight_width,
                        color: selected_branch_color,
                        dash: None,
                    },
                });
            }
            primitives.push(ScenePrimitive::StrokeLine {
                from: to_local(to_screen(layout.positions[*parent])),
                to: to_local(to_screen(layout.positions[*child])),
                style: StrokeStyle {
                    width: branch_width,
                    color: base_color,
                    dash: None,
                },
            });
        }
    }

    if painter.show_node_bars {
        if let Some(field) = painter.node_bar_field() {
            match layout.layout_type {
                TreeLayoutType::Rectangular
                | TreeLayoutType::Phylogram
                | TreeLayoutType::Slanted
                | TreeLayoutType::Daylight => {
                    for node in &tree.nodes {
                        let Some((mut min, mut max)) = node.numeric_range_attribute(field) else {
                            continue;
                        };
                        if min > max {
                            std::mem::swap(&mut min, &mut max);
                        }
                        let y = layout.positions[node.id].1;
                        // Keep bar midpoint anchored to the corresponding node position.
                        let node_x = layout.positions[node.id].0;
                        let half_width = ((max - min).abs() as f32) * 0.5;
                        let x0 = node_x - half_width;
                        let x1 = node_x + half_width;
                        let p0 = to_local(to_screen((x0, y)));
                        let p1 = to_local(to_screen((x1, y)));
                        let left = p0.x.min(p1.x);
                        let right = p0.x.max(p1.x).max(left + 1.0);
                        let yc = (p0.y + p1.y) * 0.5;
                        let half = painter.node_bar_thickness().max(0.5) * 0.5;
                        primitives.push(ScenePrimitive::FillRect {
                            rect: Rect::from_min_max(
                                Pos2::new(left, yc - half),
                                Pos2::new(right, yc + half),
                            ),
                            color: painter.node_bar_color(),
                        });
                    }
                }
                TreeLayoutType::Circular => {
                    let center_world = tree
                        .root
                        .map(|root_id| layout.positions[root_id])
                        .unwrap_or((layout.width * 0.5, layout.height * 0.5));
                    for node in &tree.nodes {
                        let Some((mut min, mut max)) = node.numeric_range_attribute(field) else {
                            continue;
                        };
                        if !min.is_finite() || !max.is_finite() {
                            continue;
                        }
                        if min > max {
                            std::mem::swap(&mut min, &mut max);
                        }

                        let node_pos = layout.positions[node.id];
                        let mut dir_x = node_pos.0 - center_world.0;
                        let mut dir_y = node_pos.1 - center_world.1;
                        let len = (dir_x * dir_x + dir_y * dir_y).sqrt();
                        if len <= 1e-6 {
                            if let Some(parent_id) = node.parent {
                                let parent = layout.positions[parent_id];
                                dir_x = node_pos.0 - parent.0;
                                dir_y = node_pos.1 - parent.1;
                            } else {
                                dir_x = 1.0;
                                dir_y = 0.0;
                            }
                        }
                        let norm = (dir_x * dir_x + dir_y * dir_y).sqrt().max(1e-6);
                        let ux = dir_x / norm;
                        let uy = dir_y / norm;

                        let node_radius =
                            ((node_pos.0 - center_world.0).powi(2)
                                + (node_pos.1 - center_world.1).powi(2))
                            .sqrt();
                        let half_width = ((max - min).abs() as f32) * 0.5;
                        let r0 = (node_radius - half_width).max(0.0);
                        let r1 = (node_radius + half_width).max(0.0);

                        let start = to_local(to_screen((
                            center_world.0 + ux * r0,
                            center_world.1 + uy * r0,
                        )));
                        let end = to_local(to_screen((
                            center_world.0 + ux * r1,
                            center_world.1 + uy * r1,
                        )));
                        primitives.push(ScenePrimitive::StrokeLine {
                            from: start,
                            to: end,
                            style: StrokeStyle {
                                width: painter.node_bar_thickness().max(0.5),
                                color: painter.node_bar_color(),
                                dash: None,
                            },
                        });
                    }
                }
                _ => {}
            }
        }
    }

    let tip_extensions = if painter.align_tip_labels {
        calculate_tip_extensions(tree)
    } else {
        HashMap::new()
    };
    let circular_tip_align_data = if painter.align_tip_labels
        && matches!(layout.layout_type, TreeLayoutType::Circular)
    {
        let center_world = layout
            .arc_segments
            .first()
            .map(|arc| arc.center)
            .or_else(|| tree.root.map(|root| layout.positions[root]))
            .unwrap_or((layout.width * 0.5, layout.height * 0.5));
        let center = to_local(to_screen(center_world));
        let mut radii = Vec::new();
        let mut max_radius = 0.0f32;
        for node in tree.nodes.iter().filter(|n| n.is_leaf()) {
            let p = to_local(to_screen(layout.positions[node.id]));
            let v = p - center;
            let r = v.length();
            max_radius = max_radius.max(r);
            let dir = if r > 1e-6 {
                v / r
            } else {
                let angle = compute_circular_tip_angle_screen(tree, layout, node, &to_screen);
                Vec2::new(angle.cos(), angle.sin())
            };
            radii.push((node.id, r, dir));
        }
        let mut data = HashMap::new();
        for (node_id, r, dir) in radii {
            let ext = (max_radius - r).max(0.0);
            if ext > 0.5 {
                data.insert(node_id, (ext, dir));
            }
        }
        data
    } else {
        HashMap::new()
    };
    if painter.align_tip_labels && !tip_extensions.is_empty() {
        for node in tree.nodes.iter().filter(|n| n.is_leaf()) {
            if let Some(&ext) = tip_extensions.get(&node.id) {
                let start_w = layout.positions[node.id];
                let end_w = match layout.layout_type {
                    TreeLayoutType::Rectangular
                    | TreeLayoutType::Slanted
                    | TreeLayoutType::Cladogram
                    | TreeLayoutType::Phylogram => (start_w.0 + ext as f32, start_w.1),
                    _ => continue,
                };
                primitives.push(ScenePrimitive::StrokeLine {
                    from: to_local(to_screen(start_w)),
                    to: to_local(to_screen(end_w)),
                    style: StrokeStyle {
                        width: 1.0,
                        color: Color32::from_rgba_unmultiplied(120, 120, 120, 180),
                        dash: Some((6.0, 4.0)),
                    },
                });
            }
        }
    }
    if painter.align_tip_labels
        && matches!(layout.layout_type, TreeLayoutType::Circular)
        && !circular_tip_align_data.is_empty()
    {
        for node in tree.nodes.iter().filter(|n| n.is_leaf()) {
            let Some(&(ext, dir)) = circular_tip_align_data.get(&node.id) else {
                continue;
            };
            let start = to_local(to_screen(layout.positions[node.id]));
            let end = start + dir * ext;
            primitives.push(ScenePrimitive::StrokeLine {
                from: start,
                to: end,
                style: StrokeStyle {
                    width: 1.0,
                    color: Color32::from_rgba_unmultiplied(120, 120, 120, 180),
                    dash: Some((6.0, 4.0)),
                },
            });
        }
    }

    let tip_value_range = if matches!(painter.tip_shape_size_mode, ShapeSizeMode::Attribute) {
        shape_value_range_for_nodes(tree, true, painter.tip_shape_size_attribute.as_deref())
    } else {
        None
    };
    let node_value_range = if matches!(painter.node_shape_size_mode, ShapeSizeMode::Attribute) {
        shape_value_range_for_nodes(tree, false, painter.node_shape_size_attribute.as_deref())
    } else {
        None
    };

    let mut tip_label_hits = Vec::new();
    let node_heights = if painter.show_node_labels
        && matches!(painter.node_label_display, crate::tree::painter::NodeLabelDisplay::NodeHeight)
    {
        Some(painter.compute_node_heights(tree))
    } else {
        None
    };
    for node in &tree.nodes {
        let mut p = to_local(to_screen(layout.positions[node.id]));
        if node.is_leaf() && painter.align_tip_labels {
            if let Some(&ext) = tip_extensions.get(&node.id) {
                if matches!(
                    layout.layout_type,
                    TreeLayoutType::Rectangular
                        | TreeLayoutType::Slanted
                        | TreeLayoutType::Cladogram
                        | TreeLayoutType::Phylogram
                ) {
                    p = to_local(to_screen((
                        layout.positions[node.id].0 + ext as f32,
                        layout.positions[node.id].1,
                    )));
                }
            }
        }

        let is_selected = selected_nodes.contains(&node.id) || selected_tips.contains(&node.id);
        let base_fill = if node.is_leaf() {
            painter.leaf_color
        } else {
            painter.internal_node_color
        };
        let fill = if node.is_leaf() {
            match painter.tip_shape_color_mode {
                ShapeColorMode::Fixed => painter.tip_shape_fixed_color,
                ShapeColorMode::UserSelection => painter
                    .tip_label_color_override(node.id)
                    .unwrap_or(base_fill),
            }
        } else {
            match painter.node_shape_color_mode {
                ShapeColorMode::Fixed => painter.node_shape_fixed_color,
                ShapeColorMode::UserSelection => painter
                    .branch_color_override(node.id)
                    .unwrap_or(base_fill),
            }
        };
        let should_draw = if node.is_leaf() {
            painter.show_tip_shapes
        } else {
            painter.show_node_shapes
        };
        if should_draw {
            let is_tip = node.is_leaf();
            let radius = painter.shape_size_for_node(
                tree,
                node.id,
                is_tip,
                if is_tip { tip_value_range } else { node_value_range },
            );
            let shape = if is_tip { painter.tip_shape } else { painter.node_shape };
            if is_selected {
                let highlight_radius = (radius + 2.0).max(radius * 1.8);
                push_shape_primitive(
                    &mut primitives,
                    p,
                    highlight_radius,
                    painter.tip_selection_color,
                    shape,
                );
            }
            push_shape_primitive(&mut primitives, p, radius, fill, shape);
        }

        if node.is_leaf() && painter.show_tip_labels {
            if let Some(text) = painter.tip_label_text(tree, node) {
                if !text.is_empty() {
                    let color = painter
                        .tip_label_color_override(node.id)
                        .unwrap_or(painter.label_color);
                    match layout.layout_type {
                        TreeLayoutType::Circular
                        | TreeLayoutType::Radial
                        | TreeLayoutType::Daylight => {
                            let base_angle = match layout.layout_type {
                                TreeLayoutType::Circular => {
                                    compute_circular_tip_angle_screen(tree, layout, node, &to_screen)
                                }
                                _ => compute_radial_tip_angle_screen(tree, layout, node, &to_screen),
                            };
                            let offset = if matches!(layout.layout_type, TreeLayoutType::Circular) {
                                12.0
                            } else {
                                8.0
                            };
                            let extra_offset = if painter.align_tip_labels
                                && matches!(layout.layout_type, TreeLayoutType::Circular)
                            {
                                circular_tip_align_data
                                    .get(&node.id)
                                    .map(|(ext, _)| *ext)
                                    .unwrap_or(0.0)
                            } else {
                                0.0
                            };
                            let dir = if matches!(layout.layout_type, TreeLayoutType::Circular) {
                                circular_tip_align_data
                                    .get(&node.id)
                                    .map(|(_, d)| *d)
                                    .unwrap_or_else(|| Vec2::new(base_angle.cos(), base_angle.sin()))
                            } else {
                                Vec2::new(base_angle.cos(), base_angle.sin())
                            };
                            let anchor_pos = p + dir * (offset + extra_offset);
                            let (rotation, align) = readable_rotation(base_angle);
                            let text_size = approx_text_size(&text, painter.tip_label_font_size);
                            let hit_rect =
                                rotated_text_bounds(anchor_pos, text_size, rotation, align);
                            if selected_tips.contains(&node.id) {
                                primitives.push(ScenePrimitive::FillPolygon {
                                    points: rotated_expanded_corners(
                                        anchor_pos, text_size, rotation, align, 2.0,
                                    ),
                                    color: painter.tip_selection_color,
                                });
                            }
                            tip_label_hits.push(TipLabelHit {
                                node_id: node.id,
                                rect: hit_rect,
                                rotation_angle: rotation,
                                anchor: align,
                                anchor_pos,
                                text_size,
                            });
                            primitives.push(ScenePrimitive::Text {
                                text,
                                anchor: anchor_pos,
                                angle: rotation,
                                align,
                                size: painter.tip_label_font_size,
                                color,
                            });
                        }
                        _ => {
                            let anchor_pos = Pos2::new(p.x + 8.0, p.y);
                            let text_size = approx_text_size(&text, painter.tip_label_font_size);
                            let hit_rect = Rect::from_min_size(
                                Pos2::new(anchor_pos.x, anchor_pos.y - text_size.y * 0.5),
                                text_size,
                            );
                            if selected_tips.contains(&node.id) {
                                primitives.push(ScenePrimitive::FillRect {
                                    rect: hit_rect.expand(2.0),
                                    color: painter.tip_selection_color,
                                });
                            }
                            tip_label_hits.push(TipLabelHit {
                                node_id: node.id,
                                rect: hit_rect,
                                rotation_angle: 0.0,
                                anchor: egui::Align2::LEFT_CENTER,
                                anchor_pos,
                                text_size,
                            });
                            primitives.push(ScenePrimitive::Text {
                                text,
                                anchor: anchor_pos,
                                angle: 0.0,
                                align: egui::Align2::LEFT_CENTER,
                                size: painter.tip_label_font_size,
                                color,
                            });
                        }
                    }
                }
            }
        } else if painter.show_node_labels {
            if let Some(name) = painter.node_label_text(tree, node, node_heights.as_ref()) {
                primitives.push(ScenePrimitive::Text {
                    text: name,
                    anchor: Pos2::new(p.x + 8.0, p.y),
                    angle: 0.0,
                    align: egui::Align2::LEFT_CENTER,
                    size: painter.node_label_font_size,
                    color: painter.label_color,
                });
            }
        }

        if painter.show_branch_labels {
            if let (Some(length), Some(parent_id)) = (node.length, node.parent) {
                let pp = to_local(to_screen(layout.positions[parent_id]));
                primitives.push(ScenePrimitive::Text {
                    text: format!("{length:.3}"),
                    anchor: Pos2::new((pp.x + p.x) * 0.5, (pp.y + p.y) * 0.5 - 12.0),
                    angle: 0.0,
                    align: egui::Align2::CENTER_CENTER,
                    size: 10.0,
                    color: painter.branch_label_color,
                });
            }
        }
    }

    if painter.show_scale_bar && layout.width > f32::EPSILON {
        let tick = nice_tick_span(layout.width);
        let scale_x = if layout.width <= f32::EPSILON {
            1.0
        } else {
            transform_inner.width() / layout.width
        };
        let bar_pixels = tick * scale_x;
        let baseline = to_local(canvas_inner.max).y - 18.0;
        let start = Pos2::new(to_local(canvas_inner.min).x + 32.0, baseline);
        let end = Pos2::new(start.x + bar_pixels, baseline);
        let c = Color32::from_rgb(210, 215, 220);
        let stroke = StrokeStyle {
            width: 2.0,
            color: c,
            dash: None,
        };
        primitives.push(ScenePrimitive::StrokeLine {
            from: start,
            to: end,
            style: stroke,
        });
        primitives.push(ScenePrimitive::StrokeLine {
            from: start,
            to: Pos2::new(start.x, baseline - 8.0),
            style: stroke,
        });
        primitives.push(ScenePrimitive::StrokeLine {
            from: end,
            to: Pos2::new(end.x, baseline - 8.0),
            style: stroke,
        });
        primitives.push(ScenePrimitive::Text {
            text: format!("Scale: {tick:.3}"),
            anchor: Pos2::new((start.x + end.x) * 0.5, baseline - 24.0),
            angle: 0.0,
            align: egui::Align2::CENTER_CENTER,
            size: 11.0,
            color: Color32::from_rgb(220, 225, 230),
        });
    }

    TreeSceneGraph {
        size: rect.size(),
        primitives,
        tip_label_hits,
    }
}

fn calculate_tip_extensions(tree: &Tree) -> HashMap<NodeId, f64> {
    let mut distances = HashMap::new();
    fn dfs(node_id: NodeId, distance: f64, tree: &Tree, distances: &mut HashMap<NodeId, f64>) {
        distances.insert(node_id, distance);
        let node = &tree.nodes[node_id];
        for &child_id in &node.children {
            let branch_length = tree.nodes[child_id].length.unwrap_or(1.0);
            dfs(child_id, distance + branch_length, tree, distances);
        }
    }
    if let Some(root_id) = tree.root {
        dfs(root_id, 0.0, tree, &mut distances);
    }
    let max_distance = tree
        .nodes
        .iter()
        .filter(|n| n.is_leaf())
        .filter_map(|n| distances.get(&n.id))
        .fold(0.0f64, |a, &b| a.max(b));
    let mut extensions = HashMap::new();
    for node in &tree.nodes {
        if node.is_leaf() {
            if let Some(&distance) = distances.get(&node.id) {
                let ext = max_distance - distance;
                if ext > 1e-6 {
                    extensions.insert(node.id, ext);
                }
            }
        }
    }
    extensions
}

fn angle_if_non_degenerate(dx: f32, dy: f32) -> Option<f32> {
    const EPS2: f32 = 1e-10;
    if (dx * dx + dy * dy) > EPS2 {
        Some(dy.atan2(dx))
    } else {
        None
    }
}

fn sibling_fanout_angle(tree: &Tree, parent_id: NodeId, node_id: NodeId, base_angle: f32) -> Option<f32> {
    let siblings = &tree.nodes[parent_id].children;
    if siblings.len() <= 1 {
        return None;
    }
    let idx = siblings.iter().position(|&id| id == node_id)? as f32;
    let n = siblings.len() as f32;
    let spread_total = ((20.0f32).to_radians() * (n - 1.0)).min((120.0f32).to_radians());
    let step = if n > 1.0 { spread_total / (n - 1.0) } else { 0.0 };
    let offset = -0.5 * spread_total + idx * step;
    Some(base_angle + offset)
}

fn compute_circular_tip_angle_screen<F>(
    tree: &Tree,
    layout: &TreeLayout,
    node: &TreeNode,
    to_screen: &F,
) -> f32
where
    F: Fn((f32, f32)) -> Pos2,
{
    let child = to_screen(layout.positions[node.id]);

    // 1) Prefer terminal segment direction from continuous branch geometry.
    if let Some(branch) = layout
        .continuous_branches
        .iter()
        .find(|b| b.child == node.id)
    {
        if branch.points.len() >= 2 {
            for world_p in branch.points.iter().skip(1) {
                let p = to_screen(*world_p);
                if let Some(angle) = angle_if_non_degenerate(child.x - p.x, child.y - p.y) {
                    return angle;
                }
            }
        }
    }

    // 2) Fallback to direct parent->child direction.
    if let Some(parent_id) = node.parent {
        let parent = to_screen(layout.positions[parent_id]);
        if let Some(angle) = angle_if_non_degenerate(child.x - parent.x, child.y - parent.y) {
            return angle;
        }

        // 2.5) Degenerate parent->child: fan out siblings around parent outward direction.
        let center = to_screen((layout.width * 0.5, layout.height * 0.5));
        let base = angle_if_non_degenerate(parent.x - center.x, parent.y - center.y)
            .or_else(|| {
                let mut anc = tree.nodes[parent_id].parent;
                while let Some(anc_id) = anc {
                    let anc_p = to_screen(layout.positions[anc_id]);
                    if let Some(a) = angle_if_non_degenerate(parent.x - anc_p.x, parent.y - anc_p.y) {
                        return Some(a);
                    }
                    anc = tree.nodes[anc_id].parent;
                }
                None
            })
            .unwrap_or(0.0);
        if let Some(fanned) = sibling_fanout_angle(tree, parent_id, node.id, base) {
            return fanned;
        }
    }

    // 3) Radial outward direction from layout center.
    let center = to_screen((layout.width * 0.5, layout.height * 0.5));
    if let Some(angle) = angle_if_non_degenerate(child.x - center.x, child.y - center.y) {
        return angle;
    }

    // 4) Walk up ancestors until a non-overlapping point is found.
    let mut anc = node.parent;
    while let Some(anc_id) = anc {
        let anc_p = to_screen(layout.positions[anc_id]);
        if let Some(angle) = angle_if_non_degenerate(child.x - anc_p.x, child.y - anc_p.y) {
            return angle;
        }
        anc = tree.nodes[anc_id].parent;
    }

    // 5) Hard fallback.
    0.0
}

fn compute_radial_tip_angle_screen<F>(
    tree: &Tree,
    layout: &TreeLayout,
    node: &TreeNode,
    to_screen: &F,
) -> f32
where
    F: Fn((f32, f32)) -> Pos2,
{
    let child = to_screen(layout.positions[node.id]);

    // 1) Direct parent->child direction.
    if let Some(parent_id) = node.parent {
        let parent = to_screen(layout.positions[parent_id]);
        if let Some(angle) = angle_if_non_degenerate(child.x - parent.x, child.y - parent.y) {
            return angle;
        }

        // 1.5) Degenerate parent->child: fan out siblings around parent outward direction.
        let center = to_screen((layout.width * 0.5, layout.height * 0.5));
        let base = angle_if_non_degenerate(parent.x - center.x, parent.y - center.y)
            .or_else(|| {
                let mut anc = tree.nodes[parent_id].parent;
                while let Some(anc_id) = anc {
                    let anc_p = to_screen(layout.positions[anc_id]);
                    if let Some(a) = angle_if_non_degenerate(parent.x - anc_p.x, parent.y - anc_p.y) {
                        return Some(a);
                    }
                    anc = tree.nodes[anc_id].parent;
                }
                None
            })
            .unwrap_or(0.0);
        if let Some(fanned) = sibling_fanout_angle(tree, parent_id, node.id, base) {
            return fanned;
        }
    }

    // 2) Outward direction from layout center.
    let center = to_screen((layout.width * 0.5, layout.height * 0.5));
    if let Some(angle) = angle_if_non_degenerate(child.x - center.x, child.y - center.y) {
        return angle;
    }

    // 3) Walk up ancestors until a non-overlapping point is found.
    let mut anc = node.parent;
    while let Some(anc_id) = anc {
        let anc_p = to_screen(layout.positions[anc_id]);
        if let Some(angle) = angle_if_non_degenerate(child.x - anc_p.x, child.y - anc_p.y) {
            return angle;
        }
        anc = tree.nodes[anc_id].parent;
    }

    // 4) Hard fallback.
    0.0
}

fn readable_rotation(angle: f32) -> (f32, egui::Align2) {
    let degree = angle.to_degrees();
    let normalized = if degree < 0.0 { degree + 360.0 } else { degree };
    if normalized > 90.0 && normalized < 270.0 {
        (angle + std::f32::consts::PI, egui::Align2::RIGHT_CENTER)
    } else {
        (angle, egui::Align2::LEFT_CENTER)
    }
}

fn corners_for_anchor(text_size: Vec2, align: egui::Align2) -> [Vec2; 4] {
    match align {
        egui::Align2::LEFT_CENTER => [
            Vec2::new(0.0, -text_size.y / 2.0),
            Vec2::new(text_size.x, -text_size.y / 2.0),
            Vec2::new(text_size.x, text_size.y / 2.0),
            Vec2::new(0.0, text_size.y / 2.0),
        ],
        egui::Align2::RIGHT_CENTER => [
            Vec2::new(-text_size.x, -text_size.y / 2.0),
            Vec2::new(0.0, -text_size.y / 2.0),
            Vec2::new(0.0, text_size.y / 2.0),
            Vec2::new(-text_size.x, text_size.y / 2.0),
        ],
        _ => {
            let half = text_size * 0.5;
            [
                Vec2::new(-half.x, -half.y),
                Vec2::new(half.x, -half.y),
                Vec2::new(half.x, half.y),
                Vec2::new(-half.x, half.y),
            ]
        }
    }
}

fn rotated_expanded_corners(
    anchor: Pos2,
    text_size: Vec2,
    angle: f32,
    align: egui::Align2,
    expand: f32,
) -> Vec<Pos2> {
    let corners = corners_for_anchor(text_size, align);
    let expanded = [
        corners[0] + Vec2::new(-expand, -expand),
        corners[1] + Vec2::new(expand, -expand),
        corners[2] + Vec2::new(expand, expand),
        corners[3] + Vec2::new(-expand, expand),
    ];
    let ca = angle.cos();
    let sa = angle.sin();
    expanded
        .iter()
        .map(|c| {
            let rv = Vec2::new(c.x * ca - c.y * sa, c.x * sa + c.y * ca);
            anchor + rv
        })
        .collect()
}

fn rotated_text_bounds(anchor: Pos2, text_size: Vec2, angle: f32, align: egui::Align2) -> Rect {
    let corners = corners_for_anchor(text_size, align);
    let ca = angle.cos();
    let sa = angle.sin();
    let mut min_x = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_y = f32::NEG_INFINITY;
    for c in corners {
        let rx = c.x * ca - c.y * sa;
        let ry = c.x * sa + c.y * ca;
        let p = anchor + Vec2::new(rx, ry);
        min_x = min_x.min(p.x);
        max_x = max_x.max(p.x);
        min_y = min_y.min(p.y);
        max_y = max_y.max(p.y);
    }
    Rect::from_min_max(Pos2::new(min_x, min_y), Pos2::new(max_x, max_y))
}

fn approx_text_size(text: &str, font_size: f32) -> Vec2 {
    Vec2::new(
        (text.chars().count() as f32 * font_size * 0.56).max(2.0),
        font_size.max(8.0),
    )
}

fn shape_value_range_for_nodes(
    tree: &Tree,
    tips_only: bool,
    attr: Option<&str>,
) -> Option<(f64, f64)> {
    let key = attr?;
    let mut min_v = f64::INFINITY;
    let mut max_v = f64::NEG_INFINITY;
    for n in &tree.nodes {
        if tips_only != n.is_leaf() {
            continue;
        }
        if let Some(v) = n.get_numeric_attribute(key) {
            if v.is_finite() {
                min_v = min_v.min(v);
                max_v = max_v.max(v);
            }
        }
    }
    if min_v.is_finite() && max_v.is_finite() {
        Some((min_v, max_v))
    } else {
        None
    }
}

fn push_shape_primitive(
    primitives: &mut Vec<ScenePrimitive>,
    center: Pos2,
    radius: f32,
    color: Color32,
    shape: ShapeType,
) {
    match shape {
        ShapeType::Circle => primitives.push(ScenePrimitive::FillCircle {
            center,
            radius,
            color,
        }),
        ShapeType::Square => {
            let half = radius.max(0.5);
            primitives.push(ScenePrimitive::FillRect {
                rect: Rect::from_min_max(
                    Pos2::new(center.x - half, center.y - half),
                    Pos2::new(center.x + half, center.y + half),
                ),
                color,
            });
        }
        ShapeType::Diamond => {
            let r = radius.max(0.5);
            primitives.push(ScenePrimitive::FillPolygon {
                points: vec![
                    Pos2::new(center.x, center.y - r),
                    Pos2::new(center.x + r, center.y),
                    Pos2::new(center.x, center.y + r),
                    Pos2::new(center.x - r, center.y),
                ],
                color,
            });
        }
    }
}

fn nice_tick_span(tree_width: f32) -> f32 {
    let raw = tree_width / 5.0;
    let exponent = raw.abs().log10().floor();
    let base = 10_f32.powf(exponent);
    [1.0, 2.0, 5.0, 10.0]
        .into_iter()
        .map(|m| m * base)
        .find(|v| *v >= raw)
        .unwrap_or(10.0 * base)
}
