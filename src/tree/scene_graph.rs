use std::collections::{HashMap, HashSet};

use eframe::egui::{self, Color32, Pos2, Rect, Vec2};

use crate::tree::layout::{TreeLayout, TreeLayoutType};
use crate::tree::painter::{HighlightShape, TipLabelHit, TreePainter};
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
                let c = to_local(to_screen((center.x, center.y)));
                primitives.push(ScenePrimitive::FillSector {
                    center: c,
                    inner_radius,
                    outer_radius,
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

    if matches!(layout.layout_type, TreeLayoutType::Circular) && !layout.arc_segments.is_empty() {
        for (parent, child) in &layout.edges {
            let is_selected = selected_nodes.contains(child)
                && !matches!(selection_mode, Some(SelectionMode::Taxa));
            let color = if is_selected {
                selected_branch_color
            } else {
                painter.branch_color_override(*child).unwrap_or(branch_color)
            };
            let width = if is_selected {
                painter.branch_highlight_stroke.width.max(branch_width * 1.8)
            } else {
                branch_width
            };
            let style = StrokeStyle {
                width,
                color,
                dash: None,
            };
            if let Some(arc) = layout
                .arc_segments
                .iter()
                .find(|a| a.child == *child && a.parent == *parent)
            {
                primitives.push(ScenePrimitive::StrokeCircularBranch {
                    child: to_local(to_screen(layout.positions[*child])),
                    center: to_local(to_screen(arc.center)),
                    radius: (to_local(to_screen((arc.center.0 + arc.radius, arc.center.1)))
                        - to_local(to_screen(arc.center)))
                    .length(),
                    start_angle: arc.start_angle,
                    end_angle: arc.end_angle,
                    style,
                });
            } else {
                primitives.push(ScenePrimitive::StrokeLine {
                    from: to_local(to_screen(layout.positions[*parent])),
                    to: to_local(to_screen(layout.positions[*child])),
                    style,
                });
            }
        }
    } else if !layout.continuous_branches.is_empty() {
        for branch in &layout.continuous_branches {
            let is_selected = selected_nodes.contains(&branch.child)
                && !matches!(selection_mode, Some(SelectionMode::Taxa));
            let color = if is_selected {
                selected_branch_color
            } else {
                painter
                    .branch_color_override(branch.child)
                    .unwrap_or(branch_color)
            };
            let width = if is_selected {
                painter.branch_highlight_stroke.width.max(branch_width * 1.8)
            } else {
                branch_width
            };
            let points: Vec<Pos2> = branch
                .points
                .iter()
                .map(|&p| to_local(to_screen(p)))
                .collect();
            primitives.push(ScenePrimitive::StrokePolyline {
                points,
                style: StrokeStyle {
                    width,
                    color,
                    dash: None,
                },
            });
        }
    } else {
        for (parent, child) in &layout.edges {
            let is_selected = selected_nodes.contains(child)
                && !matches!(selection_mode, Some(SelectionMode::Taxa));
            let color = if is_selected {
                selected_branch_color
            } else {
                painter.branch_color_override(*child).unwrap_or(branch_color)
            };
            let width = if is_selected {
                painter.branch_highlight_stroke.width.max(branch_width * 1.8)
            } else {
                branch_width
            };
            primitives.push(ScenePrimitive::StrokeLine {
                from: to_local(to_screen(layout.positions[*parent])),
                to: to_local(to_screen(layout.positions[*child])),
                style: StrokeStyle {
                    width,
                    color,
                    dash: None,
                },
            });
        }
    }

    if painter.show_node_bars {
        if let Some(field) = painter.node_bar_field() {
            if matches!(
                layout.layout_type,
                TreeLayoutType::Rectangular
                    | TreeLayoutType::Phylogram
                    | TreeLayoutType::Slanted
                    | TreeLayoutType::Daylight
            ) {
                for node in &tree.nodes {
                    let Some((mut min, mut max)) = node.numeric_range_attribute(field) else {
                        continue;
                    };
                    if min > max {
                        std::mem::swap(&mut min, &mut max);
                    }
                    let y = layout.positions[node.id].1;
                    let p0 = to_local(to_screen((min as f32, y)));
                    let p1 = to_local(to_screen((max as f32, y)));
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
        }
    }

    let tip_extensions = if painter.align_tip_labels {
        calculate_tip_extensions(tree)
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

    let mut tip_label_hits = Vec::new();
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
        let fill = if is_selected {
            painter.selected_color
        } else if node.is_leaf() {
            painter.leaf_color
        } else {
            painter.internal_node_color
        };
        let should_draw = if node.is_leaf() {
            painter.show_tip_shapes
        } else {
            painter.show_node_shapes
        };
        if should_draw {
            primitives.push(ScenePrimitive::FillCircle {
                center: p,
                radius: painter.node_radius,
                color: fill,
            });
        }

        if node.is_leaf() && painter.show_tip_labels {
            if let Some(text) = painter.tip_label_text(tree, node) {
                if !text.is_empty() {
                    let color = painter
                        .tip_label_color_override(node.id)
                        .unwrap_or(painter.label_color);
                    match layout.layout_type {
                        TreeLayoutType::Circular | TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                            let base_angle = match layout.layout_type {
                                TreeLayoutType::Circular => {
                                    compute_circular_tip_angle_screen(layout, node, &to_screen)
                                }
                                _ => compute_radial_tip_angle_screen(layout, node, &to_screen),
                            };
                            let offset = if matches!(layout.layout_type, TreeLayoutType::Circular) {
                                12.0
                            } else {
                                8.0
                            };
                            let dir = Vec2::new(base_angle.cos(), base_angle.sin());
                            let anchor_pos = p + dir * offset;
                            let (rotation, align) = readable_rotation(base_angle);
                            let text_size = approx_text_size(&text, painter.tip_label_font_size);
                            let hit_rect = rotated_text_bounds(anchor_pos, text_size, rotation, align);
                            if selected_tips.contains(&node.id) {
                                primitives.push(ScenePrimitive::FillPolygon {
                                    points: rotated_expanded_corners(anchor_pos, text_size, rotation, align, 2.0),
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
            if let Some(name) = &node.name {
                primitives.push(ScenePrimitive::Text {
                    text: name.clone(),
                    anchor: Pos2::new(p.x + 8.0, p.y),
                    angle: 0.0,
                    align: egui::Align2::LEFT_CENTER,
                    size: 10.0,
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

fn compute_circular_tip_angle_screen<F>(layout: &TreeLayout, node: &TreeNode, to_screen: &F) -> f32
where
    F: Fn((f32, f32)) -> Pos2,
{
    if let Some(parent_id) = node.parent {
        if let Some(branch) = layout.continuous_branches.iter().find(|b| b.child == node.id) {
            if branch.points.len() >= 2 {
                let child = to_screen(branch.points[0]);
                let shoulder = to_screen(branch.points[1]);
                return (child.y - shoulder.y).atan2(child.x - shoulder.x);
            }
        }
        let parent = to_screen(layout.positions[parent_id]);
        let child = to_screen(layout.positions[node.id]);
        (child.y - parent.y).atan2(child.x - parent.x)
    } else {
        let center = to_screen((layout.width * 0.5, layout.height * 0.5));
        let node_p = to_screen(layout.positions[node.id]);
        (node_p.y - center.y).atan2(node_p.x - center.x)
    }
}

fn compute_radial_tip_angle_screen<F>(layout: &TreeLayout, node: &TreeNode, to_screen: &F) -> f32
where
    F: Fn((f32, f32)) -> Pos2,
{
    if let Some(parent_id) = node.parent {
        let parent = to_screen(layout.positions[parent_id]);
        let child = to_screen(layout.positions[node.id]);
        (child.y - parent.y).atan2(child.x - parent.x)
    } else {
        0.0
    }
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
