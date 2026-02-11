use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

use eframe::egui::{self, vec2, Color32, FontFamily, FontId, Stroke, Vec2};

use super::{NodeId, Tree, TreeNode};
use crate::rotated_text::RotatedText;
use crate::tree::layout::TreeLayout;

#[derive(Clone, Debug)]
pub enum HighlightShape {
    Rect {
        top_left: egui::Pos2,
        bottom_right: egui::Pos2,
        color: Color32,
    },
    Sector {
        center: egui::Pos2,
        inner_radius: f32,
        outer_radius: f32,
        start_angle: f32,
        end_angle: f32,
        color: Color32,
    },
    Polygon {
        points: Vec<egui::Pos2>,
        color: Color32,
    },
}

// 用于存储标签绘制信息的结构
struct TipLabelInfo {
    rect: egui::Rect,
    rotation_angle: f32,
    anchor: egui::Align2,
    anchor_pos: egui::Pos2,
    text_size: egui::Vec2,
}

#[derive(Clone)]
pub struct TipLabelHit {
    pub node_id: NodeId,
    pub rect: egui::Rect,
    pub rotation_angle: f32,    // 旋转角度
    pub anchor: egui::Align2,   // 锚点类型
    pub anchor_pos: egui::Pos2, // 锚点位置（旋转中心）
    pub text_size: egui::Vec2,  // 原始文本尺寸
}

impl TipLabelHit {
    /// 检测点是否在旋转的标签内（使用多边形检测）
    pub fn contains(&self, point: egui::Pos2, expand: f32) -> bool {
        // 如果旋转角度很小，使用简单的矩形测试
        let angle_tolerance = 0.1; // 约5.7度
        if self.rotation_angle.abs() < angle_tolerance {
            return self.rect.expand(expand).contains(point);
        }

        // 对于旋转的标签，使用精确的多边形检测
        // 计算文本四个角相对于锚点的原始位置
        let corners = match self.anchor {
            egui::Align2::LEFT_CENTER => [
                egui::vec2(0.0, -self.text_size.y / 2.0),
                egui::vec2(self.text_size.x, -self.text_size.y / 2.0),
                egui::vec2(self.text_size.x, self.text_size.y / 2.0),
                egui::vec2(0.0, self.text_size.y / 2.0),
            ],
            egui::Align2::RIGHT_CENTER => [
                egui::vec2(-self.text_size.x, -self.text_size.y / 2.0),
                egui::vec2(0.0, -self.text_size.y / 2.0),
                egui::vec2(0.0, self.text_size.y / 2.0),
                egui::vec2(-self.text_size.x, self.text_size.y / 2.0),
            ],
            _ => {
                // 其他锚点类型的默认处理
                let half = self.text_size / 2.0;
                [
                    egui::vec2(-half.x, -half.y),
                    egui::vec2(half.x, -half.y),
                    egui::vec2(half.x, half.y),
                    egui::vec2(-half.x, half.y),
                ]
            }
        };

        // 添加expand边距并旋转四个角点
        let cos_a = self.rotation_angle.cos();
        let sin_a = self.rotation_angle.sin();

        // 扩展角点（在旋转前扩展）
        let expanded_corners = [
            corners[0] + egui::vec2(-expand, -expand),
            corners[1] + egui::vec2(expand, -expand),
            corners[2] + egui::vec2(expand, expand),
            corners[3] + egui::vec2(-expand, expand),
        ];

        // 旋转并转换到世界坐标
        let rotated_corners: Vec<egui::Pos2> = expanded_corners
            .iter()
            .map(|corner| {
                let rotated = egui::vec2(
                    corner.x * cos_a - corner.y * sin_a,
                    corner.x * sin_a + corner.y * cos_a,
                );
                self.anchor_pos + rotated
            })
            .collect();

        // 使用点在多边形内的测试算法（射线法）
        self.point_in_polygon(point, &rotated_corners)
    }

    /// 检测点是否在多边形内部（射线法）
    fn point_in_polygon(&self, point: egui::Pos2, polygon: &[egui::Pos2]) -> bool {
        let mut inside = false;
        let n = polygon.len();

        for i in 0..n {
            let j = (i + 1) % n;
            let xi = polygon[i].x;
            let yi = polygon[i].y;
            let xj = polygon[j].x;
            let yj = polygon[j].y;

            let intersect = ((yi > point.y) != (yj > point.y))
                && (point.x < (xj - xi) * (point.y - yi) / (yj - yi) + xi);

            if intersect {
                inside = !inside;
            }
        }

        inside
    }
}

#[derive(Debug, Clone)]
pub struct TreePainter {
    pub branch_stroke: Stroke,
    pub branch_highlight_stroke: Stroke,
    pub node_radius: f32,
    pub leaf_color: Color32,
    pub internal_node_color: Color32,
    pub selected_color: Color32,
    pub label_color: Color32,
    pub branch_label_color: Color32,
    pub background_color: Color32,
    pub canvas_color: Color32,
    pub highlight_color: Color32,
    pub show_tip_labels: bool,
    pub show_node_labels: bool,
    pub show_branch_labels: bool,
    pub show_scale_bar: bool,
    pub show_tip_shapes: bool,
    pub show_node_shapes: bool,
    pub show_node_bars: bool,
    pub node_bar_field: Option<String>,
    pub node_bar_color: Color32,
    pub node_bar_thickness: f32,
    pub align_tip_labels: bool,
    pub tip_label_display: TipLabelDisplay,
    pub tip_label_font_family: TipLabelFontFamily,
    pub tip_label_font_size: f32,
    pub tip_label_format: TipLabelNumberFormat,
    pub tip_label_precision: usize,
    pub tip_selection_color: Color32,
    branch_color_overrides: HashMap<NodeId, Color32>,
    tip_label_color_overrides: HashMap<NodeId, Color32>,
    highlighted_clades: HashMap<NodeId, Color32>,
}

impl Default for TreePainter {
    fn default() -> Self {
        Self {
            branch_stroke: Stroke::new(1.8, Color32::BLACK),
            branch_highlight_stroke: Stroke::new(5.0, Color32::from_rgb(120, 170, 255)),
            node_radius: 3.0,
            leaf_color: Color32::from_rgb(255, 205, 140),
            internal_node_color: Color32::from_rgb(135, 205, 255),
            selected_color: Color32::from_rgb(255, 100, 100),
            label_color: Color32::BLACK,
            branch_label_color: Color32::BLACK,
            background_color: Color32::WHITE,
            canvas_color: Color32::WHITE,
            highlight_color: Color32::from_rgb(96, 186, 255),
            show_tip_labels: true,
            show_node_labels: false,
            show_branch_labels: false,
            show_scale_bar: false,
            show_tip_shapes: false,
            show_node_shapes: false,
            show_node_bars: false,
            node_bar_field: None,
            node_bar_color: Color32::from_rgba_unmultiplied(96, 186, 255, 100),
            node_bar_thickness: 10.0,
            align_tip_labels: false,
            tip_label_display: TipLabelDisplay::Labels,
            tip_label_font_family: TipLabelFontFamily::Proportional,
            tip_label_font_size: 13.0,
            tip_label_format: TipLabelNumberFormat::Decimal,
            tip_label_precision: 4,
            tip_selection_color: Color32::from_rgba_unmultiplied(120, 170, 255, 150),
            branch_color_overrides: HashMap::new(),
            tip_label_color_overrides: HashMap::new(),
            highlighted_clades: HashMap::new(),
        }
    }
}

impl TreePainter {
    /// Create a coordinate transformation function from layout space to screen space.
    /// This ensures consistent coordinate transformation between rendering and hit testing.
    pub fn create_to_screen_transform(
        &self,
        tree: &Tree,
        layout: &TreeLayout,
        inner: egui::Rect,
    ) -> impl Fn((f32, f32)) -> egui::Pos2 + '_ {
        // Calculate scaling factors
        let scale_x = if layout.width <= f32::EPSILON {
            inner.width().max(1.0)
        } else {
            inner.width().max(1.0) / layout.width
        };

        let scale_y = if layout.height <= f32::EPSILON {
            inner.height().max(1.0)
        } else {
            inner.height().max(1.0) / layout.height
        };

        // Pre-calculate parameters for circular/radial layouts
        let (layout_center, layout_radius, available_radius, center_x, center_y) = match layout
            .layout_type
        {
            super::layout::TreeLayoutType::Circular
            | super::layout::TreeLayoutType::Radial
            | super::layout::TreeLayoutType::Daylight => {
                // Reserve radial margin for visible tip labels to reduce clipping.
                // This is independent of align_tip_labels for circular/radial layouts.
                let label_margin = if self.show_tip_labels {
                    let max_chars = tree
                        .nodes
                        .iter()
                        .filter(|n| n.is_leaf())
                        .filter_map(|n| self.tip_label_text(tree, n))
                        .map(|s| s.chars().count())
                        .max()
                        .unwrap_or(0) as f32;
                    let text_w = max_chars * self.tip_label_font_size * 0.56;
                    let text_h = self.tip_label_font_size.max(8.0);
                    let text_radius = text_w.max(text_h) * 0.5;
                    let offset =
                        if matches!(layout.layout_type, super::layout::TreeLayoutType::Circular) {
                            12.0
                        } else {
                            8.0
                        };
                    let max_margin = inner.width().min(inner.height()) * 0.30;
                    (offset + text_radius).min(max_margin)
                } else {
                    0.0
                };
                let base_radius = inner.width().min(inner.height()) * 0.5;
                let min_radius = inner.width().min(inner.height()) * 0.12;
                let available_radius = (base_radius - label_margin - 2.0).max(min_radius);

                // 获取根节点位置作为圆的中心（进化树形状的真实中心）
                let layout_center =
                    if matches!(layout.layout_type, super::layout::TreeLayoutType::Circular) {
                        if let Some(root_id) = tree.root {
                            layout.positions[root_id]
                        } else {
                            (layout.width * 0.5, layout.height * 0.5)
                        }
                    } else {
                        (layout.width * 0.5, layout.height * 0.5)
                    };

                // Radius for circular/radial/daylight should be based on actual geometry,
                // not rectangular tip-extension logic.
                let mut max_radius = 0.0f32;
                for node in &tree.nodes {
                    let pos = layout.positions[node.id];
                    let dx = pos.0 - layout_center.0;
                    let dy = pos.1 - layout_center.1;
                    let radius = (dx * dx + dy * dy).sqrt();
                    max_radius = max_radius.max(radius);
                }
                let layout_radius = max_radius.max(1e-6);

                // 计算画布中心
                let center_x = inner.left() + inner.width() * 0.5;
                let center_y = inner.top() + inner.height() * 0.5;

                (
                    layout_center,
                    layout_radius,
                    available_radius,
                    center_x,
                    center_y,
                )
            }
            _ => {
                // For non-circular layouts, these values won't be used
                ((0.0, 0.0), 1.0, 1.0, 0.0, 0.0)
            }
        };

        let layout_type = layout.layout_type;
        let inner_left = inner.left();
        let inner_top = inner.top();

        move |pos: (f32, f32)| -> egui::Pos2 {
            match layout_type {
                super::layout::TreeLayoutType::Circular
                | super::layout::TreeLayoutType::Radial
                | super::layout::TreeLayoutType::Daylight => {
                    let scale = available_radius / layout_radius;

                    // 将布局坐标转换为相对于画布中心的坐标（使用根节点作为参考中心）
                    let x = center_x + (pos.0 - layout_center.0) * scale;
                    let y = center_y + (pos.1 - layout_center.1) * scale;

                    egui::pos2(x, y)
                }
                _ => {
                    // 其他布局使用标准坐标转换
                    let x = inner_left + pos.0 * scale_x;
                    let y = inner_top + pos.1 * scale_y;
                    egui::pos2(x, y)
                }
            }
        }
    }

    #[allow(dead_code)]
    pub fn set_highlight_color(&mut self, color: Color32) {
        self.highlight_color = color;
        self.branch_highlight_stroke = Stroke::new(self.branch_highlight_stroke.width, color);
        let alpha = self.tip_selection_color.a();
        self.tip_selection_color =
            Color32::from_rgba_unmultiplied(color.r(), color.g(), color.b(), alpha);
    }

    pub fn set_branch_color(&mut self, node_id: NodeId, color: Color32) {
        self.branch_color_overrides.insert(node_id, color);
    }

    pub fn set_node_bar_field(&mut self, field: Option<String>) {
        self.node_bar_field = field;
    }

    pub fn node_bar_field(&self) -> Option<&str> {
        self.node_bar_field.as_deref()
    }

    pub fn set_node_bar_color(&mut self, color: Color32) {
        self.node_bar_color = color;
    }

    pub fn node_bar_color(&self) -> Color32 {
        self.node_bar_color
    }

    pub fn set_node_bar_thickness(&mut self, thickness: f32) {
        self.node_bar_thickness = thickness.max(0.5);
    }

    pub fn node_bar_thickness(&self) -> f32 {
        self.node_bar_thickness
    }

    pub fn branch_color_override(&self, node_id: NodeId) -> Option<Color32> {
        self.branch_color_overrides.get(&node_id).copied()
    }

    pub fn set_tip_label_color(&mut self, node_id: NodeId, color: Color32) {
        self.tip_label_color_overrides.insert(node_id, color);
    }

    pub fn tip_label_color_override(&self, node_id: NodeId) -> Option<Color32> {
        self.tip_label_color_overrides.get(&node_id).copied()
    }

    pub fn clear_color_overrides(&mut self) {
        self.branch_color_overrides.clear();
        self.tip_label_color_overrides.clear();
    }

    pub fn add_highlighted_clade(&mut self, node_id: NodeId, color: Color32) {
        self.highlighted_clades.insert(node_id, color);
    }

    pub fn remove_highlighted_clade(&mut self, node_id: NodeId) {
        self.highlighted_clades.remove(&node_id);
    }

    pub fn clear_highlighted_clades(&mut self) {
        self.highlighted_clades.clear();
    }

    pub fn highlighted_clades(&self) -> &HashMap<NodeId, Color32> {
        &self.highlighted_clades
    }

    pub fn compute_highlight_shapes(
        &self,
        tree: &Tree,
        layout: &TreeLayout,
    ) -> Vec<HighlightShape> {
        use crate::tree::layout::TreeLayoutType;

        if self.highlighted_clades.is_empty() {
            return Vec::new();
        }

        match layout.layout_type {
            TreeLayoutType::Rectangular | TreeLayoutType::Phylogram | TreeLayoutType::Cladogram => {
                self.compute_rectangular_highlights(tree, layout)
            }
            TreeLayoutType::Circular => self.compute_circular_highlights(tree, layout),
            TreeLayoutType::Radial => self.compute_radial_highlights(tree, layout),
            TreeLayoutType::Daylight => self.compute_circular_highlights(tree, layout),
            TreeLayoutType::Slanted => self.compute_rectangular_highlights(tree, layout),
        }
    }

    fn compute_rectangular_highlights(
        &self,
        tree: &Tree,
        layout: &TreeLayout,
    ) -> Vec<HighlightShape> {
        let mut highlights = Vec::new();

        // Collect all tip nodes and their vertical positions (sorted)
        let mut tip_positions: Vec<(NodeId, f32)> = tree
            .nodes
            .iter()
            .filter(|node| node.is_leaf())
            .map(|node| (node.id, layout.positions[node.id].1))
            .collect();

        if tip_positions.is_empty() {
            return highlights;
        }

        tip_positions.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(Ordering::Equal));

        let tip_index_map: HashMap<NodeId, usize> = tip_positions
            .iter()
            .enumerate()
            .map(|(idx, (id, _))| (*id, idx))
            .collect();

        let tip_ys: Vec<f32> = tip_positions.iter().map(|(_, y)| *y).collect();

        // Estimate a reasonable fallback gap when no neighbour exists
        let default_gap =
            Self::average_gap(&tip_ys).unwrap_or_else(|| (self.node_radius.max(1.0)) * 4.0);

        for (&highlighted_node_id, &highlight_color) in &self.highlighted_clades {
            let leaf_descendants = self.collect_leaf_descendants(tree, highlighted_node_id);
            if leaf_descendants.is_empty() {
                continue;
            }

            let mut indices: Vec<usize> = leaf_descendants
                .iter()
                .filter_map(|leaf_id| tip_index_map.get(leaf_id).copied())
                .collect();

            if indices.is_empty() {
                continue;
            }

            indices.sort_unstable();
            let min_idx = indices[0];
            let max_idx = *indices.last().unwrap();

            let min_y = tip_ys[min_idx];
            let max_y = tip_ys[max_idx];

            let gap_above = if min_idx > 0 {
                (min_y - tip_ys[min_idx - 1]).abs() * 0.5
            } else {
                default_gap * 0.5
            };

            let gap_below = if max_idx + 1 < tip_ys.len() {
                (tip_ys[max_idx + 1] - max_y).abs() * 0.5
            } else {
                default_gap * 0.5
            };

            let top_y = min_y - gap_above;
            let bottom_y = max_y + gap_below;

            let node_world = layout.positions[highlighted_node_id];
            let branch_mid_x = if let Some(parent_id) = tree.nodes[highlighted_node_id].parent {
                let parent_world = layout.positions[parent_id];
                (parent_world.0 + node_world.0) * 0.5
            } else {
                node_world.0
            };

            let furthest_tip_x = leaf_descendants
                .iter()
                .map(|&tip_id| layout.positions[tip_id].0)
                .fold(branch_mid_x, f32::max);

            let left = branch_mid_x.min(furthest_tip_x);
            let right = branch_mid_x.max(furthest_tip_x);

            highlights.push(HighlightShape::Rect {
                top_left: egui::pos2(left, top_y.min(bottom_y)),
                bottom_right: egui::pos2(right, top_y.max(bottom_y)),
                color: highlight_color,
            });
        }

        highlights
    }

    fn compute_circular_highlights(&self, tree: &Tree, layout: &TreeLayout) -> Vec<HighlightShape> {
        let mut highlights = Vec::new();

        let center_world = if let Some(root_id) = tree.root {
            layout.positions[root_id]
        } else {
            (layout.width * 0.5, layout.height * 0.5)
        };

        let tip_extensions = if self.align_tip_labels {
            self.calculate_tip_extensions(tree)
        } else {
            HashMap::new()
        };

        for (&highlighted_node_id, &highlight_color) in &self.highlighted_clades {
            let leaf_descendants = self.collect_leaf_descendants(tree, highlighted_node_id);
            if leaf_descendants.is_empty() {
                continue;
            }

            let inner_radius = self.calculate_highlight_inner_radius(
                tree,
                layout,
                highlighted_node_id,
                center_world,
            );

            let mut outer_radius = 0.0f32;
            for &tip_id in &leaf_descendants {
                let pos = layout.positions[tip_id];
                let dx = pos.0 - center_world.0;
                let dy = pos.1 - center_world.1;
                let mut radius = (dx * dx + dy * dy).sqrt();
                if let Some(extension) = tip_extensions.get(&tip_id) {
                    radius += *extension as f32;
                }
                outer_radius = outer_radius.max(radius);
            }

            if outer_radius <= inner_radius {
                outer_radius = inner_radius + 1.0;
            }

            if let Some((start_angle, end_angle)) =
                self.calculate_sector_angles(tree, layout, center_world, &leaf_descendants)
            {
                highlights.push(HighlightShape::Sector {
                    center: egui::pos2(center_world.0, center_world.1),
                    inner_radius,
                    outer_radius,
                    start_angle,
                    end_angle,
                    color: highlight_color,
                });
            }
        }

        highlights
    }

    fn compute_radial_highlights(&self, tree: &Tree, layout: &TreeLayout) -> Vec<HighlightShape> {
        let mut highlights = Vec::new();
        let padding = (layout.width.max(layout.height) * 0.015).max(1.5);
        let center = if let Some(root_id) = tree.root {
            layout.positions[root_id]
        } else {
            (layout.width * 0.5, layout.height * 0.5)
        };

        for (&highlighted_node_id, &highlight_color) in &self.highlighted_clades {
            let leaf_descendants = self.collect_leaf_descendants(tree, highlighted_node_id);
            if leaf_descendants.is_empty() {
                continue;
            }

            let node_pos = layout.positions[highlighted_node_id];
            let branch_start = if let Some(parent_id) = tree.nodes[highlighted_node_id].parent {
                let parent_pos = layout.positions[parent_id];
                egui::pos2(
                    (node_pos.0 + parent_pos.0) * 0.5,
                    (node_pos.1 + parent_pos.1) * 0.5,
                )
            } else {
                egui::pos2(node_pos.0, node_pos.1)
            };

            let ordered_tips = self.radial_ordered_leaf_tips(layout, center, &leaf_descendants);
            if ordered_tips.is_empty() {
                continue;
            }

            let mut polygon = Vec::with_capacity(ordered_tips.len() + 1);
            polygon.push(branch_start);
            for tip_id in ordered_tips {
                let p = layout.positions[tip_id];
                polygon.push(egui::pos2(p.0, p.1));
            }

            // Single-tip clade: widen near tip to avoid degenerate triangle.
            if polygon.len() == 2 {
                let a = polygon[0];
                let b = polygon[1];
                let dir = egui::vec2(b.x - a.x, b.y - a.y);
                let len = dir.length().max(1e-6);
                let normal = egui::vec2(-dir.y / len, dir.x / len) * padding;
                polygon = vec![a, b + normal, b - normal];
            }

            if polygon.len() >= 3 {
                highlights.push(HighlightShape::Polygon {
                    points: polygon,
                    color: highlight_color,
                });
            }
        }

        highlights
    }

    fn collect_leaf_descendants(&self, tree: &Tree, node_id: NodeId) -> Vec<NodeId> {
        self.collect_all_descendants(tree, node_id)
            .into_iter()
            .filter(|descendant| tree.nodes[*descendant].is_leaf())
            .collect()
    }

    fn average_gap(values: &[f32]) -> Option<f32> {
        if values.len() < 2 {
            return None;
        }

        let mut total = 0.0f32;
        let mut count = 0usize;

        for pair in values.windows(2) {
            let gap = (pair[1] - pair[0]).abs();
            if gap > f32::EPSILON {
                total += gap;
                count += 1;
            }
        }

        if count > 0 {
            Some(total / count as f32)
        } else {
            None
        }
    }

    fn polygon_from_points(points: &[egui::Pos2], padding: f32) -> Vec<egui::Pos2> {
        let unique_points = Self::dedup_points(points);

        match unique_points.as_slice() {
            [] => Vec::new(),
            [single] => Self::approximate_circle(*single, padding.max(2.0), 16),
            [a, b] => Self::approximate_capsule(*a, *b, padding.max(2.0)),
            _ => {
                let hull = Self::convex_hull(&unique_points);
                if hull.len() < 3 {
                    if hull.len() == 2 {
                        Self::approximate_capsule(hull[0], hull[1], padding.max(2.0))
                    } else {
                        Self::approximate_circle(hull[0], padding.max(2.0), 16)
                    }
                } else {
                    hull
                }
            }
        }
    }

    fn radial_ordered_leaf_tips(
        &self,
        layout: &TreeLayout,
        center: (f32, f32),
        leaf_descendants: &[NodeId],
    ) -> Vec<NodeId> {
        let mut tips: Vec<(NodeId, f32)> = leaf_descendants
            .iter()
            .map(|&id| {
                let pos = layout.positions[id];
                let dx = pos.0 - center.0;
                let dy = pos.1 - center.1;
                let mut angle = dy.atan2(dx);
                if angle < 0.0 {
                    angle += std::f32::consts::TAU;
                }
                (id, angle)
            })
            .collect();

        if tips.is_empty() {
            return Vec::new();
        }

        tips.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(Ordering::Equal));
        if tips.len() <= 2 {
            return tips.into_iter().map(|(id, _)| id).collect();
        }

        // Rotate sequence after largest angular gap to keep contiguous clade boundary.
        let mut cut = 0usize;
        let mut max_gap = -1.0f32;
        for i in 0..tips.len() {
            let a = tips[i].1;
            let b = if i + 1 < tips.len() {
                tips[i + 1].1
            } else {
                tips[0].1 + std::f32::consts::TAU
            };
            let gap = b - a;
            if gap > max_gap {
                max_gap = gap;
                cut = i;
            }
        }

        let start = (cut + 1) % tips.len();
        let mut ordered = Vec::with_capacity(tips.len());
        for k in 0..tips.len() {
            ordered.push(tips[(start + k) % tips.len()].0);
        }
        ordered
    }

    fn approximate_circle(center: egui::Pos2, radius: f32, segments: usize) -> Vec<egui::Pos2> {
        let steps = segments.max(8);
        (0..steps)
            .map(|i| {
                let angle = (i as f32 / steps as f32) * std::f32::consts::TAU;
                egui::pos2(
                    center.x + radius * angle.cos(),
                    center.y + radius * angle.sin(),
                )
            })
            .collect()
    }

    fn approximate_capsule(a: egui::Pos2, b: egui::Pos2, radius: f32) -> Vec<egui::Pos2> {
        let dir = egui::vec2(b.x - a.x, b.y - a.y);
        let length = dir.length();

        if length <= 1e-3 {
            return Self::approximate_circle(a, radius, 16);
        }

        let unit = dir / length;
        let perp = egui::vec2(-unit.y, unit.x) * radius;

        vec![
            egui::pos2(a.x + perp.x, a.y + perp.y),
            egui::pos2(b.x + perp.x, b.y + perp.y),
            egui::pos2(b.x - perp.x, b.y - perp.y),
            egui::pos2(a.x - perp.x, a.y - perp.y),
        ]
    }

    pub fn paint_tree(
        &self,
        painter: &egui::Painter,
        tree: &Tree,
        layout: &TreeLayout,
        selected_nodes: &HashSet<NodeId>,
        selected_tips: &HashSet<NodeId>,
        rect: egui::Rect,
        margin: egui::Vec2,
        selection_mode: Option<crate::tree::viewer::SelectionMode>,
    ) -> Vec<TipLabelHit> {
        let mut tip_label_hits = Vec::new();
        // Paint background
        painter.rect_filled(rect, 10.0, self.background_color);

        let inner = rect.shrink2(margin);
        let inner = if inner.is_positive() { inner } else { rect };
        painter.rect_filled(inner, 8.0, self.canvas_color);

        // Calculate tip extensions for align_tip_labels (needed for other parts of painting)
        let tip_extensions = if self.align_tip_labels {
            self.calculate_tip_extensions(tree)
        } else {
            HashMap::new()
        };

        // Calculate scale_x (needed for scale bar)
        let scale_x = if layout.width <= f32::EPSILON {
            inner.width().max(1.0)
        } else {
            inner.width().max(1.0) / layout.width
        };

        // Use the centralized coordinate transformation function
        let to_screen = self.create_to_screen_transform(tree, layout, inner);

        // Paint highlight rectangles for Rectangular layout
        if matches!(
            layout.layout_type,
            super::layout::TreeLayoutType::Rectangular
        ) {
            // Get all tip y-positions in the entire tree (sorted) - computed once for all highlights
            let mut all_tip_ys: Vec<f32> = tree
                .nodes
                .iter()
                .filter(|node| node.is_leaf())
                .map(|node| layout.positions[node.id].1)
                .collect();
            all_tip_ys.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

            // Draw each highlighted clade
            for (&highlighted_node_id, &highlight_color) in &self.highlighted_clades {
                // Collect all tip nodes in the highlighted clade
                let mut clade_tips = Vec::new();
                let mut to_visit = vec![highlighted_node_id];

                while let Some(node_id) = to_visit.pop() {
                    let node = &tree.nodes[node_id];
                    if node.is_leaf() {
                        clade_tips.push(node_id);
                    } else {
                        to_visit.extend(&node.children);
                    }
                }

                if !clade_tips.is_empty() {
                    // Find the branch midpoint (horizontal position)
                    let highlighted_node = &tree.nodes[highlighted_node_id];
                    let highlighted_node_world = layout.positions[highlighted_node_id];

                    // Calculate horizontal midpoint between node and its parent
                    let branch_midpoint_x = if let Some(parent_id) = highlighted_node.parent {
                        let parent_world = layout.positions[parent_id];
                        (parent_world.0 + highlighted_node_world.0) * 0.5
                    } else {
                        // If root, use the node's own position
                        highlighted_node_world.0
                    };

                    // Find the furthest tip (rightmost position)
                    let furthest_tip_x = clade_tips
                        .iter()
                        .map(|&tip_id| layout.positions[tip_id].0)
                        .fold(f32::NEG_INFINITY, |a, b| a.max(b));

                    // Get y-positions of tips in this clade (sorted)
                    let mut clade_tip_ys: Vec<f32> = clade_tips
                        .iter()
                        .map(|&tip_id| layout.positions[tip_id].1)
                        .collect();
                    clade_tip_ys
                        .sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

                    if let (Some(&min_clade_y), Some(&max_clade_y)) =
                        (clade_tip_ys.first(), clade_tip_ys.last())
                    {
                        // Calculate top edge: halfway to the clade above
                        let top_y = if let Some(&above_y) =
                            all_tip_ys.iter().rev().find(|&&y| y < min_clade_y)
                        {
                            (min_clade_y + above_y) * 0.5
                        } else {
                            // No clade above, extend to the top margin
                            min_clade_y - (all_tip_ys[1] - all_tip_ys[0]) * 0.5
                        };

                        // Calculate bottom edge: halfway to the clade below
                        let bottom_y =
                            if let Some(&below_y) = all_tip_ys.iter().find(|&&y| y > max_clade_y) {
                                (max_clade_y + below_y) * 0.5
                            } else {
                                // No clade below, extend to the bottom margin
                                let n = all_tip_ys.len();
                                max_clade_y + (all_tip_ys[n - 1] - all_tip_ys[n - 2]) * 0.5
                            };

                        // Convert world coordinates to screen coordinates
                        let top_left_screen = to_screen((branch_midpoint_x, top_y));
                        let bottom_right_screen = to_screen((furthest_tip_x, bottom_y));

                        // Draw the filled rectangle
                        let highlight_rect =
                            egui::Rect::from_two_pos(top_left_screen, bottom_right_screen);
                        painter.rect_filled(highlight_rect, 0.0, highlight_color);
                    }
                }
            }
        }

        // Paint highlight bubbles for Radial layout
        if matches!(layout.layout_type, super::layout::TreeLayoutType::Radial) {
            for (&highlighted_node_id, &highlight_color) in &self.highlighted_clades {
                let descendants = self.collect_all_descendants(tree, highlighted_node_id);
                let leaf_descendants: Vec<NodeId> = descendants
                    .into_iter()
                    .filter(|&id| tree.nodes[id].is_leaf())
                    .collect();

                if leaf_descendants.is_empty() {
                    continue;
                }

                let mut highlight_points: Vec<egui::Pos2> = leaf_descendants
                    .iter()
                    .map(|&tip_id| to_screen(layout.positions[tip_id]))
                    .collect();

                let node_screen = to_screen(layout.positions[highlighted_node_id]);
                highlight_points.push(node_screen);

                if let Some(parent_id) = tree.nodes[highlighted_node_id].parent {
                    let parent_screen = to_screen(layout.positions[parent_id]);
                    let direction = node_screen - parent_screen;
                    let length = direction.length().max(1.0);
                    let offset = direction / length * (length * 0.2 + 24.0);
                    highlight_points.push(node_screen + offset);
                }

                self.draw_radial_highlight_polygon(painter, &highlight_points, highlight_color);
            }
        }

        // Paint highlight sectors for Circular layout - 重写版本
        if matches!(layout.layout_type, super::layout::TreeLayoutType::Circular) {
            // 1. 基础设置：使用根节点位置作为中心（进化树形状的真实中心）
            let center_world = if let Some(root_id) = tree.root {
                layout.positions[root_id]
            } else {
                // 如果没有根节点，使用布局的几何中心
                (layout.width * 0.5, layout.height * 0.5)
            };
            let center_screen = to_screen(center_world);

            // 计算从world到screen的缩放因子（与to_screen保持一致）
            let scale = {
                let label_margin = if self.align_tip_labels && !tip_extensions.is_empty() {
                    self.tip_label_font_size * 8.0
                } else {
                    0.0
                };
                let available_radius = inner.width().min(inner.height()) * 0.45 - label_margin;

                // 计算layout的真实最大半径（从根节点中心到所有节点的最大距离）
                let layout_max_radius = {
                    let mut max_radius = 0.0f32;
                    for node in &tree.nodes {
                        let pos = layout.positions[node.id];
                        let dx = pos.0 - center_world.0;
                        let dy = pos.1 - center_world.1;
                        let radius = (dx * dx + dy * dy).sqrt();
                        max_radius = max_radius.max(radius);
                    }
                    if self.align_tip_labels && !tip_extensions.is_empty() {
                        let max_extension =
                            tip_extensions.values().fold(0.0f64, |a, &b| a.max(b)) as f32;
                        max_radius + max_extension
                    } else {
                        max_radius
                    }
                }
                .max(1e-6);

                available_radius / layout_max_radius
            };

            // 2. 遍历每个需要高亮的clade
            for (&highlighted_node_id, &highlight_color) in &self.highlighted_clades {
                // 3. 收集该clade的所有叶节点（后代）
                let descendants = self.collect_all_descendants(tree, highlighted_node_id);
                let leaf_descendants: Vec<NodeId> = descendants
                    .into_iter()
                    .filter(|&id| tree.nodes[id].is_leaf())
                    .collect();

                if leaf_descendants.is_empty() {
                    continue; // 没有叶节点，跳过
                }

                // 4. 计算内层半径（highlighted节点到其父节点的中点距离）
                let inner_radius_world = self.calculate_highlight_inner_radius(
                    tree,
                    layout,
                    highlighted_node_id,
                    center_world,
                );

                // 5. 计算外层半径（最远叶节点的距离）
                let outer_radius_world = leaf_descendants
                    .iter()
                    .map(|&tip_id| {
                        let pos = layout.positions[tip_id];
                        let dx = pos.0 - center_world.0;
                        let dy = pos.1 - center_world.1;
                        (dx * dx + dy * dy).sqrt()
                    })
                    .fold(f32::NEG_INFINITY, |a, b| a.max(b));

                if outer_radius_world <= 0.0 {
                    continue;
                }

                if let Some((start_angle, end_angle)) =
                    self.calculate_sector_angles(tree, layout, center_world, &leaf_descendants)
                {
                    let inner_radius_screen = inner_radius_world * scale;
                    let outer_radius_screen = outer_radius_world * scale;

                    self.draw_highlight_sector(
                        painter,
                        center_screen,
                        inner_radius_screen,
                        outer_radius_screen,
                        start_angle,
                        end_angle,
                        highlight_color,
                    );
                }
            }
        }

        // Paint branches
        match layout.layout_type {
            super::layout::TreeLayoutType::Rectangular => {
                for (parent, child) in &layout.edges {
                    let parent_world = layout.positions[*parent];
                    let child_world = layout.positions[*child];
                    let bend_world = (parent_world.0, child_world.1);

                    let parent_screen = to_screen(parent_world);
                    let bend_screen = to_screen(bend_world);
                    let child_screen = to_screen(child_world);

                    let override_color = self.branch_color_overrides.get(child).copied();
                    let is_selected = selected_nodes.contains(child);
                    self.draw_branch(
                        painter,
                        vec![parent_screen, bend_screen, child_screen],
                        override_color,
                        is_selected,
                        selection_mode,
                    );
                }
            }
            super::layout::TreeLayoutType::Circular => {
                // 使用连续分支绘制圆形布局
                if !layout.continuous_branches.is_empty() {
                    // 优先使用连续分支绘制
                    for branch in &layout.continuous_branches {
                        let screen_points: Vec<egui::Pos2> = branch
                            .points
                            .iter()
                            .map(|&point| to_screen(point))
                            .collect();

                        let override_color =
                            self.branch_color_overrides.get(&branch.child).copied();
                        let is_selected = selected_nodes.contains(&branch.child);
                        self.draw_branch(
                            painter,
                            screen_points,
                            override_color,
                            is_selected,
                            selection_mode,
                        );
                    }
                } else {
                    // 回退到分段绘制（向后兼容）
                    for arc_segment in &layout.arc_segments {
                        self.draw_arc(
                            painter,
                            arc_segment,
                            &to_screen,
                            inner,
                            selected_nodes,
                            selection_mode,
                        );
                    }

                    for segment in &layout.rect_segments {
                        let start_screen = to_screen(segment.start);
                        let end_screen = to_screen(segment.end);

                        let override_color = segment
                            .child
                            .and_then(|child| self.branch_color_overrides.get(&child).copied());
                        let is_selected = segment
                            .child
                            .map_or(false, |child| selected_nodes.contains(&child));
                        self.draw_branch(
                            painter,
                            vec![start_screen, end_screen],
                            override_color,
                            is_selected,
                            selection_mode,
                        );
                    }
                }
            }
            super::layout::TreeLayoutType::Radial | super::layout::TreeLayoutType::Daylight => {
                for (parent, child) in &layout.edges {
                    let parent_world = layout.positions[*parent];
                    let child_world = layout.positions[*child];
                    let parent_screen = to_screen(parent_world);
                    let child_screen = to_screen(child_world);

                    let override_color = self.branch_color_overrides.get(child).copied();
                    let is_selected = selected_nodes.contains(child);
                    self.draw_branch(
                        painter,
                        vec![parent_screen, child_screen],
                        override_color,
                        is_selected,
                        selection_mode,
                    );
                }
            }
            _ => {
                for (parent, child) in &layout.edges {
                    let parent_world = layout.positions[*parent];
                    let child_world = layout.positions[*child];
                    let parent_screen = to_screen(parent_world);
                    let child_screen = to_screen(child_world);

                    let override_color = self.branch_color_overrides.get(child).copied();
                    let is_selected = selected_nodes.contains(child);
                    self.draw_branch(
                        painter,
                        vec![parent_screen, child_screen],
                        override_color,
                        is_selected,
                        selection_mode,
                    );
                }
            }
        }

        if self.show_node_bars {
            if let Some(field) = self.node_bar_field.as_deref() {
                self.paint_node_bars(painter, tree, layout, &to_screen, field);
            }
        }

        // Paint tip extensions (dashed lines) if align_tip_labels is enabled
        if self.align_tip_labels && !tip_extensions.is_empty() {
            let dashed_stroke =
                Stroke::new(self.branch_stroke.width * 0.8, Color32::from_gray(150));

            for (tip_id, &extension) in &tip_extensions {
                if extension < 1e-6 {
                    continue;
                }

                let tip_world = layout.positions[*tip_id];
                let tip_screen = to_screen(tip_world);

                // Calculate extended position based on layout type
                let extended_pos = match layout.layout_type {
                    super::layout::TreeLayoutType::Rectangular
                    | super::layout::TreeLayoutType::Slanted
                    | super::layout::TreeLayoutType::Cladogram
                    | super::layout::TreeLayoutType::Phylogram => {
                        // For rectangular-like layouts, extend horizontally
                        let extended_world = (tip_world.0 + extension as f32, tip_world.1);
                        to_screen(extended_world)
                    }
                    super::layout::TreeLayoutType::Circular => {
                        // For circular layout, extend along the straight branch direction (shoulder to tip)
                        // Find the continuous branch for this tip
                        let direction = if let Some(branch) = layout
                            .continuous_branches
                            .iter()
                            .find(|b| b.child == *tip_id)
                        {
                            if branch.points.len() >= 2 {
                                // branch.points structure: [child, shoulder, ...arc points..., parent]
                                let child_pos = branch.points[0];
                                let shoulder_pos = branch.points[1];
                                let dx = child_pos.0 - shoulder_pos.0;
                                let dy = child_pos.1 - shoulder_pos.1;
                                let distance = (dx * dx + dy * dy).sqrt().max(1e-6);
                                (dx / distance, dy / distance)
                            } else {
                                // Fallback: use radial direction from center
                                let center_x = layout.width * 0.5;
                                let center_y = layout.height * 0.5;
                                let dx = tip_world.0 - center_x;
                                let dy = tip_world.1 - center_y;
                                let distance = (dx * dx + dy * dy).sqrt().max(1e-6);
                                (dx / distance, dy / distance)
                            }
                        } else {
                            // Fallback: use radial direction from center
                            let center_x = layout.width * 0.5;
                            let center_y = layout.height * 0.5;
                            let dx = tip_world.0 - center_x;
                            let dy = tip_world.1 - center_y;
                            let distance = (dx * dx + dy * dy).sqrt().max(1e-6);
                            (dx / distance, dy / distance)
                        };

                        let extended_world = (
                            tip_world.0 + direction.0 * extension as f32,
                            tip_world.1 + direction.1 * extension as f32,
                        );
                        to_screen(extended_world)
                    }
                    super::layout::TreeLayoutType::Radial
                    | super::layout::TreeLayoutType::Daylight => {
                        // For radial layouts, extend radially from center
                        let center_x = layout.width * 0.5;
                        let center_y = layout.height * 0.5;
                        let dx = tip_world.0 - center_x;
                        let dy = tip_world.1 - center_y;
                        let distance = (dx * dx + dy * dy).sqrt().max(1e-6);
                        let extended_world = (
                            tip_world.0 + dx / distance * extension as f32,
                            tip_world.1 + dy / distance * extension as f32,
                        );
                        to_screen(extended_world)
                    }
                };

                // Draw dashed line
                self.draw_dashed_line(painter, tip_screen, extended_pos, dashed_stroke);
            }
        }

        // Paint nodes
        for node in &tree.nodes {
            let pos = to_screen(layout.positions[node.id]);

            let is_selected = selected_nodes.contains(&node.id) || selected_tips.contains(&node.id);
            let color = if is_selected {
                self.selected_color
            } else if node.is_leaf() {
                self.leaf_color
            } else {
                self.internal_node_color
            };

            // Only draw node shapes if the corresponding option is enabled
            let should_draw = if node.is_leaf() {
                self.show_tip_shapes
            } else {
                self.show_node_shapes
            };

            if should_draw {
                painter.circle_filled(pos, self.node_radius, color);
            }

            // Paint labels
            if node.is_leaf() && self.show_tip_labels {
                if let Some(label) = self.tip_label_text(tree, node) {
                    let tip_selected = selected_tips.contains(&node.id);
                    let text_color = self
                        .tip_label_color_overrides
                        .get(&node.id)
                        .copied()
                        .unwrap_or(self.label_color);

                    // Calculate label position (extended if align_tip_labels is enabled)
                    let label_pos = if self.align_tip_labels {
                        if let Some(&extension) = tip_extensions.get(&node.id) {
                            let tip_world = layout.positions[node.id];
                            let extended_world = match layout.layout_type {
                                super::layout::TreeLayoutType::Rectangular
                                | super::layout::TreeLayoutType::Slanted
                                | super::layout::TreeLayoutType::Cladogram
                                | super::layout::TreeLayoutType::Phylogram => {
                                    (tip_world.0 + extension as f32, tip_world.1)
                                }
                                super::layout::TreeLayoutType::Circular => {
                                    // For circular layout, extend along the straight branch direction (shoulder to tip)
                                    let direction = if let Some(branch) = layout
                                        .continuous_branches
                                        .iter()
                                        .find(|b| b.child == node.id)
                                    {
                                        if branch.points.len() >= 2 {
                                            let child_pos = branch.points[0];
                                            let shoulder_pos = branch.points[1];
                                            let dx = child_pos.0 - shoulder_pos.0;
                                            let dy = child_pos.1 - shoulder_pos.1;
                                            let distance = (dx * dx + dy * dy).sqrt().max(1e-6);
                                            (dx / distance, dy / distance)
                                        } else {
                                            // Fallback: use radial direction from center
                                            let center_x = layout.width * 0.5;
                                            let center_y = layout.height * 0.5;
                                            let dx = tip_world.0 - center_x;
                                            let dy = tip_world.1 - center_y;
                                            let distance = (dx * dx + dy * dy).sqrt().max(1e-6);
                                            (dx / distance, dy / distance)
                                        }
                                    } else {
                                        // Fallback: use radial direction from center
                                        let center_x = layout.width * 0.5;
                                        let center_y = layout.height * 0.5;
                                        let dx = tip_world.0 - center_x;
                                        let dy = tip_world.1 - center_y;
                                        let distance = (dx * dx + dy * dy).sqrt().max(1e-6);
                                        (dx / distance, dy / distance)
                                    };

                                    (
                                        tip_world.0 + direction.0 * extension as f32,
                                        tip_world.1 + direction.1 * extension as f32,
                                    )
                                }
                                super::layout::TreeLayoutType::Radial
                                | super::layout::TreeLayoutType::Daylight => {
                                    let center_x = layout.width * 0.5;
                                    let center_y = layout.height * 0.5;
                                    let dx = tip_world.0 - center_x;
                                    let dy = tip_world.1 - center_y;
                                    let distance = (dx * dx + dy * dy).sqrt().max(1e-6);
                                    (
                                        tip_world.0 + dx / distance * extension as f32,
                                        tip_world.1 + dy / distance * extension as f32,
                                    )
                                }
                            };
                            to_screen(extended_world)
                        } else {
                            pos
                        }
                    } else {
                        pos
                    };

                    if let Some(label_info) = self.paint_tip_label(
                        painter,
                        tree,
                        node,
                        &label,
                        label_pos,
                        layout,
                        text_color,
                        tip_selected,
                        &to_screen,
                    ) {
                        tip_label_hits.push(TipLabelHit {
                            node_id: node.id,
                            rect: label_info.rect,
                            rotation_angle: label_info.rotation_angle,
                            anchor: label_info.anchor,
                            anchor_pos: label_info.anchor_pos,
                            text_size: label_info.text_size,
                        });
                    }
                }
            } else if self.show_node_labels {
                if let Some(label) = &node.name {
                    let text_pos = pos + egui::vec2(8.0, 0.0);
                    painter.text(
                        text_pos,
                        egui::Align2::LEFT_CENTER,
                        label,
                        egui::FontId::proportional(10.0),
                        self.label_color,
                    );
                }
            }

            // Paint branch lengths
            if self.show_branch_labels {
                if let (Some(length), Some(parent_id)) = (node.length, node.parent) {
                    let parent_world = layout.positions[parent_id];
                    let parent_screen = to_screen(parent_world);
                    let child_screen = pos;
                    let mid_x = (parent_screen.x + child_screen.x) * 0.5;
                    let mid_y = match layout.layout_type {
                        super::layout::TreeLayoutType::Rectangular
                        | super::layout::TreeLayoutType::Phylogram => child_screen.y - 12.0,
                        _ => (parent_screen.y + child_screen.y) * 0.5 - 12.0,
                    };
                    let midpoint = egui::pos2(mid_x, mid_y);
                    painter.text(
                        midpoint,
                        egui::Align2::CENTER_TOP,
                        format!("{length:.3}"),
                        egui::FontId::proportional(10.0),
                        self.branch_label_color,
                    );
                }
            }
        }

        // Paint scale bar if applicable
        if self.show_scale_bar && layout.width > f32::EPSILON {
            self.paint_scale_bar(painter, inner, scale_x, layout.width);
        }

        tip_label_hits
    }

    fn paint_node_bars<F>(
        &self,
        painter: &egui::Painter,
        tree: &Tree,
        layout: &TreeLayout,
        to_screen: &F,
        field: &str,
    ) where
        F: Fn((f32, f32)) -> egui::Pos2,
    {
        use crate::tree::layout::TreeLayoutType;

        match layout.layout_type {
            TreeLayoutType::Rectangular
            | TreeLayoutType::Phylogram
            | TreeLayoutType::Slanted
            | TreeLayoutType::Daylight => {
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

                    let y_world = layout.positions[node.id].1;
                    let start = to_screen((min as f32, y_world));
                    let end = to_screen((max as f32, y_world));

                    let mut left = start.x.min(end.x);
                    let mut right = start.x.max(end.x);

                    if (right - left).abs() < 0.5 {
                        let center = (left + right) * 0.5;
                        left = center - 0.5;
                        right = center + 0.5;
                    }

                    let y_center = (start.y + end.y) * 0.5;
                    let half = self.node_bar_thickness.max(0.5) * 0.5;
                    let rect = egui::Rect::from_min_max(
                        egui::pos2(left, y_center - half),
                        egui::pos2(right, y_center + half),
                    );

                    painter.rect_filled(rect, 2.0, self.node_bar_color);
                }
            }
            _ => {}
        }
    }

    fn draw_branch(
        &self,
        painter: &egui::Painter,
        points: Vec<egui::Pos2>,
        override_color: Option<Color32>,
        is_selected: bool,
        selection_mode: Option<crate::tree::viewer::SelectionMode>,
    ) {
        let base_width = self.branch_stroke.width;
        let base_color = override_color.unwrap_or(self.branch_stroke.color);

        // Only highlight branches if not in Taxa mode
        let should_highlight = is_selected
            && !matches!(
                selection_mode,
                Some(crate::tree::viewer::SelectionMode::Taxa)
            );

        if should_highlight {
            let highlight_width = self.branch_highlight_stroke.width.max(base_width * 3.0);
            let highlight_stroke = Stroke::new(highlight_width, self.tip_selection_color);
            painter.add(egui::Shape::line(points.clone(), highlight_stroke));
        }

        let base_stroke = Stroke::new(base_width, base_color);
        painter.add(egui::Shape::line(points, base_stroke));
    }

    /// Draw a filled sector (circular segment) for highlighting clades in Circular layout
    fn draw_filled_sector(
        &self,
        painter: &egui::Painter,
        center: egui::Pos2,
        inner_radius: f32,
        outer_radius: f32,
        start_angle: f32,
        end_angle: f32,
        color: Color32,
    ) {
        // Normalize角度，确保end在start之后，便于统一处理跨零的扇区
        let start_angle = start_angle.rem_euclid(std::f32::consts::TAU);
        let mut end_angle = end_angle;
        while end_angle <= start_angle {
            end_angle += std::f32::consts::TAU;
        }

        // Number of segments to approximate the arc
        // Use more segments for larger radii and larger angular spans to ensure smooth curves
        let angular_span = end_angle - start_angle;
        let avg_radius = (inner_radius + outer_radius) * 0.5;
        // Aim for ~2 pixels per segment at the average radius for smooth rendering
        let segment_count = ((angular_span * avg_radius / 2.0)
            .max(angular_span * 30.0 / std::f32::consts::PI))
        .max(12.0) as usize;
        let angle_step = (end_angle - start_angle) / segment_count as f32;

        // Sample points along the outer and inner arcs（保留相同的角度顺序，便于后续构建四边形带）
        let mut outer_points = Vec::with_capacity(segment_count + 1);
        let mut inner_points = Vec::with_capacity(segment_count + 1);

        for i in 0..=segment_count {
            let angle = start_angle + angle_step * i as f32;
            let outer = egui::pos2(
                center.x + outer_radius * angle.cos(),
                center.y + outer_radius * angle.sin(),
            );
            let inner = egui::pos2(
                center.x + inner_radius * angle.cos(),
                center.y + inner_radius * angle.sin(),
            );
            outer_points.push(outer);
            inner_points.push(inner);
        }

        // 手动构建网格以避免凸多边形填充产生的额外三角区
        let mut mesh = egui::Mesh::default();

        let mut outer_indices = Vec::with_capacity(outer_points.len());
        for point in &outer_points {
            let idx = mesh.vertices.len() as u32;
            mesh.vertices.push(egui::epaint::Vertex {
                pos: *point,
                uv: egui::pos2(0.0, 0.0),
                color,
            });
            outer_indices.push(idx);
        }

        let mut inner_indices = Vec::with_capacity(inner_points.len());
        for point in inner_points.iter().rev() {
            // 反向写入，保证最终索引构成的三角形法向一致
            let idx = mesh.vertices.len() as u32;
            mesh.vertices.push(egui::epaint::Vertex {
                pos: *point,
                uv: egui::pos2(0.0, 0.0),
                color,
            });
            inner_indices.push(idx);
        }
        inner_indices.reverse();

        for i in 0..segment_count {
            let outer_start = outer_indices[i];
            let outer_end = outer_indices[i + 1];
            let inner_start = inner_indices[i];
            let inner_end = inner_indices[i + 1];

            // Triangle 1
            mesh.indices.push(outer_start);
            mesh.indices.push(outer_end);
            mesh.indices.push(inner_end);

            // Triangle 2
            mesh.indices.push(outer_start);
            mesh.indices.push(inner_end);
            mesh.indices.push(inner_start);
        }

        painter.add(egui::Shape::mesh(mesh));
    }

    fn draw_arc<F>(
        &self,
        painter: &egui::Painter,
        arc_segment: &crate::tree::layout::ArcSegment,
        to_screen: &F,
        _inner: egui::Rect,
        selected_nodes: &HashSet<NodeId>,
        selection_mode: Option<crate::tree::viewer::SelectionMode>,
    ) where
        F: Fn((f32, f32)) -> egui::Pos2,
    {
        // 转换中心点 - 现在center已经是正确的布局坐标中心
        let center_screen = to_screen(arc_segment.center);

        // 计算半径的缩放
        // 由于to_screen函数已经处理了所有的缩放，我们只需要从center计算相对距离
        // 使用极坐标位置来计算实际的屏幕半径
        let point_on_arc = (
            arc_segment.center.0 + arc_segment.radius * arc_segment.start_angle.cos(),
            arc_segment.center.1 + arc_segment.radius * arc_segment.start_angle.sin(),
        );
        let point_screen = to_screen(point_on_arc);

        // 计算屏幕空间中的半径
        let dx = point_screen.x - center_screen.x;
        let dy = point_screen.y - center_screen.y;
        let radius = (dx * dx + dy * dy).sqrt();

        // 检查是否被选中
        let is_selected = selected_nodes.contains(&arc_segment.child);
        let override_color = self.branch_color_overrides.get(&arc_segment.child).copied();
        let base_color = override_color.unwrap_or(self.branch_stroke.color);

        // 绘制圆弧
        let start_angle = arc_segment.start_angle;
        let end_angle = arc_segment.end_angle;

        // 使用多个小线段来近似圆弧
        let segment_count = 20; // 圆弧分段数
        let angle_step = (end_angle - start_angle) / segment_count as f32;

        let mut arc_points = Vec::with_capacity(segment_count + 1);

        for i in 0..=segment_count {
            let angle = start_angle + angle_step * i as f32;
            let x = center_screen.x + radius * angle.cos();
            let y = center_screen.y + radius * angle.sin();
            arc_points.push(egui::pos2(x, y));
        }

        // 绘制圆弧
        // Only highlight branches if not in Taxa mode
        let should_highlight = is_selected
            && !matches!(
                selection_mode,
                Some(crate::tree::viewer::SelectionMode::Taxa)
            );

        if should_highlight {
            let highlight_width = self
                .branch_highlight_stroke
                .width
                .max(self.branch_stroke.width * 3.0);
            let highlight_stroke = Stroke::new(highlight_width, self.tip_selection_color);
            painter.add(egui::Shape::line(arc_points.clone(), highlight_stroke));
        }

        let base_stroke = Stroke::new(self.branch_stroke.width, base_color);
        painter.add(egui::Shape::line(arc_points, base_stroke));
    }

    pub fn tip_label_text(&self, _tree: &Tree, node: &TreeNode) -> Option<String> {
        match self.tip_label_display {
            TipLabelDisplay::Names => node.name.clone(),
            TipLabelDisplay::Labels => node.label.clone().or_else(|| node.name.clone()),
            TipLabelDisplay::BranchLength => node.length.map(|value| self.format_numeric(value)),
        }
    }

    fn format_numeric(&self, value: f64) -> String {
        match self.tip_label_format {
            TipLabelNumberFormat::Decimal => {
                let precision = self.tip_label_precision;
                format!("{value:.precision$}")
            }
            TipLabelNumberFormat::Scientific => {
                let precision = self.tip_label_precision;
                format!("{value:.precision$e}")
            }
            TipLabelNumberFormat::Percentage => {
                let percent = value * 100.0;
                let precision = self.tip_label_precision;
                format!("{percent:.precision$}%")
            }
        }
    }

    fn paint_scale_bar(
        &self,
        painter: &egui::Painter,
        inner: egui::Rect,
        scale_x: f32,
        tree_width: f32,
    ) {
        if let Some(tick) = self.nice_tick_span(tree_width) {
            let bar_pixels = tick * scale_x;
            let baseline = inner.bottom() - 18.0;
            let start = egui::pos2(inner.left() + 32.0, baseline);
            let end = egui::pos2(start.x + bar_pixels, baseline);

            let stroke = Stroke::new(2.0, Color32::from_rgb(210, 215, 220));
            painter.line_segment([start, end], stroke);
            painter.line_segment([start, egui::pos2(start.x, baseline - 8.0)], stroke);
            painter.line_segment([end, egui::pos2(end.x, baseline - 8.0)], stroke);

            painter.text(
                egui::pos2((start.x + end.x) * 0.5, baseline - 12.0),
                egui::Align2::CENTER_BOTTOM,
                format!("Scale: {tick:.3}"),
                egui::FontId::proportional(11.0),
                Color32::from_rgb(220, 225, 230),
            );
        }
    }

    fn nice_tick_span(&self, total: f32) -> Option<f32> {
        if total <= f32::EPSILON {
            return None;
        }
        let raw = total / 5.0;
        let exponent = raw.abs().log10().floor();
        let base = 10_f32.powf(exponent);
        for multiplier in [1.0, 2.0, 5.0, 10.0] {
            let candidate = multiplier * base;
            if candidate >= raw {
                return Some(candidate);
            }
        }
        Some(10.0 * base)
    }

    pub fn set_branch_stroke_width(&mut self, width: f32) {
        self.branch_stroke = Stroke::new(width, self.branch_stroke.color);
        self.branch_highlight_stroke = Stroke::new(width + 0.7, self.branch_highlight_stroke.color);
    }

    pub fn set_node_radius(&mut self, radius: f32) {
        self.node_radius = radius;
    }

    fn paint_tip_label<F>(
        &self,
        painter: &egui::Painter,
        tree: &Tree,
        node: &TreeNode,
        label: &str,
        node_pos: egui::Pos2,
        layout: &TreeLayout,
        text_color: Color32,
        is_selected: bool,
        to_screen: &F,
    ) -> Option<TipLabelInfo>
    where
        F: Fn((f32, f32)) -> egui::Pos2,
    {
        let font_id = FontId::new(
            self.tip_label_font_size,
            self.tip_label_font_family.into_font_family(),
        );

        // 根据布局类型选择不同的标签绘制方式
        match layout.layout_type {
            super::layout::TreeLayoutType::Circular => {
                // Circular布局使用从中心到Tip的方向
                Some(self.paint_circular_tip_label(
                    painter,
                    node,
                    label,
                    node_pos,
                    layout,
                    text_color,
                    is_selected,
                    font_id,
                    to_screen,
                ))
            }
            super::layout::TreeLayoutType::Radial | super::layout::TreeLayoutType::Daylight => {
                // Radial和Daylight布局使用从Internal Node到Tip的方向
                Some(self.paint_radial_tip_label(
                    painter,
                    tree,
                    node,
                    label,
                    node_pos,
                    layout,
                    text_color,
                    is_selected,
                    font_id,
                    to_screen,
                ))
            }
            _ => {
                // 其他布局使用标准的右侧标签
                let rect = self.paint_right_side_tip_label(
                    painter,
                    label,
                    node_pos,
                    text_color,
                    is_selected,
                    font_id.clone(),
                );
                // 获取文本尺寸
                let galley = painter.layout_no_wrap(label.to_string(), font_id, text_color);
                let text_size = galley.size();
                let label_pos = node_pos + egui::vec2(8.0, 0.0); // 右侧标签的位置

                Some(TipLabelInfo {
                    rect,
                    rotation_angle: 0.0,
                    anchor: egui::Align2::LEFT_CENTER,
                    anchor_pos: label_pos,
                    text_size,
                })
            }
        }
    }

    fn paint_right_side_tip_label(
        &self,
        painter: &egui::Painter,
        label: &str,
        node_pos: egui::Pos2,
        text_color: Color32,
        is_selected: bool,
        font_id: FontId,
    ) -> egui::Rect {
        let text_pos = node_pos + egui::vec2(8.0, 0.0);

        if is_selected {
            let galley = painter.layout_no_wrap(label.to_string(), font_id.clone(), text_color);
            let size = galley.size();
            let mut rect =
                egui::Rect::from_min_size(text_pos + egui::vec2(0.0, -size.y * 0.5), size);
            rect = rect.expand2(egui::Vec2::splat(2.0));
            painter.rect_filled(rect, 3.0, self.tip_selection_color);
        }

        let galley = painter.layout_no_wrap(label.to_string(), font_id.clone(), text_color);
        let text_rect = egui::Rect::from_min_size(
            text_pos + egui::vec2(0.0, -galley.size().y * 0.5),
            galley.size(),
        );

        painter.text(
            text_pos,
            egui::Align2::LEFT_CENTER,
            label,
            font_id,
            text_color,
        );

        text_rect
    }

    fn paint_circular_tip_label<F>(
        &self,
        painter: &egui::Painter,
        node: &TreeNode,
        label: &str,
        node_pos: egui::Pos2,
        layout: &TreeLayout,
        text_color: Color32,
        is_selected: bool,
        font_id: FontId,
        to_screen: &F,
    ) -> TipLabelInfo
    where
        F: Fn((f32, f32)) -> egui::Pos2,
    {
        // Circular布局：使用从shoulder点（弧线和直线的拐角点）到Tip的方向

        // 1. 计算分支方向角度（在屏幕坐标系中计算，确保与hitbox一致）
        let straight_line_angle = if let Some(_parent_id) = node.parent {
            // 查找对应的continuous branch来获取shoulder点
            if let Some(branch) = layout
                .continuous_branches
                .iter()
                .find(|b| b.child == node.id)
            {
                // branch.points的结构是: [child, shoulder, ...arc points..., parent]
                // 第二个点就是shoulder点（拐角点）
                if branch.points.len() >= 2 {
                    let child_world_pos = branch.points[0]; // Tip节点位置
                    let shoulder_world_pos = branch.points[1]; // 拐角点位置

                    // 转换到屏幕坐标系中计算角度
                    let child_screen = to_screen(child_world_pos);
                    let shoulder_screen = to_screen(shoulder_world_pos);

                    let dx = child_screen.x - shoulder_screen.x;
                    let dy = child_screen.y - shoulder_screen.y;
                    dy.atan2(dx)
                } else {
                    // fallback: 如果找不到shoulder点，使用屏幕坐标系中的父节点到子节点方向
                    let parent_screen = to_screen(layout.positions[_parent_id]);
                    let child_screen = to_screen(layout.positions[node.id]);

                    let dx = child_screen.x - parent_screen.x;
                    let dy = child_screen.y - parent_screen.y;
                    dy.atan2(dx)
                }
            } else {
                // fallback: 如果找不到对应的branch，使用屏幕坐标系中的父节点到子节点方向
                let parent_screen = to_screen(layout.positions[_parent_id]);
                let child_screen = to_screen(layout.positions[node.id]);

                let dx = child_screen.x - parent_screen.x;
                let dy = child_screen.y - parent_screen.y;
                dy.atan2(dx)
            }
        } else {
            // 根节点：使用从布局中心到节点的方向（在屏幕坐标系中）
            let layout_center = (layout.width * 0.5, layout.height * 0.5);
            let center_screen = to_screen(layout_center);
            let node_screen = to_screen(layout.positions[node.id]);

            let dx = node_screen.x - center_screen.x;
            let dy = node_screen.y - center_screen.y;
            dy.atan2(dx)
        };

        // 2. 标签位置计算
        let label_offset_distance = 12.0; // Circular布局的间距
        let direction_x = straight_line_angle.cos();
        let direction_y = straight_line_angle.sin();
        let label_offset = egui::vec2(
            label_offset_distance * direction_x,
            label_offset_distance * direction_y,
        );
        let label_pos = node_pos + label_offset;

        // 3. 文字旋转角度处理（与Radial相同的翻转逻辑）
        let angle_deg = straight_line_angle.to_degrees();
        let normalized_angle_deg = if angle_deg < 0.0 {
            angle_deg + 360.0
        } else {
            angle_deg
        };

        let (text_rotation_angle, text_anchor) =
            if normalized_angle_deg > 90.0 && normalized_angle_deg < 270.0 {
                // 文字上下颠倒了，需要翻转180度
                (
                    straight_line_angle + std::f32::consts::PI,
                    egui::Align2::RIGHT_CENTER,
                )
            } else {
                // 文字正常可读
                (straight_line_angle, egui::Align2::LEFT_CENTER)
            };

        // 4. 获取文本尺寸
        let galley = painter.layout_no_wrap(label.to_string(), font_id.clone(), text_color);
        let text_size = galley.size();

        // 5. 绘制旋转文字
        let text_rect = painter.rotated_text(
            label_pos,
            text_anchor,
            label,
            font_id.clone(),
            text_color,
            text_rotation_angle,
        );

        // 6. 绘制选择背景（如果节点被选中）
        if is_selected {
            let expanded_rect = text_rect.expand(2.0);
            let angle_tolerance = 0.1;

            if text_rotation_angle.abs() < angle_tolerance
                || (text_rotation_angle - std::f32::consts::PI).abs() < angle_tolerance
                || (text_rotation_angle + std::f32::consts::PI).abs() < angle_tolerance
            {
                painter.rect_filled(expanded_rect, 3.0, self.tip_selection_color);
            } else {
                let galley = painter.layout_no_wrap(label.to_string(), font_id, text_color);
                let text_size = galley.size();

                let corners = match text_anchor {
                    egui::Align2::LEFT_CENTER => [
                        egui::vec2(0.0, -text_size.y / 2.0),
                        egui::vec2(text_size.x, -text_size.y / 2.0),
                        egui::vec2(text_size.x, text_size.y / 2.0),
                        egui::vec2(0.0, text_size.y / 2.0),
                    ],
                    egui::Align2::RIGHT_CENTER => [
                        egui::vec2(-text_size.x, -text_size.y / 2.0),
                        egui::vec2(0.0, -text_size.y / 2.0),
                        egui::vec2(0.0, text_size.y / 2.0),
                        egui::vec2(-text_size.x, text_size.y / 2.0),
                    ],
                    _ => [
                        egui::vec2(0.0, -text_size.y / 2.0),
                        egui::vec2(text_size.x, -text_size.y / 2.0),
                        egui::vec2(text_size.x, text_size.y / 2.0),
                        egui::vec2(0.0, text_size.y / 2.0),
                    ],
                };

                let expand = 2.0;
                let expanded_corners = [
                    corners[0] + egui::vec2(-expand, -expand),
                    corners[1] + egui::vec2(expand, -expand),
                    corners[2] + egui::vec2(expand, expand),
                    corners[3] + egui::vec2(-expand, expand),
                ];

                let cos_a = text_rotation_angle.cos();
                let sin_a = text_rotation_angle.sin();
                let rotated_corners: Vec<egui::Pos2> = expanded_corners
                    .iter()
                    .map(|corner| {
                        let rotated = egui::vec2(
                            corner.x * cos_a - corner.y * sin_a,
                            corner.x * sin_a + corner.y * cos_a,
                        );
                        label_pos + rotated
                    })
                    .collect();

                painter.add(egui::Shape::convex_polygon(
                    rotated_corners,
                    self.tip_selection_color,
                    egui::Stroke::NONE,
                ));
            }
        }

        TipLabelInfo {
            rect: text_rect,
            rotation_angle: text_rotation_angle,
            anchor: text_anchor,
            anchor_pos: label_pos,
            text_size,
        }
    }

    fn paint_radial_tip_label<F>(
        &self,
        painter: &egui::Painter,
        _tree: &Tree,
        node: &TreeNode,
        label: &str,
        node_pos: egui::Pos2,
        layout: &TreeLayout,
        text_color: Color32,
        is_selected: bool,
        font_id: FontId,
        to_screen: &F,
    ) -> TipLabelInfo
    where
        F: Fn((f32, f32)) -> egui::Pos2,
    {
        // Radial布局：从Internal Node到Tip节点的方向

        // 1. 计算分支方向角度（在屏幕坐标系中计算，确保与hitbox一致）
        let branch_angle = if let Some(parent_id) = node.parent {
            // 转换到屏幕坐标系中计算分支方向
            let parent_screen = to_screen(layout.positions[parent_id]);
            // node_pos已经是屏幕坐标，直接使用

            // 计算屏幕坐标系中从父节点到叶节点的方向
            let direction_x = node_pos.x - parent_screen.x;
            let direction_y = node_pos.y - parent_screen.y;

            // 计算角度（使用atan2确保正确的象限）
            direction_y.atan2(direction_x)
        } else {
            // 根节点默认向右
            0.0
        };

        // 2. 标签位置计算
        // 从叶节点沿分支方向延伸一小段距离作为间距
        let label_offset_distance = 8.0; // Radial布局的间距

        // 计算标签位置：从节点沿分支方向延伸
        // 这个位置将作为所有文字的定位点（无论使用哪种锚点）
        let direction_x = branch_angle.cos();
        let direction_y = branch_angle.sin();
        let label_offset = egui::vec2(
            label_offset_distance * direction_x,
            label_offset_distance * direction_y,
        );
        let label_pos = node_pos + label_offset; // 统一的文字定位点

        // 3. 文字旋转角度处理
        // 对于90度到270度之间的角度，进行180度翻转以增强可读性
        let angle_deg = branch_angle.to_degrees();
        let normalized_angle_deg = if angle_deg < 0.0 {
            angle_deg + 360.0
        } else {
            angle_deg
        };

        let (text_rotation_angle, text_anchor) =
            if normalized_angle_deg > 90.0 && normalized_angle_deg < 270.0 {
                // 文字上下颠倒了，需要翻转180度
                // 使用RIGHT_CENTER锚点，但位置仍然是Branch方向延伸出的定位点
                (
                    branch_angle + std::f32::consts::PI,
                    egui::Align2::RIGHT_CENTER,
                )
            } else {
                // 文字正常可读，使用原始角度
                // 使用LEFT_CENTER锚点，位置是Branch方向延伸出的定位点
                (branch_angle, egui::Align2::LEFT_CENTER)
            };

        // 4. 获取文本尺寸
        let galley = painter.layout_no_wrap(label.to_string(), font_id.clone(), text_color);
        let text_size = galley.size();

        // 5. 绘制旋转文字
        // label_pos 是统一的定位点：
        // - LEFT_CENTER: 文字从这个点向右延伸
        // - RIGHT_CENTER: 文字从这个点向左延伸
        let text_rect = painter.rotated_text(
            label_pos,
            text_anchor,
            label,
            font_id.clone(),
            text_color,
            text_rotation_angle,
        );

        // 5. 绘制选择背景（如果节点被选中）
        if is_selected {
            // 使用与HitBox相同的逻辑：直接使用text_rect.expand(2.0)
            // 这是因为HitBox使用的是 hit.rect.expand(2.0).contains(pointer_pos)
            // 创建一个与HitBox相同的多边形选择框
            let expanded_rect = text_rect.expand(2.0);

            // 如果角度接近0或文字没有太大旋转，使用简单的矩形
            let angle_tolerance = 0.1; // 约5.7度
            if text_rotation_angle.abs() < angle_tolerance
                || (text_rotation_angle - std::f32::consts::PI).abs() < angle_tolerance
                || (text_rotation_angle + std::f32::consts::PI).abs() < angle_tolerance
            {
                painter.rect_filled(expanded_rect, 3.0, self.tip_selection_color);
            } else {
                // 对于有明显旋转的文字，创建旋转的选择框
                let galley = painter.layout_no_wrap(label.to_string(), font_id, text_color);
                let text_size = galley.size();

                // 根据锚点计算文字四个角相对于锚点的位置
                let corners = match text_anchor {
                    egui::Align2::LEFT_CENTER => [
                        egui::vec2(0.0, -text_size.y / 2.0),
                        egui::vec2(text_size.x, -text_size.y / 2.0),
                        egui::vec2(text_size.x, text_size.y / 2.0),
                        egui::vec2(0.0, text_size.y / 2.0),
                    ],
                    egui::Align2::RIGHT_CENTER => [
                        egui::vec2(-text_size.x, -text_size.y / 2.0),
                        egui::vec2(0.0, -text_size.y / 2.0),
                        egui::vec2(0.0, text_size.y / 2.0),
                        egui::vec2(-text_size.x, text_size.y / 2.0),
                    ],
                    _ => [
                        egui::vec2(0.0, -text_size.y / 2.0),
                        egui::vec2(text_size.x, -text_size.y / 2.0),
                        egui::vec2(text_size.x, text_size.y / 2.0),
                        egui::vec2(0.0, text_size.y / 2.0),
                    ],
                };

                // 添加扩展边距到角点
                let expand = 2.0;
                let expanded_corners = [
                    corners[0] + egui::vec2(-expand, -expand),
                    corners[1] + egui::vec2(expand, -expand),
                    corners[2] + egui::vec2(expand, expand),
                    corners[3] + egui::vec2(-expand, expand),
                ];

                // 旋转四个角点
                let cos_a = text_rotation_angle.cos();
                let sin_a = text_rotation_angle.sin();
                let rotated_corners: Vec<egui::Pos2> = expanded_corners
                    .iter()
                    .map(|corner| {
                        let rotated = egui::vec2(
                            corner.x * cos_a - corner.y * sin_a,
                            corner.x * sin_a + corner.y * cos_a,
                        );
                        label_pos + rotated
                    })
                    .collect();

                // 绘制旋转后的选择背景
                painter.add(egui::Shape::convex_polygon(
                    rotated_corners,
                    self.tip_selection_color,
                    egui::Stroke::NONE,
                ));
            }
        }

        TipLabelInfo {
            rect: text_rect,
            rotation_angle: text_rotation_angle,
            anchor: text_anchor,
            anchor_pos: label_pos,
            text_size,
        }
    }

    /// Draw a dashed line between two points
    fn draw_dashed_line(
        &self,
        painter: &egui::Painter,
        start: egui::Pos2,
        end: egui::Pos2,
        stroke: Stroke,
    ) {
        let dx = end.x - start.x;
        let dy = end.y - start.y;
        let length = (dx * dx + dy * dy).sqrt();

        if length < 1e-6 {
            return;
        }

        let dash_length = 5.0;
        let gap_length = 3.0;
        let unit_x = dx / length;
        let unit_y = dy / length;

        let mut current_dist = 0.0;
        let mut drawing = true;

        while current_dist < length {
            let segment_length = if drawing { dash_length } else { gap_length };
            let next_dist = (current_dist + segment_length).min(length);

            if drawing {
                let p1 = egui::pos2(
                    start.x + unit_x * current_dist,
                    start.y + unit_y * current_dist,
                );
                let p2 = egui::pos2(start.x + unit_x * next_dist, start.y + unit_y * next_dist);
                painter.line_segment([p1, p2], stroke);
            }

            current_dist = next_dist;
            drawing = !drawing;
        }
    }

    /// Calculate the distance from root to each tip, and return extension needed for each tip
    fn calculate_tip_extensions(&self, tree: &Tree) -> HashMap<NodeId, f64> {
        let mut distances = HashMap::new();

        // Calculate distance from root to each node
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
        let mut extensions = HashMap::new();
        for node in &tree.nodes {
            if node.is_leaf() {
                if let Some(&distance) = distances.get(&node.id) {
                    let extension = max_distance - distance;
                    if extension > 1e-6 {
                        // Only add if extension is significant
                        extensions.insert(node.id, extension);
                    }
                }
            }
        }

        extensions
    }

    // Helper functions for Circular layout highlight calculation

    /// Collect all descendant nodes from a given node
    fn collect_all_descendants(&self, tree: &Tree, node_id: NodeId) -> Vec<NodeId> {
        let mut descendants = Vec::new();
        let mut stack = vec![node_id];

        while let Some(current_id) = stack.pop() {
            descendants.push(current_id);
            let node = &tree.nodes[current_id];
            stack.extend(&node.children);
        }

        descendants
    }

    /// Calculate the inner radius for highlight (midpoint between node and its parent)
    /// Based on Java: x0 = (xPosition + xParent) / 2.0
    fn calculate_highlight_inner_radius(
        &self,
        tree: &Tree,
        layout: &TreeLayout,
        node_id: NodeId,
        center: (f32, f32),
    ) -> f32 {
        let node_pos = layout.positions[node_id];
        let node_dx = node_pos.0 - center.0;
        let node_dy = node_pos.1 - center.1;
        let node_radius = (node_dx * node_dx + node_dy * node_dy).sqrt();

        // Find the parent and calculate midpoint radius
        let tree_node = &tree.nodes[node_id];
        if let Some(parent_id) = tree_node.parent {
            let parent_pos = layout.positions[parent_id];
            let parent_dx = parent_pos.0 - center.0;
            let parent_dy = parent_pos.1 - center.1;
            let parent_radius = (parent_dx * parent_dx + parent_dy * parent_dy).sqrt();

            // Return the midpoint between parent and node radii
            (parent_radius + node_radius) * 0.5
        } else {
            // Root node: use its own radius slightly reduced
            node_radius * 0.9
        }
    }

    fn calculate_sector_angles(
        &self,
        tree: &Tree,
        layout: &TreeLayout,
        center: (f32, f32),
        leaf_descendants: &[NodeId],
    ) -> Option<(f32, f32)> {
        use std::cmp::Ordering;

        if leaf_descendants.is_empty() {
            return None;
        }

        let mut all_leaf_angles: Vec<(NodeId, f32)> = tree
            .nodes
            .iter()
            .filter(|n| n.is_leaf())
            .map(|n| {
                let pos = layout.positions[n.id];
                let dx = pos.0 - center.0;
                let dy = pos.1 - center.1;
                let mut angle = dy.atan2(dx);
                if angle < 0.0 {
                    angle += std::f32::consts::TAU;
                }
                (n.id, angle)
            })
            .collect();

        if all_leaf_angles.is_empty() {
            return None;
        }

        all_leaf_angles.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(Ordering::Equal));

        let total = all_leaf_angles.len();
        let mut index_map = HashMap::with_capacity(total);
        for (idx, (node_id, _)) in all_leaf_angles.iter().enumerate() {
            index_map.insert(*node_id, idx);
        }

        let mut leaf_info: Vec<(f32, usize)> = leaf_descendants
            .iter()
            .filter_map(|node_id| {
                index_map
                    .get(node_id)
                    .map(|&idx| (all_leaf_angles[idx].1, idx))
            })
            .collect();

        if leaf_info.is_empty() {
            return None;
        }

        leaf_info.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(Ordering::Equal));

        if leaf_info.len() == total {
            return Some((0.0, std::f32::consts::TAU));
        }

        if leaf_info.len() == 1 {
            let angle = leaf_info[0].0;
            let idx = leaf_info[0].1;
            let prev_idx = (idx + total - 1) % total;
            let next_idx = (idx + 1) % total;
            let start = Self::midpoint_angle(all_leaf_angles[prev_idx].1, angle);
            let mut end = Self::midpoint_angle(angle, all_leaf_angles[next_idx].1);
            if end <= start {
                end += std::f32::consts::TAU;
            }
            return Some((start, end));
        }

        let mut max_gap = -1.0f32;
        let mut cut_index = 0usize;
        for i in 0..leaf_info.len() {
            let current = leaf_info[i].0;
            let next = if i + 1 < leaf_info.len() {
                leaf_info[i + 1].0
            } else {
                leaf_info[0].0 + std::f32::consts::TAU
            };
            let gap = next - current;
            if gap > max_gap {
                max_gap = gap;
                cut_index = i;
            }
        }

        let start_entry = leaf_info[(cut_index + 1) % leaf_info.len()];
        let end_entry = leaf_info[cut_index];
        let start_idx = start_entry.1;
        let end_idx = end_entry.1;
        let start_angle = start_entry.0;

        let prev_idx = (start_idx + total - 1) % total;
        let next_idx = (end_idx + 1) % total;

        let start = Self::midpoint_angle(all_leaf_angles[prev_idx].1, start_angle);
        let mut end = Self::midpoint_angle(end_entry.0, all_leaf_angles[next_idx].1);
        if end <= start {
            end += std::f32::consts::TAU;
        }

        Some((start, end))
    }

    fn midpoint_angle(a: f32, b: f32) -> f32 {
        let diff = (b - a).rem_euclid(std::f32::consts::TAU);
        (a + diff * 0.5).rem_euclid(std::f32::consts::TAU)
    }

    pub(crate) fn radial_highlight_polygon_points(
        &self,
        raw_points: &[egui::Pos2],
    ) -> Option<Vec<egui::Pos2>> {
        let points = Self::dedup_points(raw_points);
        if points.is_empty() {
            return None;
        }

        if points.len() == 1 {
            return Some(Self::approximate_circle(points[0], 16.0, 24));
        }

        if points.len() == 2 {
            return Some(Self::approximate_capsule(points[0], points[1], 16.0));
        }

        let hull = Self::convex_hull(&points);
        if hull.is_empty() {
            return None;
        }

        if hull.len() == 1 {
            return Some(Self::approximate_circle(hull[0], 12.0, 24));
        }

        if hull.len() == 2 {
            return Some(Self::approximate_capsule(hull[0], hull[1], 12.0));
        }

        let beziers = Self::catmull_rom_to_beziers(&hull);
        let polygon_points = Self::sample_bezier_loop(&beziers, 10);

        if polygon_points.len() < 3 {
            return Some(Self::approximate_circle(hull[0], 12.0, 24));
        }

        Some(polygon_points)
    }

    pub(crate) fn radial_highlight_polygons_mapped<F>(
        &self,
        tree: &Tree,
        layout: &TreeLayout,
        mut map: F,
    ) -> Vec<(Vec<egui::Pos2>, Color32)>
    where
        F: FnMut((f32, f32)) -> egui::Pos2,
    {
        use crate::tree::layout::TreeLayoutType;

        if !matches!(layout.layout_type, TreeLayoutType::Radial) {
            return Vec::new();
        }

        let mut polygons = Vec::new();

        for (&highlighted_node_id, &highlight_color) in &self.highlighted_clades {
            let descendants = self.collect_all_descendants(tree, highlighted_node_id);
            let leaf_descendants: Vec<NodeId> = descendants
                .into_iter()
                .filter(|&id| tree.nodes[id].is_leaf())
                .collect();

            if leaf_descendants.is_empty() {
                continue;
            }

            let mut highlight_points: Vec<egui::Pos2> = leaf_descendants
                .iter()
                .map(|&tip_id| map(layout.positions[tip_id]))
                .collect();

            let node_screen = map(layout.positions[highlighted_node_id]);
            highlight_points.push(node_screen);

            if let Some(parent_id) = tree.nodes[highlighted_node_id].parent {
                let parent_screen = map(layout.positions[parent_id]);
                let direction = node_screen - parent_screen;
                let length = direction.length().max(1.0);
                let offset = direction / length * (length * 0.2 + 24.0);
                highlight_points.push(node_screen + offset);
            }

            if let Some(polygon_points) = self.radial_highlight_polygon_points(&highlight_points) {
                polygons.push((polygon_points, highlight_color));
            }
        }

        polygons
    }

    fn draw_radial_highlight_polygon(
        &self,
        painter: &egui::Painter,
        raw_points: &[egui::Pos2],
        color: Color32,
    ) {
        let Some(polygon_points) = self.radial_highlight_polygon_points(raw_points) else {
            return;
        };

        let mut mesh = egui::Mesh::default();

        let mut sum = vec2(0.0, 0.0);
        for p in &polygon_points {
            sum += vec2(p.x, p.y);
        }
        let centroid = egui::pos2(
            sum.x / polygon_points.len() as f32,
            sum.y / polygon_points.len() as f32,
        );

        let center_index = mesh.vertices.len() as u32;
        mesh.vertices.push(egui::epaint::Vertex {
            pos: centroid,
            uv: egui::pos2(0.0, 0.0),
            color,
        });

        let mut boundary_indices = Vec::with_capacity(polygon_points.len());
        for point in &polygon_points {
            let idx = mesh.vertices.len() as u32;
            mesh.vertices.push(egui::epaint::Vertex {
                pos: *point,
                uv: egui::pos2(0.0, 0.0),
                color,
            });
            boundary_indices.push(idx);
        }

        for i in 0..polygon_points.len() {
            let next_i = (i + 1) % polygon_points.len();
            mesh.indices.push(center_index);
            mesh.indices.push(boundary_indices[i]);
            mesh.indices.push(boundary_indices[next_i]);
        }

        painter.add(egui::Shape::mesh(mesh));
    }

    fn dedup_points(points: &[egui::Pos2]) -> Vec<egui::Pos2> {
        let mut unique: Vec<egui::Pos2> = Vec::new();
        const EPS: f32 = 0.5;
        for &point in points {
            if !unique
                .iter()
                .any(|p| (p.x - point.x).abs() < EPS && (p.y - point.y).abs() < EPS)
            {
                unique.push(point);
            }
        }
        unique
    }

    fn convex_hull(points: &[egui::Pos2]) -> Vec<egui::Pos2> {
        if points.len() <= 1 {
            return points.to_vec();
        }

        let mut pts = points.to_vec();
        pts.sort_by(|a, b| {
            a.x.partial_cmp(&b.x)
                .unwrap_or(Ordering::Equal)
                .then_with(|| a.y.partial_cmp(&b.y).unwrap_or(Ordering::Equal))
        });

        pts.dedup_by(|a, b| (a.x - b.x).abs() < 1e-3 && (a.y - b.y).abs() < 1e-3);

        if pts.len() <= 2 {
            return pts;
        }

        let mut lower: Vec<egui::Pos2> = Vec::new();
        for &p in &pts {
            while lower.len() >= 2
                && Self::cross(lower[lower.len() - 2], lower[lower.len() - 1], p) <= 0.0
            {
                lower.pop();
            }
            lower.push(p);
        }

        let mut upper: Vec<egui::Pos2> = Vec::new();
        for &p in pts.iter().rev() {
            while upper.len() >= 2
                && Self::cross(upper[upper.len() - 2], upper[upper.len() - 1], p) <= 0.0
            {
                upper.pop();
            }
            upper.push(p);
        }

        lower.pop();
        upper.pop();
        lower.extend(upper);
        lower
    }

    fn cross(a: egui::Pos2, b: egui::Pos2, c: egui::Pos2) -> f32 {
        let ab = egui::vec2(b.x - a.x, b.y - a.y);
        let ac = egui::vec2(c.x - a.x, c.y - a.y);
        ab.x * ac.y - ab.y * ac.x
    }

    fn catmull_rom_to_beziers(points: &[egui::Pos2]) -> Vec<[egui::Pos2; 4]> {
        let n = points.len();
        if n < 3 {
            return Vec::new();
        }

        let mut segments = Vec::with_capacity(n);
        for i in 0..n {
            let p0 = points[(i + n - 1) % n];
            let p1 = points[i % n];
            let p2 = points[(i + 1) % n];
            let p3 = points[(i + 2) % n];

            let (t0, t1, t2, t3) = Self::centripetal_params(p0, p1, p2, p3);

            let start = Self::catmull_rom_point(p0, p1, p2, p3, t0, t1, t2, t3, 0.0);
            let end = Self::catmull_rom_point(p0, p1, p2, p3, t0, t1, t2, t3, 1.0);

            let deriv_start = Self::catmull_derivative(p0, p1, p2, p3, t0, t1, t2, t3, 0.0);
            let deriv_end = Self::catmull_derivative(p0, p1, p2, p3, t0, t1, t2, t3, 1.0);

            let b0 = start;
            let b3 = end;
            let b1 = Self::pos_add_vec(b0, deriv_start / 3.0);
            let b2 = Self::pos_add_vec(b3, -deriv_end / 3.0);

            segments.push([b0, b1, b2, b3]);
        }

        segments
    }

    fn centripetal_params(
        p0: egui::Pos2,
        p1: egui::Pos2,
        p2: egui::Pos2,
        p3: egui::Pos2,
    ) -> (f32, f32, f32, f32) {
        let alpha = 0.5;
        let t0 = 0.0;
        let t1 = t0 + (p1 - p0).length().powf(alpha).max(1e-3);
        let t2 = t1 + (p2 - p1).length().powf(alpha).max(1e-3);
        let t3 = t2 + (p3 - p2).length().powf(alpha).max(1e-3);
        (t0, t1, t2, t3)
    }

    fn catmull_rom_point(
        p0: egui::Pos2,
        p1: egui::Pos2,
        p2: egui::Pos2,
        p3: egui::Pos2,
        t0: f32,
        t1: f32,
        t2: f32,
        t3: f32,
        s: f32,
    ) -> egui::Pos2 {
        let t = t1 + s * (t2 - t1);
        let a1 = Self::lerp_pos(p0, p1, (t - t0) / (t1 - t0));
        let a2 = Self::lerp_pos(p1, p2, (t - t1) / (t2 - t1));
        let a3 = Self::lerp_pos(p2, p3, (t - t2) / (t3 - t2));

        let b1 = Self::lerp_pos(a1, a2, (t - t0) / (t2 - t0));
        let b2 = Self::lerp_pos(a2, a3, (t - t1) / (t3 - t1));

        Self::lerp_pos(b1, b2, (t - t1) / (t2 - t1))
    }

    fn catmull_derivative(
        p0: egui::Pos2,
        p1: egui::Pos2,
        p2: egui::Pos2,
        p3: egui::Pos2,
        t0: f32,
        t1: f32,
        t2: f32,
        t3: f32,
        s: f32,
    ) -> Vec2 {
        let delta = 1e-3;
        let s1 = (s - delta).max(0.0);
        let s2 = (s + delta).min(1.0);
        let p_a = Self::catmull_rom_point(p0, p1, p2, p3, t0, t1, t2, t3, s1).to_vec2();
        let p_b = Self::catmull_rom_point(p0, p1, p2, p3, t0, t1, t2, t3, s2).to_vec2();
        (p_b - p_a) / (s2 - s1)
    }

    fn pos_add_vec(pos: egui::Pos2, vec: Vec2) -> egui::Pos2 {
        egui::pos2(pos.x + vec.x, pos.y + vec.y)
    }

    fn sample_bezier_loop(segments: &[[egui::Pos2; 4]], steps_per: usize) -> Vec<egui::Pos2> {
        let steps_per = steps_per.max(1);
        let mut samples = Vec::new();

        for (idx, seg) in segments.iter().enumerate() {
            for step in 0..=steps_per {
                if idx > 0 && step == 0 {
                    continue;
                }
                if idx < segments.len() - 1 && step == steps_per {
                    continue;
                }
                let t = step as f32 / steps_per as f32;
                samples.push(Self::cubic_bezier_point(seg, t));
            }
        }

        samples
    }

    fn cubic_bezier_point(seg: &[egui::Pos2; 4], t: f32) -> egui::Pos2 {
        let inv = 1.0 - t;
        let p0 = seg[0];
        let p1 = seg[1];
        let p2 = seg[2];
        let p3 = seg[3];
        let x = inv.powi(3) * p0.x
            + 3.0 * inv.powi(2) * t * p1.x
            + 3.0 * inv * t.powi(2) * p2.x
            + t.powi(3) * p3.x;
        let y = inv.powi(3) * p0.y
            + 3.0 * inv.powi(2) * t * p1.y
            + 3.0 * inv * t.powi(2) * p2.y
            + t.powi(3) * p3.y;
        egui::pos2(x, y)
    }

    fn lerp_pos(a: egui::Pos2, b: egui::Pos2, t: f32) -> egui::Pos2 {
        egui::pos2(a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t)
    }

    fn draw_capsule(painter: &egui::Painter, a: egui::Pos2, b: egui::Pos2, color: Color32) {
        let delta = b - a;
        let length = delta.length();
        let radius = 14.0;

        if length <= 1e-3 {
            painter.circle_filled(a, radius, color);
            return;
        }

        let dir = delta / length;
        let perp = egui::vec2(-dir.y, dir.x) * radius;

        let mut mesh = egui::Mesh::default();

        // Add rectangle body
        let rect_points = [a + perp, b + perp, b - perp, a - perp];
        for point in rect_points.iter() {
            mesh.vertices.push(egui::epaint::Vertex {
                pos: *point,
                uv: egui::pos2(0.0, 0.0),
                color,
            });
        }
        mesh.indices.extend_from_slice(&[0, 1, 2, 0, 2, 3]);

        painter.add(egui::Shape::mesh(mesh));

        painter.circle_filled(a, radius, color);
        painter.circle_filled(b, radius, color);
    }

    /// Draw the highlight sector using the existing draw_filled_sector function
    fn draw_highlight_sector(
        &self,
        painter: &egui::Painter,
        center: egui::Pos2,
        inner_radius: f32,
        outer_radius: f32,
        start_angle: f32,
        end_angle: f32,
        color: Color32,
    ) {
        // Ensure radii are valid
        let inner_radius = inner_radius.max(0.0);
        let outer_radius = outer_radius.max(inner_radius + 1.0);

        // Handle angle wrap-around case
        if end_angle < start_angle {
            // Split into two sectors
            // First sector: from start_angle to TAU
            self.draw_filled_sector(
                painter,
                center,
                inner_radius,
                outer_radius,
                start_angle,
                std::f32::consts::TAU,
                color,
            );
            // Second sector: from 0 to end_angle
            self.draw_filled_sector(
                painter,
                center,
                inner_radius,
                outer_radius,
                0.0,
                end_angle - std::f32::consts::TAU,
                color,
            );
        } else {
            // Normal case
            self.draw_filled_sector(
                painter,
                center,
                inner_radius,
                outer_radius,
                start_angle,
                end_angle,
                color,
            );
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TipLabelDisplay {
    Names,
    Labels,
    BranchLength,
}

impl TipLabelDisplay {
    pub fn label(self) -> &'static str {
        match self {
            Self::Names => "Names",
            Self::Labels => "Labels",
            Self::BranchLength => "Branch Length",
        }
    }

    pub fn is_numeric(self) -> bool {
        matches!(self, Self::BranchLength)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TipLabelNumberFormat {
    Decimal,
    Scientific,
    Percentage,
}

impl TipLabelNumberFormat {
    pub fn label(self) -> &'static str {
        match self {
            Self::Decimal => "Decimal",
            Self::Scientific => "Scientific",
            Self::Percentage => "Percentage",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TipLabelFontFamily {
    Proportional,
    Monospace,
}

impl TipLabelFontFamily {
    pub fn display_name(self) -> &'static str {
        match self {
            Self::Proportional => "Proportional",
            Self::Monospace => "Monospace",
        }
    }

    pub fn into_font_family(self) -> FontFamily {
        match self {
            Self::Proportional => FontFamily::Proportional,
            Self::Monospace => FontFamily::Monospace,
        }
    }
}
