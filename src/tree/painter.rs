use std::cmp::Ordering;
use std::collections::HashMap;

use eframe::egui::{self, Color32, FontFamily, Stroke};

use super::{NodeId, Tree, TreeNode};
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
    pub scale_bar_range: f32,
    pub scale_bar_font_size: f32,
    pub scale_bar_line_width: f32,
    pub show_tip_shapes: bool,
    pub show_node_shapes: bool,
    pub tip_shape: ShapeType,
    pub node_shape: ShapeType,
    pub tip_shape_size_mode: ShapeSizeMode,
    pub node_shape_size_mode: ShapeSizeMode,
    pub tip_shape_size_attribute: Option<String>,
    pub node_shape_size_attribute: Option<String>,
    pub tip_shape_max_size: f32,
    pub node_shape_max_size: f32,
    pub tip_shape_min_size: f32,
    pub node_shape_min_size: f32,
    pub tip_shape_color_mode: ShapeColorMode,
    pub node_shape_color_mode: ShapeColorMode,
    pub tip_shape_fixed_color: Color32,
    pub node_shape_fixed_color: Color32,
    pub show_node_bars: bool,
    pub node_bar_field: Option<String>,
    pub node_bar_color: Color32,
    pub node_bar_thickness: f32,
    pub align_tip_labels: bool,
    pub tip_label_display: TipLabelDisplay,
    pub node_label_display: NodeLabelDisplay,
    pub branch_label_display: BranchLabelDisplay,
    pub node_label_attribute: Option<String>,
    pub node_label_font_size: f32,
    pub branch_label_font_size: f32,
    pub node_label_format: TipLabelNumberFormat,
    pub branch_label_format: TipLabelNumberFormat,
    pub node_label_precision: usize,
    pub branch_label_precision: usize,
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
            scale_bar_range: 0.5,
            scale_bar_font_size: 11.0,
            scale_bar_line_width: 2.0,
            show_tip_shapes: false,
            show_node_shapes: false,
            tip_shape: ShapeType::Circle,
            node_shape: ShapeType::Circle,
            tip_shape_size_mode: ShapeSizeMode::Fixed,
            node_shape_size_mode: ShapeSizeMode::Fixed,
            tip_shape_size_attribute: None,
            node_shape_size_attribute: None,
            tip_shape_max_size: 4.0,
            node_shape_max_size: 4.0,
            tip_shape_min_size: 2.0,
            node_shape_min_size: 2.0,
            tip_shape_color_mode: ShapeColorMode::Fixed,
            node_shape_color_mode: ShapeColorMode::Fixed,
            tip_shape_fixed_color: Color32::from_rgb(255, 205, 140),
            node_shape_fixed_color: Color32::from_rgb(135, 205, 255),
            show_node_bars: false,
            node_bar_field: None,
            node_bar_color: Color32::from_rgba_unmultiplied(96, 186, 255, 100),
            node_bar_thickness: 10.0,
            align_tip_labels: false,
            tip_label_display: TipLabelDisplay::Labels,
            node_label_display: NodeLabelDisplay::Labels,
            branch_label_display: BranchLabelDisplay::BranchLength,
            node_label_attribute: None,
            node_label_font_size: 10.0,
            branch_label_font_size: 10.0,
            node_label_format: TipLabelNumberFormat::Decimal,
            branch_label_format: TipLabelNumberFormat::Decimal,
            node_label_precision: 2,
            branch_label_precision: 2,
            tip_label_font_family: TipLabelFontFamily::Proportional,
            tip_label_font_size: 13.0,
            tip_label_format: TipLabelNumberFormat::Decimal,
            tip_label_precision: 2,
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

    pub fn branch_color_overrides(&self) -> &HashMap<NodeId, Color32> {
        &self.branch_color_overrides
    }

    pub fn set_tip_label_color(&mut self, node_id: NodeId, color: Color32) {
        self.tip_label_color_overrides.insert(node_id, color);
    }

    pub fn tip_label_color_override(&self, node_id: NodeId) -> Option<Color32> {
        self.tip_label_color_overrides.get(&node_id).copied()
    }

    pub fn tip_label_color_overrides(&self) -> &HashMap<NodeId, Color32> {
        &self.tip_label_color_overrides
    }

    pub fn clear_color_overrides(&mut self) {
        self.branch_color_overrides.clear();
        self.tip_label_color_overrides.clear();
    }

    pub fn add_highlighted_clade(&mut self, node_id: NodeId, color: Color32) {
        self.highlighted_clades.insert(node_id, color);
    }

    pub fn clear_highlighted_clades(&mut self) {
        self.highlighted_clades.clear();
    }

    pub fn highlighted_clades(&self) -> &HashMap<NodeId, Color32> {
        &self.highlighted_clades
    }

    pub fn shape_size_for_node(
        &self,
        tree: &Tree,
        node_id: NodeId,
        is_tip: bool,
        value_range: Option<(f64, f64)>,
    ) -> f32 {
        let (mode, attr, min_size, max_size) = if is_tip {
            (
                self.tip_shape_size_mode,
                self.tip_shape_size_attribute.as_deref(),
                self.tip_shape_min_size,
                self.tip_shape_max_size,
            )
        } else {
            (
                self.node_shape_size_mode,
                self.node_shape_size_attribute.as_deref(),
                self.node_shape_min_size,
                self.node_shape_max_size,
            )
        };

        if !matches!(mode, ShapeSizeMode::Attribute) {
            return max_size.max(0.5);
        }

        let Some(key) = attr else {
            return max_size.max(0.5);
        };
        let Some(v) = tree.nodes[node_id].get_numeric_attribute(key) else {
            return max_size.max(0.5);
        };
        let Some((min_v, max_v)) = value_range else {
            return max_size.max(0.5);
        };

        if (max_v - min_v).abs() < f64::EPSILON {
            return max_size.max(0.5);
        }

        let t = ((v - min_v) / (max_v - min_v)).clamp(0.0, 1.0) as f32;
        (min_size + (max_size - min_size) * t).max(0.5)
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
    pub fn tip_label_text(&self, _tree: &Tree, node: &TreeNode) -> Option<String> {
        match self.tip_label_display {
            TipLabelDisplay::Names => node.name.clone(),
            TipLabelDisplay::Labels => node.label.clone().or_else(|| node.name.clone()),
            TipLabelDisplay::BranchLength => node.length.map(|value| {
                Self::format_numeric_with(value, self.tip_label_format, self.tip_label_precision)
            }),
        }
    }

    pub fn node_label_text(
        &self,
        tree: &Tree,
        node: &TreeNode,
        node_heights: Option<&HashMap<NodeId, f64>>,
    ) -> Option<String> {
        match self.node_label_display {
            NodeLabelDisplay::Names => node.name.clone(),
            NodeLabelDisplay::Labels => node.label.clone().or_else(|| node.name.clone()),
            NodeLabelDisplay::BranchLength => node.length.map(|value| {
                Self::format_numeric_with(value, self.node_label_format, self.node_label_precision)
            }),
            NodeLabelDisplay::NodeHeight => {
                let height = node_heights
                    .and_then(|map| map.get(&node.id).copied())
                    .unwrap_or_else(|| self.node_height_fallback(tree, node.id));
                Some(Self::format_numeric_with(
                    height,
                    self.node_label_format,
                    self.node_label_precision,
                ))
            }
            NodeLabelDisplay::Attribute => {
                let key = self.node_label_attribute.as_deref()?;
                if let Some((min, max)) = node.numeric_range_attribute(key) {
                    let min_text =
                        Self::format_numeric_with(min, self.node_label_format, self.node_label_precision);
                    let max_text =
                        Self::format_numeric_with(max, self.node_label_format, self.node_label_precision);
                    if (max - min).abs() <= f64::EPSILON {
                        Some(min_text)
                    } else {
                        Some(format!("[{min_text},{max_text}]"))
                    }
                } else if let Some(value) = node.get_numeric_attribute(key) {
                    Some(Self::format_numeric_with(
                        value,
                        self.node_label_format,
                        self.node_label_precision,
                    ))
                } else {
                    node.get_attribute(key).cloned()
                }
            }
        }
    }

    pub fn branch_label_text(&self, node: &TreeNode) -> Option<String> {
        match self.branch_label_display {
            BranchLabelDisplay::Names => node.name.clone(),
            BranchLabelDisplay::BranchLength => node.length.map(|value| {
                Self::format_numeric_with(
                    value,
                    self.branch_label_format,
                    self.branch_label_precision,
                )
            }),
        }
    }

    pub fn compute_node_heights(&self, tree: &Tree) -> HashMap<NodeId, f64> {
        let mut heights = HashMap::new();
        fn dfs(tree: &Tree, node_id: NodeId, out: &mut HashMap<NodeId, f64>) -> f64 {
            let node = &tree.nodes[node_id];
            if node.children.is_empty() {
                out.insert(node_id, 0.0);
                return 0.0;
            }
            let mut max_h = 0.0f64;
            for &child_id in &node.children {
                let len = tree.nodes[child_id].length.unwrap_or(1.0);
                let child_h = dfs(tree, child_id, out);
                max_h = max_h.max(child_h + len);
            }
            out.insert(node_id, max_h);
            max_h
        }
        if let Some(root_id) = tree.root {
            dfs(tree, root_id, &mut heights);
        }
        heights
    }

    fn node_height_fallback(&self, tree: &Tree, node_id: NodeId) -> f64 {
        fn dist_to_tip(tree: &Tree, node_id: NodeId) -> f64 {
            let node = &tree.nodes[node_id];
            if node.children.is_empty() {
                return 0.0;
            }
            node.children
                .iter()
                .map(|&child_id| tree.nodes[child_id].length.unwrap_or(1.0) + dist_to_tip(tree, child_id))
                .fold(0.0f64, |a, b| a.max(b))
        }
        dist_to_tip(tree, node_id)
    }

    fn format_numeric_with(
        value: f64,
        format: TipLabelNumberFormat,
        precision: usize,
    ) -> String {
        match format {
            TipLabelNumberFormat::Decimal => {
                format!("{value:.precision$}")
            }
            TipLabelNumberFormat::Scientific => {
                format!("{value:.precision$e}")
            }
            TipLabelNumberFormat::Percentage => {
                let percent = value * 100.0;
                format!("{percent:.precision$}%")
            }
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShapeType {
    Circle,
    Square,
    Diamond,
}

impl ShapeType {
    pub fn label(self) -> &'static str {
        match self {
            Self::Circle => "Circle",
            Self::Square => "Square",
            Self::Diamond => "Diamond",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShapeSizeMode {
    Fixed,
    Attribute,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShapeColorMode {
    UserSelection,
    Fixed,
}

impl ShapeColorMode {
    pub fn label(self) -> &'static str {
        match self {
            Self::UserSelection => "User Selection",
            Self::Fixed => "Fixed",
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
pub enum NodeLabelDisplay {
    Names,
    Labels,
    BranchLength,
    NodeHeight,
    Attribute,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BranchLabelDisplay {
    Names,
    BranchLength,
}

impl BranchLabelDisplay {
    pub fn label(self) -> &'static str {
        match self {
            Self::Names => "Names",
            Self::BranchLength => "Branch Length",
        }
    }

    pub fn is_numeric(self) -> bool {
        matches!(self, Self::BranchLength)
    }
}

impl NodeLabelDisplay {
    pub fn label(self) -> &'static str {
        match self {
            Self::Names => "Names",
            Self::Labels => "Labels",
            Self::BranchLength => "Branch Length",
            Self::NodeHeight => "Node Height",
            Self::Attribute => "Attribute",
        }
    }

    pub fn is_numeric(self) -> bool {
        matches!(self, Self::BranchLength | Self::NodeHeight | Self::Attribute)
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
