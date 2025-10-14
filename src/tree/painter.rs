use std::collections::{HashMap, HashSet};

use eframe::egui::{self, Color32, FontFamily, FontId, Stroke};

use super::{NodeId, Tree, TreeNode};
use crate::tree::layout::TreeLayout;
use crate::rotated_text::RotatedText;

#[derive(Clone)]
pub struct TipLabelHit {
    pub node_id: NodeId,
    pub rect: egui::Rect,
    pub rotation_angle: f32,  // 添加旋转角度信息
    pub anchor: egui::Align2,  // 添加锚点信息
}

impl TipLabelHit {
    /// 检测点是否在旋转的标签内
    pub fn contains(&self, point: egui::Pos2, expand: f32) -> bool {
        // 如果旋转角度很小，使用简单的矩形测试
        let angle_tolerance = 0.1; // 约5.7度
        if self.rotation_angle.abs() < angle_tolerance {
            return self.rect.expand(expand).contains(point);
        }

        // 对于旋转的标签，需要更精确的测试
        // 将点转换到标签的本地坐标系
        let rect = self.rect.expand(expand);
        let center = rect.center();

        // 将点相对于中心进行反向旋转
        let dx = point.x - center.x;
        let dy = point.y - center.y;
        let cos_a = (-self.rotation_angle).cos();
        let sin_a = (-self.rotation_angle).sin();
        let local_x = dx * cos_a - dy * sin_a + center.x;
        let local_y = dx * sin_a + dy * cos_a + center.y;
        let local_point = egui::pos2(local_x, local_y);

        // 在本地坐标系中检测
        rect.contains(local_point)
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
            align_tip_labels: false,
            tip_label_display: TipLabelDisplay::Names,
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

    pub fn paint_tree(
        &self,
        painter: &egui::Painter,
        tree: &Tree,
        layout: &TreeLayout,
        selected_nodes: &HashSet<NodeId>,
        selected_tips: &HashSet<NodeId>,
        rect: egui::Rect,
        margin: egui::Vec2,
    ) -> Vec<TipLabelHit> {
        let mut tip_label_hits = Vec::new();
        // Paint background
        painter.rect_filled(rect, 10.0, self.background_color);

        let inner = rect.shrink2(margin);
        let inner = if inner.is_positive() { inner } else { rect };
        painter.rect_filled(inner, 8.0, self.canvas_color);

        // Calculate tip extensions for align_tip_labels
        let tip_extensions = if self.align_tip_labels {
            self.calculate_tip_extensions(tree)
        } else {
            HashMap::new()
        };

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

        let to_screen = |pos: (f32, f32)| -> egui::Pos2 {
            match layout.layout_type {
                super::layout::TreeLayoutType::Circular | super::layout::TreeLayoutType::Radial | super::layout::TreeLayoutType::Daylight => {
                    // 对于圆形、径向和Daylight布局，使用统一的缩放因子保持形状
                    // 计算画布内切圆的最大可用半径，为标签留出固定空间
                    let label_margin = if self.align_tip_labels && !tip_extensions.is_empty() {
                        self.tip_label_font_size * 8.0 // 为延伸的标签留出屏幕空间
                    } else {
                        0.0
                    };
                    let available_radius = inner.width().min(inner.height()) * 0.45 - label_margin;

                    // 获取根节点位置作为圆的中心（进化树形状的真实中心）
                    let layout_center = if matches!(layout.layout_type, super::layout::TreeLayoutType::Circular) {
                        if let Some(root_id) = tree.root {
                            layout.positions[root_id]
                        } else {
                            (layout.width * 0.5, layout.height * 0.5)
                        }
                    } else {
                        (layout.width * 0.5, layout.height * 0.5)
                    };

                    // 计算layout的实际半径（从根节点中心到所有节点的最大距离）
                    let layout_radius = if self.align_tip_labels && !tip_extensions.is_empty() {
                        // 找到所有tip到根节点的最大距离，加上extension
                        let mut max_radius = 0.0f32;
                        for node in &tree.nodes {
                            if node.is_leaf() {
                                let pos = layout.positions[node.id];
                                let dx = pos.0 - layout_center.0;
                                let dy = pos.1 - layout_center.1;
                                let radius = (dx * dx + dy * dy).sqrt();
                                max_radius = max_radius.max(radius);
                            }
                        }
                        // 加上最大的extension（都在layout坐标系中）
                        let max_extension = tip_extensions.values().fold(0.0f64, |a, &b| a.max(b)) as f32;
                        (max_radius + max_extension).max(1e-6)
                    } else {
                        // 计算所有节点到根节点的最大欧几里得距离（精确值）
                        let mut max_radius = 0.0f32;
                        for node in &tree.nodes {
                            let pos = layout.positions[node.id];
                            let dx = pos.0 - layout_center.0;
                            let dy = pos.1 - layout_center.1;
                            let radius = (dx * dx + dy * dy).sqrt();
                            max_radius = max_radius.max(radius);
                        }
                        max_radius.max(1e-6)
                    };

                    let scale = available_radius / layout_radius;

                    // 计算画布中心
                    let center_x = inner.left() + inner.width() * 0.5;
                    let center_y = inner.top() + inner.height() * 0.5;

                    // 将布局坐标转换为相对于画布中心的坐标（使用根节点作为参考中心）
                    let x = center_x + (pos.0 - layout_center.0) * scale;
                    let y = center_y + (pos.1 - layout_center.1) * scale;

                    egui::pos2(x, y)
                }
                _ => {
                    // 其他布局使用标准坐标转换
                    let x = inner.left() + pos.0 * scale_x;
                    let y = inner.top() + pos.1 * scale_y;
                    egui::pos2(x, y)
                }
            }
        };

        // Paint highlight rectangles for Rectangular layout
        if matches!(layout.layout_type, super::layout::TreeLayoutType::Rectangular) {
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
                    clade_tip_ys.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

                    if let (Some(&min_clade_y), Some(&max_clade_y)) = (clade_tip_ys.first(), clade_tip_ys.last()) {
                        // Calculate top edge: halfway to the clade above
                        let top_y = if let Some(&above_y) = all_tip_ys.iter().rev().find(|&&y| y < min_clade_y) {
                            (min_clade_y + above_y) * 0.5
                        } else {
                            // No clade above, extend to the top margin
                            min_clade_y - (all_tip_ys[1] - all_tip_ys[0]) * 0.5
                        };

                        // Calculate bottom edge: halfway to the clade below
                        let bottom_y = if let Some(&below_y) = all_tip_ys.iter().find(|&&y| y > max_clade_y) {
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
                        let highlight_rect = egui::Rect::from_two_pos(top_left_screen, bottom_right_screen);
                        painter.rect_filled(highlight_rect, 0.0, highlight_color);
                    }
                }
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
                        let max_extension = tip_extensions.values().fold(0.0f64, |a, &b| a.max(b)) as f32;
                        max_radius + max_extension
                    } else {
                        max_radius
                    }
                }.max(1e-6);

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

                if let Some((start_angle, end_angle)) = self.calculate_sector_angles(
                    tree,
                    layout,
                    center_world,
                    &leaf_descendants,
                ) {
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
                    );
                }
            }
            super::layout::TreeLayoutType::Circular => {
                // 使用连续分支绘制圆形布局
                if !layout.continuous_branches.is_empty() {
                    // 优先使用连续分支绘制
                    for branch in &layout.continuous_branches {
                        let screen_points: Vec<egui::Pos2> = branch.points
                            .iter()
                            .map(|&point| to_screen(point))
                            .collect();

                        let override_color = self.branch_color_overrides.get(&branch.child).copied();
                        let is_selected = selected_nodes.contains(&branch.child);
                        self.draw_branch(
                            painter,
                            screen_points,
                            override_color,
                            is_selected,
                        );
                    }
                } else {
                    // 回退到分段绘制（向后兼容）
                    for arc_segment in &layout.arc_segments {
                        self.draw_arc(
                            painter,
                            arc_segment,
                            to_screen,
                            inner,
                            selected_nodes,
                        );
                    }

                    for segment in &layout.rect_segments {
                        let start_screen = to_screen(segment.start);
                        let end_screen = to_screen(segment.end);

                        let override_color = segment.child.and_then(|child| self.branch_color_overrides.get(&child).copied());
                        let is_selected = segment.child.map_or(false, |child| selected_nodes.contains(&child));
                        self.draw_branch(
                            painter,
                            vec![start_screen, end_screen],
                            override_color,
                            is_selected,
                        );
                    }
                }
            }
            super::layout::TreeLayoutType::Radial
            | super::layout::TreeLayoutType::Daylight => {
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
                    );
                }
            }
        }

        // Paint tip extensions (dashed lines) if align_tip_labels is enabled
        if self.align_tip_labels && !tip_extensions.is_empty() {
            let dashed_stroke = Stroke::new(
                self.branch_stroke.width * 0.8,
                Color32::from_gray(150)
            );

            for (tip_id, &extension) in &tip_extensions {
                if extension < 1e-6 {
                    continue;
                }

                let tip_world = layout.positions[*tip_id];
                let tip_screen = to_screen(tip_world);

                // Calculate extended position based on layout type
                let extended_pos = match layout.layout_type {
                    super::layout::TreeLayoutType::Rectangular |
                    super::layout::TreeLayoutType::Slanted |
                    super::layout::TreeLayoutType::Cladogram |
                    super::layout::TreeLayoutType::Phylogram => {
                        // For rectangular-like layouts, extend horizontally
                        let extended_world = (tip_world.0 + extension as f32, tip_world.1);
                        to_screen(extended_world)
                    }
                    super::layout::TreeLayoutType::Circular => {
                        // For circular layout, extend along the straight branch direction (shoulder to tip)
                        // Find the continuous branch for this tip
                        let direction = if let Some(branch) = layout.continuous_branches.iter().find(|b| b.child == *tip_id) {
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
                    super::layout::TreeLayoutType::Radial |
                    super::layout::TreeLayoutType::Daylight => {
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
                                super::layout::TreeLayoutType::Rectangular |
                                super::layout::TreeLayoutType::Slanted |
                                super::layout::TreeLayoutType::Cladogram |
                                super::layout::TreeLayoutType::Phylogram => {
                                    (tip_world.0 + extension as f32, tip_world.1)
                                }
                                super::layout::TreeLayoutType::Circular => {
                                    // For circular layout, extend along the straight branch direction (shoulder to tip)
                                    let direction = if let Some(branch) = layout.continuous_branches.iter().find(|b| b.child == node.id) {
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
                                super::layout::TreeLayoutType::Radial |
                                super::layout::TreeLayoutType::Daylight => {
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

                    if let Some((text_rect, rotation_angle, anchor)) = self.paint_tip_label(
                        painter,
                        tree,
                        node,
                        &label,
                        label_pos,
                        layout,
                        text_color,
                        tip_selected,
                    ) {
                        tip_label_hits.push(TipLabelHit {
                            node_id: node.id,
                            rect: text_rect,
                            rotation_angle,
                            anchor,
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

    fn draw_branch(
        &self,
        painter: &egui::Painter,
        points: Vec<egui::Pos2>,
        override_color: Option<Color32>,
        is_selected: bool,
    ) {
        let base_width = self.branch_stroke.width;
        let base_color = override_color.unwrap_or(self.branch_stroke.color);

        if is_selected {
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

    fn draw_arc(
        &self,
        painter: &egui::Painter,
        arc_segment: &crate::tree::layout::ArcSegment,
        to_screen: impl Fn((f32, f32)) -> egui::Pos2,
        _inner: egui::Rect,
        selected_nodes: &HashSet<NodeId>,
    ) {
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
        if is_selected {
            let highlight_width = self.branch_highlight_stroke.width.max(self.branch_stroke.width * 3.0);
            let highlight_stroke = Stroke::new(highlight_width, self.tip_selection_color);
            painter.add(egui::Shape::line(arc_points.clone(), highlight_stroke));
        }

        let base_stroke = Stroke::new(self.branch_stroke.width, base_color);
        painter.add(egui::Shape::line(arc_points, base_stroke));
    }

    pub fn tip_label_text(&self, _tree: &Tree, node: &TreeNode) -> Option<String> {
        match self.tip_label_display {
            TipLabelDisplay::Names => node.name.clone(),
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

    fn paint_tip_label(
        &self,
        painter: &egui::Painter,
        tree: &Tree,
        node: &TreeNode,
        label: &str,
        node_pos: egui::Pos2,
        layout: &TreeLayout,
        text_color: Color32,
        is_selected: bool,
    ) -> Option<(egui::Rect, f32, egui::Align2)> {
        let font_id = FontId::new(
            self.tip_label_font_size,
            self.tip_label_font_family.into_font_family(),
        );

        // 根据布局类型选择不同的标签绘制方式
        match layout.layout_type {
            super::layout::TreeLayoutType::Circular => {
                // Circular布局使用从中心到Tip的方向
                let (rect, angle, anchor) = self.paint_circular_tip_label(
                    painter, node, label, node_pos, layout, text_color, is_selected, font_id,
                );
                Some((rect, angle, anchor))
            }
            super::layout::TreeLayoutType::Radial | super::layout::TreeLayoutType::Daylight => {
                // Radial和Daylight布局使用从Internal Node到Tip的方向
                let (rect, angle, anchor) = self.paint_radial_tip_label(
                    painter, tree, node, label, node_pos, layout, text_color, is_selected, font_id,
                );
                Some((rect, angle, anchor))
            }
            _ => {
                // 其他布局使用标准的右侧标签
                let rect = self.paint_right_side_tip_label(
                    painter, label, node_pos, text_color, is_selected, font_id,
                );
                Some((rect, 0.0, egui::Align2::LEFT_CENTER))
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
            let mut rect = egui::Rect::from_min_size(
                text_pos + egui::vec2(0.0, -size.y * 0.5),
                size,
            );
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

    fn paint_circular_tip_label(
        &self,
        painter: &egui::Painter,
        node: &TreeNode,
        label: &str,
        node_pos: egui::Pos2,
        layout: &TreeLayout,
        text_color: Color32,
        is_selected: bool,
        font_id: FontId,
    ) -> (egui::Rect, f32, egui::Align2) {
        // Circular布局：使用从shoulder点（弧线和直线的拐角点）到Tip的方向

        // 1. 计算分支方向角度（在布局坐标系中计算，避免窗口缩放影响）
        let straight_line_angle = if let Some(_parent_id) = node.parent {
            // 查找对应的continuous branch来获取shoulder点
            if let Some(branch) = layout.continuous_branches.iter().find(|b| b.child == node.id) {
                // branch.points的结构是: [child, shoulder, ...arc points..., parent]
                // 第二个点就是shoulder点（拐角点）
                if branch.points.len() >= 2 {
                    let child_world_pos = branch.points[0];  // Tip节点位置
                    let shoulder_world_pos = branch.points[1];  // 拐角点位置

                    // 在布局坐标系中计算角度（不受窗口缩放影响）
                    let dx = child_world_pos.0 - shoulder_world_pos.0;
                    let dy = child_world_pos.1 - shoulder_world_pos.1;
                    dy.atan2(dx)
                } else {
                    // fallback: 如果找不到shoulder点，使用布局坐标系中的父节点到子节点方向
                    let parent_world_pos = layout.positions[_parent_id];
                    let child_world_pos = layout.positions[node.id];

                    let dx = child_world_pos.0 - parent_world_pos.0;
                    let dy = child_world_pos.1 - parent_world_pos.1;
                    dy.atan2(dx)
                }
            } else {
                // fallback: 如果找不到对应的branch，使用布局坐标系中的父节点到子节点方向
                let parent_world_pos = layout.positions[_parent_id];
                let child_world_pos = layout.positions[node.id];

                let dx = child_world_pos.0 - parent_world_pos.0;
                let dy = child_world_pos.1 - parent_world_pos.1;
                dy.atan2(dx)
            }
        } else {
            // 根节点：使用从布局中心到节点的方向
            let layout_center_x = layout.width * 0.5;
            let layout_center_y = layout.height * 0.5;
            let node_world_pos = layout.positions[node.id];

            let dx = node_world_pos.0 - layout_center_x;
            let dy = node_world_pos.1 - layout_center_y;
            dy.atan2(dx)
        };

        // 2. 标签位置计算
        let label_offset_distance = 12.0;  // Circular布局的间距
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

        let (text_rotation_angle, text_anchor) = if normalized_angle_deg > 90.0 && normalized_angle_deg < 270.0 {
            // 文字上下颠倒了，需要翻转180度
            (straight_line_angle + std::f32::consts::PI, egui::Align2::RIGHT_CENTER)
        } else {
            // 文字正常可读
            (straight_line_angle, egui::Align2::LEFT_CENTER)
        };

        // 4. 绘制旋转文字
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
            let expanded_rect = text_rect.expand(2.0);
            let angle_tolerance = 0.1;

            if text_rotation_angle.abs() < angle_tolerance ||
               (text_rotation_angle - std::f32::consts::PI).abs() < angle_tolerance ||
               (text_rotation_angle + std::f32::consts::PI).abs() < angle_tolerance {
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
                    ]
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

        (text_rect, text_rotation_angle, text_anchor)
    }

    fn paint_radial_tip_label(
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
    ) -> (egui::Rect, f32, egui::Align2) {
        // Radial布局：从Internal Node到Tip节点的方向

        // 1. 计算分支方向角度（从父节点指向子节点）
        let branch_angle = if let Some(parent_id) = node.parent {
            // 使用布局坐标系中的分支方向（统一计算）
            let parent_world_pos = layout.positions[parent_id];
            let node_world_pos = layout.positions[node.id];

            // 计算布局坐标系中从父节点到叶节点的方向
            let direction_x = node_world_pos.0 - parent_world_pos.0;
            let direction_y = node_world_pos.1 - parent_world_pos.1;

            // 计算角度（使用atan2确保正确的象限）
            direction_y.atan2(direction_x)
        } else {
            // 根节点默认向右
            0.0
        };

        // 2. 标签位置计算
        // 从叶节点沿分支方向延伸一小段距离作为间距
        let label_offset_distance = 8.0;  // Radial布局的间距

        // 计算标签位置：从节点沿分支方向延伸
        // 这个位置将作为所有文字的定位点（无论使用哪种锚点）
        let direction_x = branch_angle.cos();
        let direction_y = branch_angle.sin();
        let label_offset = egui::vec2(
            label_offset_distance * direction_x,
            label_offset_distance * direction_y,
        );
        let label_pos = node_pos + label_offset;  // 统一的文字定位点

        // 3. 文字旋转角度处理
        // 对于90度到270度之间的角度，进行180度翻转以增强可读性
        let angle_deg = branch_angle.to_degrees();
        let normalized_angle_deg = if angle_deg < 0.0 {
            angle_deg + 360.0
        } else {
            angle_deg
        };

        let (text_rotation_angle, text_anchor) = if normalized_angle_deg > 90.0 && normalized_angle_deg < 270.0 {
            // 文字上下颠倒了，需要翻转180度
            // 使用RIGHT_CENTER锚点，但位置仍然是Branch方向延伸出的定位点
            (branch_angle + std::f32::consts::PI, egui::Align2::RIGHT_CENTER)
        } else {
            // 文字正常可读，使用原始角度
            // 使用LEFT_CENTER锚点，位置是Branch方向延伸出的定位点
            (branch_angle, egui::Align2::LEFT_CENTER)
        };

        // 4. 绘制旋转文字
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
            if text_rotation_angle.abs() < angle_tolerance ||
               (text_rotation_angle - std::f32::consts::PI).abs() < angle_tolerance ||
               (text_rotation_angle + std::f32::consts::PI).abs() < angle_tolerance {
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
                    ]
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

        (text_rect, text_rotation_angle, text_anchor)
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
                let p2 = egui::pos2(
                    start.x + unit_x * next_dist,
                    start.y + unit_y * next_dist,
                );
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
        let mut extensions = HashMap::new();
        for node in &tree.nodes {
            if node.is_leaf() {
                if let Some(&distance) = distances.get(&node.id) {
                    let extension = max_distance - distance;
                    if extension > 1e-6 {  // Only add if extension is significant
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

        all_leaf_angles
            .sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(Ordering::Equal));

        let total = all_leaf_angles.len();
        let mut index_map = HashMap::with_capacity(total);
        for (idx, (node_id, _)) in all_leaf_angles.iter().enumerate() {
            index_map.insert(*node_id, idx);
        }

        let mut leaf_info: Vec<(f32, usize)> = leaf_descendants
            .iter()
            .filter_map(|node_id| index_map.get(node_id).map(|&idx| (all_leaf_angles[idx].1, idx)))
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TipLabelDisplay {
    Names,
    BranchLength,
}

impl TipLabelDisplay {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
