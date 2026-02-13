use super::{NodeId, Tree};
use eframe::egui::{self, FontFamily};

mod circular;
mod cladogram;
mod daylight;
mod phylogram;
mod radial;
mod rectangular;
mod slanted;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TreeLayoutType {
    Rectangular,
    Circular,
    Radial,
    Slanted,
    Cladogram,
    Phylogram,
    Daylight,
}

#[derive(Debug, Clone)]
pub struct TreeLayout {
    pub positions: Vec<(f32, f32)>,
    pub edges: Vec<(NodeId, NodeId)>,
    pub width: f32,
    pub height: f32,
    pub leaf_count: usize,
    pub layout_type: TreeLayoutType,
    pub rect_segments: Vec<RectSegment>,
    pub arc_segments: Vec<ArcSegment>,
    pub continuous_branches: Vec<ContinuousBranch>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RectSegmentKind {
    Horizontal,
    Vertical,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct RectSegment {
    pub start: (f32, f32),
    pub end: (f32, f32),
    pub parent: NodeId,
    pub child: Option<NodeId>,
    pub kind: RectSegmentKind,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ArcSegment {
    pub center: (f32, f32),
    pub radius: f32,
    pub start_angle: f32,
    pub end_angle: f32,
    pub parent: NodeId,
    pub child: NodeId,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ContinuousBranch {
    pub points: Vec<(f32, f32)>,
    pub parent: NodeId,
    pub child: NodeId,
}

pub(super) const DEFAULT_BRANCH_LENGTH: f32 = 1.0;
pub(super) const ROOT_LENGTH_PROPORTION: f32 = 0.01;

impl TreeLayout {
    /// Build a layout for the provided tree using the specified layout type.
    pub fn from_tree(tree: &Tree, layout_type: TreeLayoutType) -> Option<Self> {
        match layout_type {
            TreeLayoutType::Rectangular => Self::rectangular_layout(tree),
            TreeLayoutType::Circular => Self::circular_layout(tree),
            TreeLayoutType::Radial => Self::radial_layout(tree),
            TreeLayoutType::Slanted => Self::slanted_layout(tree),
            TreeLayoutType::Cladogram => Self::cladogram_layout(tree),
            TreeLayoutType::Phylogram => Self::phylogram_layout(tree),
            TreeLayoutType::Daylight => Self::daylight_layout(tree),
        }
    }

    fn rectangular_layout(tree: &Tree) -> Option<Self> {
        rectangular::build(tree)
    }

    fn circular_layout(tree: &Tree) -> Option<Self> {
        circular::build(tree)
    }

    fn radial_layout(tree: &Tree) -> Option<Self> {
        radial::build(tree)
    }

    fn slanted_layout(tree: &Tree) -> Option<Self> {
        slanted::build(tree)
    }

    fn cladogram_layout(tree: &Tree) -> Option<Self> {
        cladogram::build(tree)
    }

    fn phylogram_layout(tree: &Tree) -> Option<Self> {
        phylogram::build(tree)
    }

    fn daylight_layout(tree: &Tree) -> Option<Self> {
        daylight::build(tree)
    }

    /// Calculate tip label bounds and adjust layout dimensions to include them
    pub fn with_tip_labels(
        mut self,
        tree: &Tree,
        show_tip_labels: bool,
        tip_label_font_size: f32,
        _tip_label_font_family: FontFamily,
    ) -> Self {
        if !show_tip_labels {
            return self;
        }

        // Use simple bounds calculation that doesn't require egui context
        let bounds = self.calculate_tip_label_bounds_simple(tree, tip_label_font_size);

        // Calculate how much extra space we need for labels
        let tree_bounds =
            egui::Rect::from_min_max(egui::pos2(0.0, 0.0), egui::pos2(self.width, self.height));

        let label_expansion = bounds.union(tree_bounds);

        // Be more conservative with expansion - limit how much labels can expand the layout
        let max_width_expansion = self.width * 0.6; // Labels can add at most 60% to width
        let max_height_expansion = self.height * 0.3; // Labels can add at most 30% to height

        let width_expansion = (label_expansion.width() - self.width).min(max_width_expansion);
        let height_expansion = (label_expansion.height() - self.height).min(max_height_expansion);

        // Apply conservative expansion
        if width_expansion > 0.0 {
            self.width += width_expansion;
        }
        if height_expansion > 0.0 {
            self.height += height_expansion;
        }

        // Handle negative space more conservatively
        if bounds.min.x < 0.0 || bounds.min.y < 0.0 {
            let offset_x = if bounds.min.x < 0.0 {
                (-bounds.min.x).min(self.width * 0.2)
            } else {
                0.0
            };
            let offset_y = if bounds.min.y < 0.0 {
                (-bounds.min.y).min(self.height * 0.1)
            } else {
                0.0
            };

            if offset_x > 0.0 || offset_y > 0.0 {
                // Shift all positions
                for pos in &mut self.positions {
                    pos.0 += offset_x;
                    pos.1 += offset_y;
                }

                // Update segments
                for segment in &mut self.rect_segments {
                    segment.start.0 += offset_x;
                    segment.start.1 += offset_y;
                    segment.end.0 += offset_x;
                    segment.end.1 += offset_y;
                }

                // Update continuous branches
                for branch in &mut self.continuous_branches {
                    for point in &mut branch.points {
                        point.0 += offset_x;
                        point.1 += offset_y;
                    }
                }

                // Update arc segments
                for arc in &mut self.arc_segments {
                    arc.center.0 += offset_x;
                    arc.center.1 += offset_y;
                }

                // Update layout dimensions
                self.width += offset_x;
                self.height += offset_y;
            }
        }

        self
    }

    // Simple bounds calculation for testing that doesn't require egui context
    fn calculate_tip_label_bounds_simple(
        &self,
        tree: &Tree,
        tip_label_font_size: f32,
    ) -> egui::Rect {
        let mut bounds = egui::Rect::NOTHING;

        for node in &tree.nodes {
            if !node.is_leaf() {
                continue;
            }

            let Some(label) = &node.name else {
                continue;
            };

            let node_pos = self.positions[node.id];
            let label_bounds = self.calculate_single_tip_label_bounds_simple(
                tree,
                node,
                label,
                node_pos,
                tip_label_font_size,
            );

            bounds = bounds.union(label_bounds);
        }

        if bounds == egui::Rect::NOTHING {
            bounds =
                egui::Rect::from_min_max(egui::pos2(0.0, 0.0), egui::pos2(self.width, self.height));
        }

        bounds
    }

    fn calculate_single_tip_label_bounds_simple(
        &self,
        tree: &Tree,
        node: &crate::tree::TreeNode,
        label: &str,
        node_pos: (f32, f32),
        font_size: f32,
    ) -> egui::Rect {
        let node_screen_pos = egui::pos2(node_pos.0, node_pos.1);

        // More conservative approximation to avoid over-expansion
        // Use smaller character width estimate and normalize font size
        let normalized_font_size = font_size.min(16.0); // Cap font size impact
        let char_width = normalized_font_size * 0.45; // More conservative width
        let text_width = (label.len() as f32 * char_width).min(120.0); // Cap maximum label width
        let text_height = normalized_font_size; // Simpler height calculation

        match self.layout_type {
            TreeLayoutType::Circular => {
                // Calculate bounds for circular layout with rotation
                let angle = self.calculate_circular_label_angle(tree, node);
                self.calculate_rotated_label_bounds_simple(
                    node_screen_pos,
                    text_width,
                    text_height,
                    angle,
                    8.0,
                ) // Reduced offset
            }
            TreeLayoutType::Radial | TreeLayoutType::Daylight => {
                // Calculate bounds for radial and daylight layout with rotation
                let angle = self.calculate_radial_label_angle(tree, node);
                self.calculate_rotated_label_bounds_simple(
                    node_screen_pos,
                    text_width,
                    text_height,
                    angle,
                    6.0,
                ) // Reduced offset
            }
            _ => {
                // For other layouts, labels are placed to the right
                let text_pos = node_screen_pos + egui::vec2(6.0, 0.0); // Reduced offset
                egui::Rect::from_min_size(
                    text_pos + egui::vec2(0.0, -text_height * 0.5),
                    egui::vec2(text_width, text_height),
                )
            }
        }
    }

    fn calculate_rotated_label_bounds_simple(
        &self,
        node_pos: egui::Pos2,
        text_width: f32,
        text_height: f32,
        angle: f32,
        offset_distance: f32,
    ) -> egui::Rect {
        // Calculate label position
        let direction_x = angle.cos();
        let direction_y = angle.sin();
        let label_offset = egui::vec2(offset_distance * direction_x, offset_distance * direction_y);
        let label_pos = node_pos + label_offset;

        // Handle text rotation and anchor
        let angle_deg = angle.to_degrees();
        let normalized_angle_deg = if angle_deg < 0.0 {
            angle_deg + 360.0
        } else {
            angle_deg
        };

        let (text_rotation_angle, text_anchor) =
            if normalized_angle_deg > 90.0 && normalized_angle_deg < 270.0 {
                (angle + std::f32::consts::PI, egui::Align2::RIGHT_CENTER)
            } else {
                (angle, egui::Align2::LEFT_CENTER)
            };

        // Calculate rotated text bounds
        self.calculate_rotated_text_bounds(
            label_pos,
            egui::vec2(text_width, text_height),
            text_rotation_angle,
            text_anchor,
        )
    }

    fn calculate_circular_label_angle(&self, tree: &Tree, node: &crate::tree::TreeNode) -> f32 {
        const EPS2: f32 = 1e-10;
        let child = self.positions[node.id];

        let angle_if_non_degenerate = |dx: f32, dy: f32| -> Option<f32> {
            if (dx * dx + dy * dy) > EPS2 {
                Some(dy.atan2(dx))
            } else {
                None
            }
        };

        // 1) Prefer terminal segment from continuous branch geometry.
        if let Some(branch) = self.continuous_branches.iter().find(|b| b.child == node.id) {
            if branch.points.len() >= 2 {
                for p in branch.points.iter().skip(1) {
                    if let Some(angle) = angle_if_non_degenerate(child.0 - p.0, child.1 - p.1) {
                        return angle;
                    }
                }
            }
        }

        // 2) Direct parent->child direction.
        if let Some(parent_id) = node.parent {
            let parent = self.positions[parent_id];
            if let Some(angle) = angle_if_non_degenerate(child.0 - parent.0, child.1 - parent.1) {
                return angle;
            }

            // Degenerate parent->child: fan out siblings around parent outward direction.
            let center = (self.width * 0.5, self.height * 0.5);
            let base = angle_if_non_degenerate(parent.0 - center.0, parent.1 - center.1)
                .or_else(|| {
                    let mut anc = tree.nodes[parent_id].parent;
                    while let Some(anc_id) = anc {
                        let p = self.positions[anc_id];
                        if let Some(a) = angle_if_non_degenerate(parent.0 - p.0, parent.1 - p.1) {
                            return Some(a);
                        }
                        anc = tree.nodes[anc_id].parent;
                    }
                    None
                })
                .unwrap_or(0.0);
            if let Some(fanned) = Self::sibling_fanout_angle(tree, parent_id, node.id, base) {
                return fanned;
            }
        }

        // 3) Outward direction from layout center.
        let center = (self.width * 0.5, self.height * 0.5);
        if let Some(angle) = angle_if_non_degenerate(child.0 - center.0, child.1 - center.1) {
            return angle;
        }

        // 4) Walk up ancestors until a non-overlapping point is found.
        let mut anc = node.parent;
        while let Some(anc_id) = anc {
            let p = self.positions[anc_id];
            if let Some(angle) = angle_if_non_degenerate(child.0 - p.0, child.1 - p.1) {
                return angle;
            }
            anc = tree.nodes[anc_id].parent;
        }

        // 5) Hard fallback.
        0.0
    }

    fn calculate_radial_label_angle(&self, tree: &Tree, node: &crate::tree::TreeNode) -> f32 {
        const EPS2: f32 = 1e-10;
        let child = self.positions[node.id];
        let angle_if_non_degenerate = |dx: f32, dy: f32| -> Option<f32> {
            if (dx * dx + dy * dy) > EPS2 {
                Some(dy.atan2(dx))
            } else {
                None
            }
        };

        // 1) Direct parent->child direction.
        if let Some(parent_id) = node.parent {
            let parent = self.positions[parent_id];
            if let Some(angle) = angle_if_non_degenerate(child.0 - parent.0, child.1 - parent.1) {
                return angle;
            }

            // Degenerate parent->child: fan out siblings around parent outward direction.
            let center = (self.width * 0.5, self.height * 0.5);
            let base = angle_if_non_degenerate(parent.0 - center.0, parent.1 - center.1)
                .or_else(|| {
                    let mut anc = tree.nodes[parent_id].parent;
                    while let Some(anc_id) = anc {
                        let p = self.positions[anc_id];
                        if let Some(a) = angle_if_non_degenerate(parent.0 - p.0, parent.1 - p.1) {
                            return Some(a);
                        }
                        anc = tree.nodes[anc_id].parent;
                    }
                    None
                })
                .unwrap_or(0.0);
            if let Some(fanned) = Self::sibling_fanout_angle(tree, parent_id, node.id, base) {
                return fanned;
            }
        }

        // 2) Outward direction from layout center.
        let center = (self.width * 0.5, self.height * 0.5);
        if let Some(angle) = angle_if_non_degenerate(child.0 - center.0, child.1 - center.1) {
            return angle;
        }

        // 3) Walk up ancestors until a non-overlapping point is found.
        let mut anc = node.parent;
        while let Some(anc_id) = anc {
            let p = self.positions[anc_id];
            if let Some(angle) = angle_if_non_degenerate(child.0 - p.0, child.1 - p.1) {
                return angle;
            }
            anc = tree.nodes[anc_id].parent;
        }

        // 4) Hard fallback.
        0.0
    }

    fn sibling_fanout_angle(
        tree: &Tree,
        parent_id: NodeId,
        node_id: NodeId,
        base_angle: f32,
    ) -> Option<f32> {
        let siblings = &tree.nodes[parent_id].children;
        if siblings.len() <= 1 {
            return None;
        }
        let idx = siblings.iter().position(|&id| id == node_id)? as f32;
        let n = siblings.len() as f32;
        let spread_total = ((20.0f32).to_radians() * (n - 1.0)).min((120.0f32).to_radians());
        let step = if n > 1.0 {
            spread_total / (n - 1.0)
        } else {
            0.0
        };
        let offset = -0.5 * spread_total + idx * step;
        Some(base_angle + offset)
    }

    fn calculate_rotated_text_bounds(
        &self,
        pos: egui::Pos2,
        text_size: egui::Vec2,
        angle: f32,
        anchor: egui::Align2,
    ) -> egui::Rect {
        let cos_a = angle.cos();
        let sin_a = angle.sin();

        let corners = match anchor {
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

        let mut min_x = f32::INFINITY;
        let mut max_x = f32::NEG_INFINITY;
        let mut min_y = f32::INFINITY;
        let mut max_y = f32::NEG_INFINITY;

        for corner in corners.iter() {
            let rotated_x = corner.x * cos_a - corner.y * sin_a;
            let rotated_y = corner.x * sin_a + corner.y * cos_a;

            min_x = min_x.min(rotated_x);
            max_x = max_x.max(rotated_x);
            min_y = min_y.min(rotated_y);
            max_y = max_y.max(rotated_y);
        }

        egui::Rect::from_min_max(
            egui::pos2(pos.x + min_x, pos.y + min_y),
            egui::pos2(pos.x + max_x, pos.y + max_y),
        )
    }
}

pub(super) fn normalize_positions(positions: &mut [(f32, f32)]) -> (f32, f32) {
    if positions.is_empty() {
        return (1.0, 1.0);
    }

    let mut min_x = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_y = f32::NEG_INFINITY;

    for (x, y) in positions.iter() {
        if x.is_finite() {
            min_x = min_x.min(*x);
            max_x = max_x.max(*x);
        }
        if y.is_finite() {
            min_y = min_y.min(*y);
            max_y = max_y.max(*y);
        }
    }

    if !min_x.is_finite() || !max_x.is_finite() || !min_y.is_finite() || !max_y.is_finite() {
        return (1.0, 1.0);
    }

    let shift_x = -min_x;
    let shift_y = -min_y;

    for (x, y) in positions.iter_mut() {
        *x += shift_x;
        *y += shift_y;
    }

    let width = (max_x - min_x).abs().max(1e-6);
    let height = (max_y - min_y).abs().max(1e-6);

    (width, height)
}

#[cfg(test)]
mod tests {
    use super::*;
    use phylotree::tree::Tree as PhyloTree;

    fn create_test_tree() -> Tree {
        let newick = "(Leaf1:1.0,Leaf2:1.5);".to_string();
        let phylo = PhyloTree::from_newick(&newick).expect("valid test newick");
        Tree::new(1, Some("Test Tree".to_string()), newick, phylo)
    }

    #[test]
    fn test_rectangular_layout() {
        let tree = create_test_tree();
        let layout = TreeLayout::from_tree(&tree, TreeLayoutType::Rectangular).unwrap();

        assert!(layout.width > 0.0);
        assert!(layout.height > 0.0);
        assert_eq!(layout.leaf_count, 2);
        assert_eq!(layout.positions.len(), 3);
        assert_eq!(layout.edges.len(), 2);
        assert!(!layout.rect_segments.is_empty());
    }

    #[test]
    fn test_circular_layout() {
        let tree = create_test_tree();
        let layout = TreeLayout::from_tree(&tree, TreeLayoutType::Circular).unwrap();

        assert!(layout.width > 0.0);
        assert!(layout.height > 0.0);
        assert_eq!(layout.leaf_count, 2);
        assert_eq!(layout.positions.len(), 3);
        assert_eq!(layout.edges.len(), 2);

        // Debug: Check centering
        let mut min_x = f32::INFINITY;
        let mut max_x = f32::NEG_INFINITY;
        let mut min_y = f32::INFINITY;
        let mut max_y = f32::NEG_INFINITY;

        for pos in &layout.positions {
            min_x = min_x.min(pos.0);
            max_x = max_x.max(pos.0);
            min_y = min_y.min(pos.1);
            max_y = max_y.max(pos.1);
        }

        let center_x = (min_x + max_x) * 0.5;
        let center_y = (min_y + max_y) * 0.5;
        let expected_center_x = layout.width * 0.5;
        let expected_center_y = layout.height * 0.5;

        // Debug output for coordinate analysis
        println!("=== Circular Layout Coordinate Analysis ===");
        println!(
            "Layout dimensions: width={:.3}, height={:.3}",
            layout.width, layout.height
        );
        println!(
            "Coordinate range: x=[{:.3}, {:.3}], y=[{:.3}, {:.3}]",
            min_x, max_x, min_y, max_y
        );
        println!("Actual center: ({:.3}, {:.3})", center_x, center_y);
        println!(
            "Expected center: ({:.3}, {:.3})",
            expected_center_x, expected_center_y
        );
        println!(
            "Center offset: ({:.3}, {:.3})",
            center_x - expected_center_x,
            center_y - expected_center_y
        );

        // Verify centering (allow small floating point errors)
        assert!(
            (center_x - expected_center_x).abs() < 1e-3,
            "Center X mismatch: actual={:.3}, expected={:.3}",
            center_x,
            expected_center_x
        );
        assert!(
            (center_y - expected_center_y).abs() < 1e-3,
            "Center Y mismatch: actual={:.3}, expected={:.3}",
            center_y,
            expected_center_y
        );

        // Circular layout should have positions around center
        for pos in layout.positions {
            assert!(pos.0 >= -1e-3);
            assert!(pos.0 <= layout.width + 1e-3);
            assert!(pos.1 >= -1e-3);
            assert!(pos.1 <= layout.height + 1e-3);
        }
    }

    #[test]
    fn test_slanted_layout() {
        let tree = create_test_tree();
        let layout = TreeLayout::from_tree(&tree, TreeLayoutType::Slanted).unwrap();

        assert!(layout.width > 0.0);
        assert!(layout.height > 0.0);
        assert_eq!(layout.leaf_count, 2);
        assert_eq!(layout.positions.len(), 3);
        assert_eq!(layout.edges.len(), 2);
    }

    #[test]
    fn test_cladogram_layout() {
        let tree = create_test_tree();
        let layout = TreeLayout::from_tree(&tree, TreeLayoutType::Cladogram).unwrap();

        assert!(layout.width > 0.0);
        assert!(layout.height > 0.0);
        assert_eq!(layout.leaf_count, 2);
        assert_eq!(layout.positions.len(), 3);
        assert_eq!(layout.edges.len(), 2);
    }

    #[test]
    fn test_layout_with_tip_labels() {
        let tree = create_test_tree();

        // Test layout without tip labels
        let layout_without_labels =
            TreeLayout::from_tree(&tree, TreeLayoutType::Rectangular).unwrap();
        let original_width = layout_without_labels.width;
        let original_height = layout_without_labels.height;

        // Test layout with tip labels
        let layout_with_labels = layout_without_labels.clone().with_tip_labels(
            &tree,
            true,
            13.0,
            egui::FontFamily::Proportional,
        );

        // Layout with labels should be larger or equal in size
        assert!(layout_with_labels.width >= original_width);
        assert!(layout_with_labels.height >= original_height);

        // For rectangular layout with tip labels, width should be expanded but not too much
        // The conservative approach should limit expansion
        assert!(layout_with_labels.width > original_width);
        assert!(layout_with_labels.width <= original_width * 1.7); // Should not expand more than 70%
    }

    #[test]
    fn test_layout_with_tip_labels_disabled() {
        let tree = create_test_tree();

        let layout = TreeLayout::from_tree(&tree, TreeLayoutType::Rectangular).unwrap();
        let original_width = layout.width;
        let original_height = layout.height;

        // Test with tip labels disabled
        let layout_no_labels = layout.clone().with_tip_labels(
            &tree,
            false, // disabled
            13.0,
            egui::FontFamily::Proportional,
        );

        // Should remain the same size
        assert_eq!(layout_no_labels.width, original_width);
        assert_eq!(layout_no_labels.height, original_height);
    }

    #[test]
    fn test_daylight_layout() {
        let tree = create_test_tree();
        let layout = TreeLayout::from_tree(&tree, TreeLayoutType::Daylight).unwrap();

        assert!(layout.width > 0.0);
        assert!(layout.height > 0.0);
        assert_eq!(layout.leaf_count, 2);
        assert_eq!(layout.positions.len(), 3);
        assert_eq!(layout.edges.len(), 2);
        assert_eq!(layout.layout_type, TreeLayoutType::Daylight);

        // Daylight layout should have positions within bounds
        for pos in &layout.positions {
            assert!(pos.0 >= -1e-3);
            assert!(pos.0 <= layout.width + 1e-3);
            assert!(pos.1 >= -1e-3);
            assert!(pos.1 <= layout.height + 1e-3);
        }
    }
}
