use super::{NodeId, Tree};

mod circular;
mod cladogram;
mod daylight;
mod phylogram;
mod radial;
mod rectangular;
mod slanted;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

    /// Default layout (rectangular phylogram)
    pub fn from_tree_default(tree: &Tree) -> Option<Self> {
        Self::from_tree(tree, TreeLayoutType::Rectangular)
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
        println!("Layout dimensions: width={:.3}, height={:.3}", layout.width, layout.height);
        println!("Coordinate range: x=[{:.3}, {:.3}], y=[{:.3}, {:.3}]", min_x, max_x, min_y, max_y);
        println!("Actual center: ({:.3}, {:.3})", center_x, center_y);
        println!("Expected center: ({:.3}, {:.3})", expected_center_x, expected_center_y);
        println!("Center offset: ({:.3}, {:.3})", center_x - expected_center_x, center_y - expected_center_y);

        // Verify centering (allow small floating point errors)
        assert!((center_x - expected_center_x).abs() < 1e-3,
            "Center X mismatch: actual={:.3}, expected={:.3}", center_x, expected_center_x);
        assert!((center_y - expected_center_y).abs() < 1e-3,
            "Center Y mismatch: actual={:.3}, expected={:.3}", center_y, expected_center_y);

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
}
