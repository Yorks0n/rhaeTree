use super::{
    ContinuousBranch, NodeId, RectSegment, RectSegmentKind, Tree, TreeLayout, TreeLayoutType,
    DEFAULT_BRANCH_LENGTH, ROOT_LENGTH_PROPORTION,
};
use crate::tree::TreeNode;

pub(super) struct RectangularLayoutData {
    pub positions: Vec<(f32, f32)>,
    pub edges: Vec<(NodeId, NodeId)>,
    pub segments: Vec<RectSegment>,
    pub max_x: f32,
    pub tip_count: usize,
}

pub(super) fn compute_base(tree: &Tree) -> Option<RectangularLayoutData> {
    let root_id = tree.root?;
    let tip_count = tree.leaf_count().max(1);

    let mut positions = vec![(0.0f32, 0.0f32); tree.nodes.len()];
    let mut edges = Vec::with_capacity(tree.nodes.len().saturating_mul(2));

    let total_height = calculate_tree_height(tree, root_id, 0.0);
    let root_length = total_height * ROOT_LENGTH_PROPORTION;

    let mut state = RectangularState {
        next_tip_index: 0,
        max_x: root_length,
        min_x: root_length,
        segments: Vec::new(),
    };

    assign_rectangular_positions(
        tree,
        root_id,
        root_length,
        &mut positions,
        &mut edges,
        &mut state,
    );

    if state.min_x.is_finite() && state.min_x > 0.0 {
        let shift = state.min_x;
        for (index, pos) in positions.iter_mut().enumerate() {
            if index == root_id || tree.nodes[index].parent.is_some() {
                pos.0 -= shift;
            }
        }
        for segment in &mut state.segments {
            segment.start.0 -= shift;
            segment.end.0 -= shift;
        }
        state.max_x -= shift;
        state.min_x = 0.0;
    }

    Some(RectangularLayoutData {
        positions,
        edges,
        segments: state.segments,
        max_x: state.max_x,
        tip_count,
    })
}

pub(super) fn build(tree: &Tree) -> Option<TreeLayout> {
    let root_id = tree.root?;
    let mut data = compute_base(tree)?;
    let root_y = data.positions[root_id].1;

    data.segments.push(RectSegment {
        start: (0.0, root_y),
        end: (data.positions[root_id].0, root_y),
        parent: root_id,
        child: None,
        kind: RectSegmentKind::Horizontal,
    });

    let layout_height = if data.tip_count > 1 {
        (data.tip_count - 1) as f32
    } else {
        1.0
    };

    // Create continuous branches from segments
    let mut continuous_branches = Vec::new();

    // Group segments by child to create continuous branch paths
    for (parent, child) in &data.edges {
        let parent_pos = data.positions[*parent];
        let child_pos = data.positions[*child];

        // For rectangular layout, branches consist of:
        // 1. Child position
        // 2. Corner/shoulder point (parent_x, child_y)
        // 3. Parent position (only if it's vertically different from shoulder)

        let mut points = Vec::new();
        points.push(child_pos);

        // Add shoulder/corner point if positions differ
        let shoulder = (parent_pos.0, child_pos.1);
        if shoulder != child_pos {
            points.push(shoulder);
        }

        // Only add parent position for vertical segments
        // (when parent has multiple children)
        if parent_pos.1 != child_pos.1 && parent_pos != shoulder {
            // Check if parent has multiple children by looking for vertical segment
            let has_vertical = data
                .segments
                .iter()
                .any(|seg| seg.parent == *parent && seg.kind == RectSegmentKind::Vertical);

            if has_vertical {
                // Add intermediate points along vertical line if needed
                // This ensures smooth selection along the entire branch
                points.push(parent_pos);
            }
        }

        continuous_branches.push(ContinuousBranch {
            points,
            parent: *parent,
            child: *child,
        });
    }

    Some(TreeLayout {
        positions: data.positions,
        edges: data.edges,
        width: data.max_x.max(1e-6),
        height: layout_height.max(1e-6),
        leaf_count: data.tip_count,
        layout_type: TreeLayoutType::Rectangular,
        rect_segments: data.segments,
        arc_segments: Vec::new(),
        continuous_branches,
    })
}

struct RectangularState {
    next_tip_index: usize,
    max_x: f32,
    min_x: f32,
    segments: Vec<RectSegment>,
}

fn assign_rectangular_positions(
    tree: &Tree,
    node_id: NodeId,
    x_pos: f32,
    positions: &mut [(f32, f32)],
    edges: &mut Vec<(NodeId, NodeId)>,
    state: &mut RectangularState,
) -> f32 {
    let node = &tree.nodes[node_id];

    let y_pos = if node.children.is_empty() {
        let y = state.next_tip_index as f32;
        state.next_tip_index += 1;
        y
    } else {
        let mut first_y = f32::MAX;
        let mut last_y = f32::MIN;
        let mut child_segments = Vec::with_capacity(node.children.len());

        let mut children = node.children.clone();
        if should_rotate(node) {
            children.reverse();
        }

        for child_id in children {
            edges.push((node_id, child_id));

            let branch_length = tree.nodes[child_id]
                .length
                .map(|value| value as f32)
                .unwrap_or(DEFAULT_BRANCH_LENGTH);
            let child_x = x_pos + branch_length;

            let child_y =
                assign_rectangular_positions(tree, child_id, child_x, positions, edges, state);

            first_y = first_y.min(child_y);
            last_y = last_y.max(child_y);
            child_segments.push((child_id, child_x, child_y));
        }

        if !child_segments.is_empty() {
            if child_segments.len() > 1 {
                state.segments.push(RectSegment {
                    start: (x_pos, first_y),
                    end: (x_pos, last_y),
                    parent: node_id,
                    child: None,
                    kind: RectSegmentKind::Vertical,
                });
            }

            for (child_id, child_x, child_y) in &child_segments {
                state.segments.push(RectSegment {
                    start: (x_pos, *child_y),
                    end: (*child_x, *child_y),
                    parent: node_id,
                    child: Some(*child_id),
                    kind: RectSegmentKind::Horizontal,
                });
            }
        }

        if first_y.is_finite() && last_y.is_finite() {
            (first_y + last_y) / 2.0
        } else {
            0.0
        }
    };

    positions[node_id] = (x_pos, y_pos);
    state.max_x = state.max_x.max(x_pos);
    state.min_x = state.min_x.min(x_pos);

    y_pos
}

fn should_rotate(node: &TreeNode) -> bool {
    node.get_attribute("!rotate")
        .map(|value| {
            let lower = value.trim().to_ascii_lowercase();
            matches!(lower.as_str(), "true" | "1" | "yes" | "on")
        })
        .unwrap_or(false)
}

fn calculate_tree_height(tree: &Tree, node_id: NodeId, current_height: f32) -> f32 {
    let node = &tree.nodes[node_id];
    let mut max_height = current_height;

    for &child_id in &node.children {
        let branch_length = tree.nodes[child_id]
            .length
            .map(|value| value as f32)
            .unwrap_or(DEFAULT_BRANCH_LENGTH);
        let child_height = calculate_tree_height(tree, child_id, current_height + branch_length);
        max_height = max_height.max(child_height);
    }

    max_height
}
