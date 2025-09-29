use super::{Tree, TreeLayout, TreeLayoutType, DEFAULT_BRANCH_LENGTH};
use crate::tree::{NodeId, TreeNode};

pub(super) fn build(tree: &Tree) -> Option<TreeLayout> {
    let root_id = tree.root?;
    let node_count = tree.nodes.len();
    if node_count == 0 {
        return None;
    }

    let mut positions = vec![(0.0f32, 0.0f32); node_count];
    let mut edges = Vec::with_capacity(node_count.saturating_mul(2));
    let mut leaf_counts = vec![0usize; node_count];
    compute_leaf_counts(tree, root_id, &mut leaf_counts);

    assign_radial_positions(
        tree,
        root_id,
        0.0,
        std::f32::consts::TAU,
        (0.0, 0.0),
        &mut positions,
        &mut edges,
        &leaf_counts,
    );

    let mut min_x = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_y = f32::NEG_INFINITY;

    for &(x, y) in &positions {
        if x.is_finite() {
            min_x = min_x.min(x);
            max_x = max_x.max(x);
        }
        if y.is_finite() {
            min_y = min_y.min(y);
            max_y = max_y.max(y);
        }
    }

    if !min_x.is_finite() || !max_x.is_finite() || !min_y.is_finite() || !max_y.is_finite() {
        return None;
    }

    let width = (max_x - min_x).abs().max(DEFAULT_BRANCH_LENGTH);
    let height = (max_y - min_y).abs().max(DEFAULT_BRANCH_LENGTH);

    let center_x = (min_x + max_x) * 0.5;
    let center_y = (min_y + max_y) * 0.5;

    for pos in &mut positions {
        pos.0 = (pos.0 - center_x) + width * 0.5;
        pos.1 = (pos.1 - center_y) + height * 0.5;
    }

    Some(TreeLayout {
        positions,
        edges,
        width,
        height,
        leaf_count: tree.leaf_count().max(1),
        layout_type: TreeLayoutType::Radial,
        rect_segments: Vec::new(),
        arc_segments: Vec::new(),
        continuous_branches: Vec::new(),
    })
}

fn assign_radial_positions(
    tree: &Tree,
    node_id: NodeId,
    start_angle: f32,
    end_angle: f32,
    parent_position: (f32, f32),
    positions: &mut [(f32, f32)],
    edges: &mut Vec<(NodeId, NodeId)>,
    leaf_counts: &[usize],
) {
    let node = &tree.nodes[node_id];

    let mut span = end_angle - start_angle;
    if span <= f32::EPSILON {
        span = std::f32::consts::TAU;
    }

    let mid_angle = start_angle + span * 0.5;
    let branch_length = node
        .length
        .map(|value| value as f32)
        .unwrap_or(if node.parent.is_some() {
            DEFAULT_BRANCH_LENGTH
        } else {
            0.0
        });

    let direction = (mid_angle.cos(), mid_angle.sin());
    let position = if node.parent.is_some() {
        (
            parent_position.0 + branch_length * direction.0,
            parent_position.1 + branch_length * direction.1,
        )
    } else {
        parent_position
    };

    positions[node_id] = position;

    if node.children.is_empty() {
        return;
    }

    let mut children = node.children.clone();
    if should_rotate(node) {
        children.reverse();
    }

    let total_leaves: usize = children
        .iter()
        .map(|child| leaf_counts[*child].max(1))
        .sum::<usize>()
        .max(1);

    let mut current_angle = start_angle;
    for child_id in children {
        edges.push((node_id, child_id));

        let fraction = leaf_counts[child_id].max(1) as f32 / total_leaves as f32;
        let child_span = span * fraction;
        let child_start = current_angle;
        let child_end = current_angle + child_span;

        assign_radial_positions(
            tree,
            child_id,
            child_start,
            child_end,
            position,
            positions,
            edges,
            leaf_counts,
        );

        current_angle = child_end;
    }
}

fn compute_leaf_counts(tree: &Tree, node_id: NodeId, counts: &mut [usize]) -> usize {
    let node = &tree.nodes[node_id];
    if node.children.is_empty() {
        counts[node_id] = 1;
        1
    } else {
        let mut total = 0usize;
        for &child in &node.children {
            total += compute_leaf_counts(tree, child, counts);
        }
        let value = total.max(1);
        counts[node_id] = value;
        value
    }
}

fn should_rotate(node: &TreeNode) -> bool {
    node.get_attribute("!rotate")
        .map(|value| {
            let lower = value.trim().to_ascii_lowercase();
            matches!(lower.as_str(), "true" | "1" | "yes" | "on")
        })
        .unwrap_or(false)
}
