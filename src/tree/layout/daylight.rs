use super::{normalize_positions, NodeId, Tree, TreeLayout, TreeLayoutType, DEFAULT_BRANCH_LENGTH};
use std::collections::VecDeque;

const DAYLIGHT_MAX_ITERATIONS: usize = 5;
const DAYLIGHT_MIN_AVG_CHANGE: f32 = 0.05;
const ANGLE_EPSILON: f32 = 1e-6;

#[derive(Clone, Copy, Debug, Default)]
struct DaylightNode {
    x: f32,
    y: f32,
    radius: f32,
    angle: f32,
    left_angle: f32,
    right_angle: f32,
    subtree_arc: f32,
}

#[derive(Debug, Clone)]
struct SubtreeInfo {
    subtree_id: usize,
    left_angle: f32,
    beta: f32, // arc angle occupied by subtree
    nodes: Vec<NodeId>,
}

pub(super) fn build(tree: &Tree) -> Option<TreeLayout> {
    let root_id = tree.root?;
    let node_count = tree.nodes.len();
    if node_count == 0 {
        return None;
    }

    let mut positions = vec![(0.0f32, 0.0f32); node_count];
    let mut edges = Vec::with_capacity(node_count.saturating_mul(2));
    for node in &tree.nodes {
        for &child in &node.children {
            edges.push((node.id, child));
        }
    }

    let mut leaf_counts = vec![0usize; node_count];
    compute_leaf_counts(tree, root_id, &mut leaf_counts);

    // Step 1: Initialize with equal angle (radial) layout
    let mut geometry = vec![DaylightNode::default(); node_count];
    initialize_equal_angle(
        tree,
        root_id,
        0.0,
        std::f32::consts::TAU,
        (0.0, 0.0),
        &mut geometry,
        &leaf_counts,
    );

    // Step 2: Apply daylight algorithm iteratively
    let breadth_first = breadth_first_nodes(tree, root_id);
    let internal_nodes: Vec<NodeId> = breadth_first
        .into_iter()
        .filter(|&id| !tree.nodes[id].children.is_empty())
        .collect();

    if !internal_nodes.is_empty() {
        for iteration in 0..DAYLIGHT_MAX_ITERATIONS {
            let mut total_change = 0.0f32;
            for node_id in &internal_nodes {
                total_change += apply_daylight_to_node(tree, *node_id, &mut geometry);
            }

            let average_change = total_change / internal_nodes.len() as f32;
            if average_change <= DAYLIGHT_MIN_AVG_CHANGE {
                break;
            }
        }
    }

    // Step 3: Extract final positions
    for (node_id, geom) in geometry.iter().enumerate() {
        positions[node_id] = (geom.x, geom.y);
    }

    let (width, height) = normalize_positions(&mut positions);

    Some(TreeLayout {
        positions,
        edges,
        width,
        height,
        leaf_count: tree.leaf_count().max(1),
        layout_type: TreeLayoutType::Daylight,
        rect_segments: Vec::new(),
        arc_segments: Vec::new(),
        continuous_branches: Vec::new(),
    })
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

fn initialize_equal_angle(
    tree: &Tree,
    node_id: NodeId,
    start_angle: f32,
    end_angle: f32,
    parent_position: (f32, f32),
    geometry: &mut [DaylightNode],
    leaf_counts: &[usize],
) {
    let node = &tree.nodes[node_id];

    let mut span = end_angle - start_angle;
    if span <= ANGLE_EPSILON {
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

    let radius = (position.0 * position.0 + position.1 * position.1).sqrt();

    geometry[node_id] = DaylightNode {
        x: position.0,
        y: position.1,
        radius,
        angle: mid_angle,
        left_angle: start_angle,
        right_angle: end_angle,
        subtree_arc: 0.0,
    };

    if node.children.is_empty() {
        geometry[node_id].subtree_arc = 0.0;
        return;
    }

    let total_leaves: usize = node
        .children
        .iter()
        .map(|&child| leaf_counts[child].max(1))
        .sum::<usize>()
        .max(1);

    let mut current_angle = start_angle;
    for &child_id in &node.children {
        let fraction = leaf_counts[child_id].max(1) as f32 / total_leaves as f32;
        let child_span = span * fraction;
        let child_start = current_angle;
        let child_end = current_angle + child_span;

        initialize_equal_angle(
            tree,
            child_id,
            child_start,
            child_end,
            position,
            geometry,
            leaf_counts,
        );

        current_angle = child_end;
    }

    // Calculate subtree arc for internal nodes
    geometry[node_id].subtree_arc = calculate_subtree_arc(tree, node_id, geometry);
}

fn calculate_subtree_arc(tree: &Tree, node_id: NodeId, geometry: &[DaylightNode]) -> f32 {
    let node = &tree.nodes[node_id];
    if node.children.is_empty() {
        return 0.0;
    }

    let mut min_angle = f32::INFINITY;
    let mut max_angle = f32::NEG_INFINITY;

    collect_tip_angles(tree, node_id, geometry, &mut min_angle, &mut max_angle);

    if min_angle.is_finite() && max_angle.is_finite() {
        let mut arc = max_angle - min_angle;
        if arc < 0.0 {
            arc += std::f32::consts::TAU;
        }
        arc
    } else {
        0.0
    }
}

fn collect_tip_angles(
    tree: &Tree,
    node_id: NodeId,
    geometry: &[DaylightNode],
    min_angle: &mut f32,
    max_angle: &mut f32,
) {
    let node = &tree.nodes[node_id];
    if node.children.is_empty() {
        // This is a tip
        let angle = geometry[node_id].angle;
        *min_angle = min_angle.min(angle);
        *max_angle = max_angle.max(angle);
    } else {
        // Recurse to children
        for &child in &node.children {
            collect_tip_angles(tree, child, geometry, min_angle, max_angle);
        }
    }
}

fn breadth_first_nodes(tree: &Tree, root_id: NodeId) -> Vec<NodeId> {
    let mut order = Vec::with_capacity(tree.nodes.len());
    let mut queue = VecDeque::new();
    queue.push_back(root_id);

    while let Some(node_id) = queue.pop_front() {
        order.push(node_id);
        for &child in &tree.nodes[node_id].children {
            queue.push_back(child);
        }
    }

    order
}

fn apply_daylight_to_node(tree: &Tree, node_id: NodeId, geometry: &mut [DaylightNode]) -> f32 {
    let node = &tree.nodes[node_id];
    if node.children.len() <= 2 {
        return 0.0; // Need at least 3 subtrees to apply daylight
    }

    // Get subtree information for each child
    let mut subtrees = Vec::new();
    for (index, &child_id) in node.children.iter().enumerate() {
        let nodes = collect_subtree_nodes(tree, child_id);
        let (left_angle, right_angle) = get_subtree_arc_angles(geometry, &nodes);

        let mut beta = left_angle - right_angle;
        if beta < 0.0 {
            beta += std::f32::consts::TAU;
        }

        subtrees.push(SubtreeInfo {
            subtree_id: index,
            left_angle,
            beta,
            nodes,
        });
    }

    // Sort by left angle
    subtrees.sort_by(|a, b| a.left_angle.partial_cmp(&b.left_angle).unwrap());

    // Calculate total daylight and equal daylight per subtree
    let total_beta: f32 = subtrees.iter().map(|s| s.beta).sum();
    let total_daylight = std::f32::consts::TAU - total_beta;
    let equal_daylight = total_daylight / subtrees.len() as f32;

    // Apply adjustments
    let mut new_left_angle = subtrees[0].left_angle;
    let mut max_change = 0.0f32;

    // Skip first subtree, adjust others
    for i in 1..subtrees.len() {
        new_left_angle += equal_daylight + subtrees[i - 1].beta;
        let adjust_angle = new_left_angle - subtrees[i].left_angle;

        max_change = max_change.max(adjust_angle.abs());

        if adjust_angle.abs() > ANGLE_EPSILON {
            rotate_subtree_nodes(geometry, &subtrees[i].nodes, node_id, adjust_angle);
        }
    }

    max_change
}

fn collect_subtree_nodes(tree: &Tree, root_id: NodeId) -> Vec<NodeId> {
    let mut nodes = Vec::new();
    let mut queue = VecDeque::new();
    queue.push_back(root_id);

    while let Some(node_id) = queue.pop_front() {
        nodes.push(node_id);
        for &child in &tree.nodes[node_id].children {
            queue.push_back(child);
        }
    }

    nodes
}

fn get_subtree_arc_angles(geometry: &[DaylightNode], nodes: &[NodeId]) -> (f32, f32) {
    let mut min_angle = f32::INFINITY;
    let mut max_angle = f32::NEG_INFINITY;

    for &node_id in nodes {
        let angle = geometry[node_id].angle;
        min_angle = min_angle.min(angle);
        max_angle = max_angle.max(angle);
    }

    if min_angle.is_finite() && max_angle.is_finite() {
        (max_angle, min_angle) // left is max, right is min
    } else {
        (0.0, 0.0)
    }
}

fn rotate_subtree_nodes(
    geometry: &mut [DaylightNode],
    nodes: &[NodeId],
    pivot_id: NodeId,
    delta_angle: f32,
) {
    let pivot = geometry[pivot_id];
    let pivot_pos = (pivot.x, pivot.y);

    for &node_id in nodes {
        let old_geom = &mut geometry[node_id];

        // Calculate relative position from pivot
        let rel_x = old_geom.x - pivot_pos.0;
        let rel_y = old_geom.y - pivot_pos.1;

        // Rotate around pivot
        let cos_delta = delta_angle.cos();
        let sin_delta = delta_angle.sin();
        let new_rel_x = rel_x * cos_delta - rel_y * sin_delta;
        let new_rel_y = rel_x * sin_delta + rel_y * cos_delta;

        // Update position
        old_geom.x = pivot_pos.0 + new_rel_x;
        old_geom.y = pivot_pos.1 + new_rel_y;

        // Update angle
        old_geom.angle += delta_angle;
        old_geom.left_angle += delta_angle;
        old_geom.right_angle += delta_angle;

        // Update radius (should remain the same, but recalculate for precision)
        old_geom.radius = (old_geom.x * old_geom.x + old_geom.y * old_geom.y).sqrt();
    }
}
