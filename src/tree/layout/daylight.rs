use super::{normalize_positions, NodeId, Tree, TreeLayout, TreeLayoutType, DEFAULT_BRANCH_LENGTH};
use std::collections::VecDeque;

const DAYLIGHT_MAX_ITERATIONS: usize = 5;
const DAYLIGHT_MIN_AVG_CHANGE: f32 = 0.05 * std::f32::consts::PI;
const ANGLE_EPSILON: f32 = 1e-6;

#[derive(Clone, Copy, Debug, Default)]
struct DaylightNode {
    start_angle: f32,
    end_angle: f32,
    radius: f32,
    min_tip_angle: f32,
    max_tip_angle: f32,
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

    let mut geometry = vec![DaylightNode::default(); node_count];
    initialize_daylight_geometry(
        tree,
        root_id,
        0.0,
        0.0,
        std::f32::consts::TAU,
        &mut geometry,
        &leaf_counts,
    );

    let breadth_first = breadth_first_nodes(tree, root_id);
    let internal_nodes: Vec<NodeId> = breadth_first
        .into_iter()
        .filter(|&id| !tree.nodes[id].children.is_empty())
        .collect();

    if !internal_nodes.is_empty() {
        for _ in 0..DAYLIGHT_MAX_ITERATIONS {
            let mut total_change = 0.0f32;
            for node_id in &internal_nodes {
                total_change += apply_daylight_adjustment(tree, *node_id, &mut geometry);
            }

            let average_change = total_change / internal_nodes.len() as f32;
            if average_change <= DAYLIGHT_MIN_AVG_CHANGE {
                break;
            }
        }
    }

    for (node_id, geom) in geometry.iter().enumerate() {
        let mid_angle = (geom.start_angle + geom.end_angle) * 0.5;
        let radius = geom.radius;
        if radius <= ANGLE_EPSILON {
            positions[node_id] = (0.0, 0.0);
        } else {
            positions[node_id] = (radius * mid_angle.cos(), radius * mid_angle.sin());
        }
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

fn initialize_daylight_geometry(
    tree: &Tree,
    node_id: NodeId,
    parent_radius: f32,
    start_angle: f32,
    end_angle: f32,
    geometry: &mut [DaylightNode],
    leaf_counts: &[usize],
) {
    let node = &tree.nodes[node_id];
    let radius = if node.parent.is_some() {
        let branch_length = node
            .length
            .map(|value| value as f32)
            .unwrap_or(DEFAULT_BRANCH_LENGTH);
        parent_radius + branch_length
    } else {
        0.0
    };

    geometry[node_id] = DaylightNode {
        start_angle,
        end_angle,
        radius,
        min_tip_angle: 0.0,
        max_tip_angle: 0.0,
    };

    if node.children.is_empty() {
        let mid = (start_angle + end_angle) * 0.5;
        geometry[node_id].min_tip_angle = mid;
        geometry[node_id].max_tip_angle = mid;
        return;
    }

    let span = (end_angle - start_angle).max(ANGLE_EPSILON);
    let total_leaves = leaf_counts[node_id].max(1) as f32;

    let mut current_start = start_angle;
    let mut min_tip = f32::INFINITY;
    let mut max_tip = f32::NEG_INFINITY;
    for &child_id in &node.children {
        let fraction = leaf_counts[child_id].max(1) as f32 / total_leaves;
        let child_span = span * fraction;
        let child_start = current_start;
        let child_end = child_start + child_span;

        initialize_daylight_geometry(
            tree,
            child_id,
            radius,
            child_start,
            child_end,
            geometry,
            leaf_counts,
        );

        current_start += child_span;

        let child_geom = geometry[child_id];
        min_tip = min_tip.min(child_geom.min_tip_angle);
        max_tip = max_tip.max(child_geom.max_tip_angle);
    }

    if !min_tip.is_finite() || !max_tip.is_finite() {
        let mid = (start_angle + end_angle) * 0.5;
        min_tip = mid;
        max_tip = mid;
    }

    geometry[node_id].min_tip_angle = min_tip;
    geometry[node_id].max_tip_angle = max_tip;
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

fn apply_daylight_adjustment(tree: &Tree, node_id: NodeId, geometry: &mut [DaylightNode]) -> f32 {
    let node = &tree.nodes[node_id];
    let child_count = node.children.len();
    if child_count <= 1 {
        return 0.0;
    }

    let parent_geom = geometry[node_id];
    let mut child_spans = Vec::with_capacity(child_count);
    for &child_id in &node.children {
        let geom = geometry[child_id];
        let span = (geom.max_tip_angle - geom.min_tip_angle).max(ANGLE_EPSILON);
        child_spans.push(span);
    }

    let total_span: f32 = child_spans.iter().copied().sum();
    let mut available = parent_geom.end_angle - parent_geom.start_angle - total_span;
    if available < 0.0 {
        available = 0.0;
    }

    let gap = if available > 0.0 {
        available / child_count as f32
    } else {
        0.0
    };

    let mut current = parent_geom.start_angle;
    let mut max_change = 0.0f32;
    for (index, &child_id) in node.children.iter().enumerate() {
        current += gap * 0.5;
        let span = child_spans[index];
        let new_start = current;
        let new_end = new_start + span;
        current = new_end + gap * 0.5;

        let old_geom = geometry[child_id];
        let old_mid = (old_geom.start_angle + old_geom.end_angle) * 0.5;
        let new_mid = (new_start + new_end) * 0.5;
        let delta = new_mid - old_mid;

        if delta.abs() > ANGLE_EPSILON {
            rotate_subtree(tree, child_id, delta, geometry);
        }

        geometry[child_id].start_angle = new_start;
        geometry[child_id].end_angle = new_end;

        max_change = max_change.max(delta.abs());
    }

    if let (Some(first_child), Some(last_child)) = (
        node.children.first().copied(),
        node.children.last().copied(),
    ) {
        geometry[node_id].min_tip_angle = geometry[first_child].min_tip_angle;
        geometry[node_id].max_tip_angle = geometry[last_child].max_tip_angle;
    }

    max_change
}

fn rotate_subtree(tree: &Tree, node_id: NodeId, delta: f32, geometry: &mut [DaylightNode]) {
    if delta.abs() <= ANGLE_EPSILON {
        return;
    }

    rotate_subtree_inner(tree, node_id, delta, geometry);
}

fn rotate_subtree_inner(tree: &Tree, node_id: NodeId, delta: f32, geometry: &mut [DaylightNode]) {
    let geom = &mut geometry[node_id];
    geom.start_angle += delta;
    geom.end_angle += delta;
    geom.min_tip_angle += delta;
    geom.max_tip_angle += delta;

    for &child in &tree.nodes[node_id].children {
        rotate_subtree_inner(tree, child, delta, geometry);
    }
}
