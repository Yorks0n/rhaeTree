use super::{NodeId, Tree, TreeLayout, TreeLayoutType};

pub(super) fn build(tree: &Tree) -> Option<TreeLayout> {
    let root_id = tree.root?;
    let mut positions = vec![(0.0f32, 0.0f32); tree.nodes.len()];
    let mut edges = Vec::with_capacity(tree.nodes.len().saturating_mul(2));
    let mut max_depth = 0usize;
    let mut leaf_index = 0usize;

    assign_cladogram_positions(
        tree,
        root_id,
        0,
        &mut positions,
        &mut edges,
        &mut leaf_index,
        &mut max_depth,
    );

    let leaf_count = leaf_index.max(1);
    let width = max_depth as f32;
    let height = if leaf_count > 1 {
        (leaf_count - 1) as f32
    } else {
        1.0
    };

    Some(TreeLayout {
        positions,
        edges,
        width: width.max(1e-6),
        height,
        leaf_count,
        layout_type: TreeLayoutType::Cladogram,
        rect_segments: Vec::new(),
        arc_segments: Vec::new(),
        continuous_branches: Vec::new(),
    })
}

fn assign_cladogram_positions(
    tree: &Tree,
    node_id: NodeId,
    depth: usize,
    positions: &mut [(f32, f32)],
    edges: &mut Vec<(NodeId, NodeId)>,
    leaf_index: &mut usize,
    max_depth: &mut usize,
) -> f32 {
    let node = &tree.nodes[node_id];
    let y_pos = if node.children.is_empty() {
        let y = *leaf_index as f32;
        *leaf_index += 1;
        y
    } else {
        let mut first_y = f32::MAX;
        let mut last_y = f32::MIN;
        for &child_id in &node.children {
            edges.push((node_id, child_id));
            let child_y = assign_cladogram_positions(
                tree,
                child_id,
                depth + 1,
                positions,
                edges,
                leaf_index,
                max_depth,
            );
            first_y = first_y.min(child_y);
            last_y = last_y.max(child_y);
        }
        if first_y.is_finite() && last_y.is_finite() {
            (first_y + last_y) / 2.0
        } else {
            *leaf_index as f32
        }
    };

    positions[node_id] = (depth as f32, y_pos);
    *max_depth = (*max_depth).max(depth);


    y_pos
}
