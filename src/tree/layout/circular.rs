use super::rectangular::{self, RectangularLayoutData};
use super::{ArcSegment, ContinuousBranch, NodeId, RectSegment, RectSegmentKind, Tree, TreeLayout, TreeLayoutType};

pub(super) fn build(tree: &Tree) -> Option<TreeLayout> {
    let data = rectangular::compute_base(tree)?;
    let RectangularLayoutData {
        positions,
        edges,
        max_x,
        tip_count,
        ..
    } = data;

    // 计算角度范围（类似Java中的angularRange）
    let angular_range = std::f32::consts::TAU * 0.9; // 留10%空白
    let start_angle = std::f32::consts::PI; // 起始角度（顶部）

    // 转换矩形坐标到极坐标
    let max_radius = max_x.max(1e-6);
    let mut polar_positions = vec![(0.0f32, 0.0f32); positions.len()];

    // 首先计算每个节点的角度（叶节点均匀分布，内部节点根据子节点角度计算）
    let mut node_angles = vec![0.0f32; positions.len()];
    compute_node_angles(tree, tree.root?, &positions, tip_count, angular_range, start_angle, &mut node_angles);

    // 然后计算极坐标位置
    // 使用原始的分支长度比例，而不是归一化到[0,1]范围
    for (index, &(x, _)) in positions.iter().enumerate() {
        let radius = x; // 直接使用x坐标作为半径，保持分支长度比例
        let angle = node_angles[index];
        polar_positions[index] = (radius * angle.cos(), radius * angle.sin());
    }

    // 计算边界框并居中位置
    let (mut min_x, mut max_x, mut min_y, mut max_y) = calculate_bounding_box(&polar_positions);

    if !min_x.is_finite() || !max_x.is_finite() || !min_y.is_finite() || !max_y.is_finite() {
        min_x = -1.0;
        max_x = 1.0;
        min_y = -1.0;
        max_y = 1.0;
    }

    let width = (max_x - min_x).abs().max(1e-6);
    let height = (max_y - min_y).abs().max(1e-6);

    // 计算中心点并居中 - 确保居中后坐标从(0,0)开始
    let center_x = (min_x + max_x) * 0.5;
    let center_y = (min_y + max_y) * 0.5;

    for pos in &mut polar_positions {
        pos.0 = pos.0 - center_x + width * 0.5;
        pos.1 = pos.1 - center_y + height * 0.5;
    }

    // 验证居中后的坐标范围确实从(0,0)开始
    let (final_min_x, _final_max_x, final_min_y, _final_max_y) = calculate_bounding_box(&polar_positions);

    // 如果居中后坐标不是从0开始，进行微调
    if final_min_x.abs() > 1e-3 || final_min_y.abs() > 1e-3 {
        for pos in &mut polar_positions {
            pos.0 -= final_min_x;
            pos.1 -= final_min_y;
        }
    }

    // 获取根节点的最终位置作为圆弧的中心
    // 根节点是进化树形状的真实中心点
    let root_center = if let Some(root_id) = tree.root {
        polar_positions[root_id]
    } else {
        // 如果没有根节点，使用布局的几何中心
        (width * 0.5, height * 0.5)
    };

    // 为圆形布局创建连续的分支线段
    let mut continuous_branches = Vec::new();
    let mut rect_segments = Vec::new();
    let mut arc_segments = Vec::new();

    for (parent, child) in &edges {
        let parent_pos = positions[*parent];
        let child_pos = positions[*child];

        // 使用预先计算的角度
        let parent_angle = node_angles[*parent];
        let child_angle = node_angles[*child];

        // 计算极坐标下的位置 - 使用原始半径，与polar_positions保持一致
        let parent_radius = parent_pos.0;
        let child_radius = child_pos.0;

        let mut parent_polar = (
            parent_radius * parent_angle.cos(),
            parent_radius * parent_angle.sin()
        );
        let mut child_polar = (
            child_radius * child_angle.cos(),
            child_radius * child_angle.sin()
        );

        // 应用与节点坐标相同的居中变换
        parent_polar.0 = parent_polar.0 - center_x + width * 0.5;
        parent_polar.1 = parent_polar.1 - center_y + height * 0.5;
        child_polar.0 = child_polar.0 - center_x + width * 0.5;
        child_polar.1 = child_polar.1 - center_y + height * 0.5;

        // 如果需要微调，也应用相同的微调
        if final_min_x.abs() > 1e-3 || final_min_y.abs() > 1e-3 {
            parent_polar.0 -= final_min_x;
            parent_polar.1 -= final_min_y;
            child_polar.0 -= final_min_x;
            child_polar.1 -= final_min_y;
        }

        // 计算肩点（shoulder point）- 也需要应用相同的变换
        let shoulder_radius = parent_radius;
        let shoulder_angle = child_angle;
        let mut shoulder_polar = (
            shoulder_radius * shoulder_angle.cos(),
            shoulder_radius * shoulder_angle.sin()
        );

        // 对肩点应用相同的居中变换
        shoulder_polar.0 = shoulder_polar.0 - center_x + width * 0.5;
        shoulder_polar.1 = shoulder_polar.1 - center_y + height * 0.5;
        if final_min_x.abs() > 1e-3 || final_min_y.abs() > 1e-3 {
            shoulder_polar.0 -= final_min_x;
            shoulder_polar.1 -= final_min_y;
        }

        // 创建连续的分支点序列：子节点 -> 肩点 -> 父节点（通过圆弧）
        let mut branch_points = Vec::new();

        // 添加子节点到肩点的直线段
        branch_points.push(child_polar);
        branch_points.push(shoulder_polar);

        // 添加肩点到父节点的圆弧段（用多个点近似）
        let arc_segment_count = 10; // 圆弧分段数
        let angle_step = (parent_angle - child_angle) / arc_segment_count as f32;

        for i in 1..arc_segment_count {
            let angle = child_angle + angle_step * i as f32;
            let mut arc_point = (
                shoulder_radius * angle.cos(),
                shoulder_radius * angle.sin()
            );

            // 对圆弧点应用相同的居中变换
            arc_point.0 = arc_point.0 - center_x + width * 0.5;
            arc_point.1 = arc_point.1 - center_y + height * 0.5;
            if final_min_x.abs() > 1e-3 || final_min_y.abs() > 1e-3 {
                arc_point.0 -= final_min_x;
                arc_point.1 -= final_min_y;
            }

            branch_points.push(arc_point);
        }

        // 添加父节点
        branch_points.push(parent_polar);

        continuous_branches.push(ContinuousBranch {
            points: branch_points,
            parent: *parent,
            child: *child,
        });

        // 保留原有的分段数据用于向后兼容
        rect_segments.push(RectSegment {
            start: child_polar,
            end: shoulder_polar,
            parent: *parent,
            child: Some(*child),
            kind: RectSegmentKind::Horizontal,
        });

        // 使用根节点的位置作为圆弧的中心
        // 这是进化树形状的真实中心，而不是布局的几何中心
        arc_segments.push(ArcSegment {
            center: root_center,
            radius: parent_radius,
            start_angle: child_angle,
            end_angle: parent_angle,
            parent: *parent,
            child: *child,
        });
    }

    Some(TreeLayout {
        positions: polar_positions,
        edges,
        width,
        height,
        leaf_count: tip_count,
        layout_type: TreeLayoutType::Circular,
        rect_segments,
        arc_segments,
        continuous_branches,
    })
}

/// 计算每个节点的角度（叶节点均匀分布，内部节点根据子节点角度计算）
fn compute_node_angles(
    tree: &Tree,
    node_id: NodeId,
    positions: &[(f32, f32)],
    tip_count: usize,
    angular_range: f32,
    start_angle: f32,
    node_angles: &mut [f32],
) -> (f32, f32) {
    let node = &tree.nodes[node_id];

    if node.children.is_empty() {
        // 叶节点：根据y坐标计算角度（均匀分布）
        let y_pos = positions[node_id].1;
        let angle = if tip_count > 1 {
            start_angle - (y_pos / (tip_count - 1) as f32) * angular_range
        } else {
            start_angle
        };
        node_angles[node_id] = angle;
        (angle, angle)
    } else {
        // 内部节点：计算子节点的角度范围
        let mut min_angle = f32::INFINITY;
        let mut max_angle = f32::NEG_INFINITY;

        for &child_id in &node.children {
            let (child_min, child_max) = compute_node_angles(
                tree, child_id, positions, tip_count, angular_range, start_angle, node_angles
            );
            min_angle = min_angle.min(child_min);
            max_angle = max_angle.max(child_max);
        }

        // 内部节点的角度是子节点角度的平均值
        let angle = (min_angle + max_angle) * 0.5;
        node_angles[node_id] = angle;
        (min_angle, max_angle)
    }
}

/// 计算极坐标位置的边界框
fn calculate_bounding_box(positions: &[(f32, f32)]) -> (f32, f32, f32, f32) {
    if positions.is_empty() {
        return (-1.0, 1.0, -1.0, 1.0);
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
        return (-1.0, 1.0, -1.0, 1.0);
    }

    (min_x, max_x, min_y, max_y)
}

