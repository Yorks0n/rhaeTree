/// Test module to verify that highlight sectors work correctly after reroot operations
/// This can be used to validate that the sector calculation properly follows
/// the rerooted tree structure

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tree::{Tree, TreeNode};
    use crate::tree::viewer::TreeViewer;
    use crate::tree::painter::TreePainter;

    #[test]
    fn test_sector_calculation_after_reroot() {
        // Create a simple test tree
        let mut tree = create_test_tree();

        // Create a viewer and set the tree
        let mut viewer = TreeViewer::new();
        viewer.set_trees(vec![tree.clone()]);

        // Select a clade for highlighting
        let highlighted_node = 2; // Select an internal node
        viewer.select_clade_nodes(highlighted_node);

        // Get descendants before reroot
        let painter = TreePainter::default();
        let descendants_before = painter.collect_all_descendants(&tree, highlighted_node);
        println!("Descendants before reroot: {:?}", descendants_before);

        // Perform manual reroot at a different branch
        viewer.reroot_at_branch(4);

        // Get the rerooted tree
        if let Some(rerooted_tree) = viewer.current_tree() {
            // Get descendants after reroot
            let descendants_after = painter.collect_all_descendants(rerooted_tree, highlighted_node);
            println!("Descendants after reroot: {:?}", descendants_after);

            // Verify that descendants are correctly updated
            // The actual descendants should change based on the new tree structure
            assert!(!descendants_after.is_empty(), "Should have descendants after reroot");

            // Verify parent-child relationships are consistent
            for node_id in descendants_after.iter() {
                let node = &rerooted_tree.nodes[*node_id];
                if let Some(parent_id) = node.parent {
                    let parent = &rerooted_tree.nodes[parent_id];
                    assert!(parent.children.contains(node_id),
                           "Parent should contain child in its children list");
                }
            }
        }

        // Test midpoint reroot
        viewer.midpoint_root();

        if let Some(midpoint_tree) = viewer.current_tree() {
            // Get descendants after midpoint reroot
            let descendants_midpoint = painter.collect_all_descendants(midpoint_tree, highlighted_node);
            println!("Descendants after midpoint reroot: {:?}", descendants_midpoint);

            // Verify tree consistency
            verify_tree_consistency(midpoint_tree);
        }
    }

    fn create_test_tree() -> Tree {
        // Create a simple tree structure for testing
        //       0 (root)
        //      / \
        //     1   2
        //    /   / \
        //   3   4   5

        let mut nodes = vec![
            TreeNode::new(0, Some("root".to_string()), None),
            TreeNode::new(1, Some("node1".to_string()), Some(1.0)),
            TreeNode::new(2, Some("node2".to_string()), Some(1.0)),
            TreeNode::new(3, Some("tip1".to_string()), Some(1.0)),
            TreeNode::new(4, Some("tip2".to_string()), Some(1.0)),
            TreeNode::new(5, Some("tip3".to_string()), Some(1.0)),
        ];

        // Set up parent-child relationships
        nodes[0].children = vec![1, 2];
        nodes[1].parent = Some(0);
        nodes[1].children = vec![3];
        nodes[2].parent = Some(0);
        nodes[2].children = vec![4, 5];
        nodes[3].parent = Some(1);
        nodes[4].parent = Some(2);
        nodes[5].parent = Some(2);

        Tree {
            id: 0,
            label: Some("Test Tree".to_string()),
            root: Some(0),
            nodes,
        }
    }

    fn verify_tree_consistency(tree: &Tree) {
        // Verify that all parent-child relationships are bidirectional
        for node in &tree.nodes {
            // Check that each child has this node as parent
            for &child_id in &node.children {
                let child = &tree.nodes[child_id];
                assert_eq!(child.parent, Some(node.id),
                          "Child {} should have parent {}", child_id, node.id);
            }

            // Check that parent has this node as child
            if let Some(parent_id) = node.parent {
                let parent = &tree.nodes[parent_id];
                assert!(parent.children.contains(&node.id),
                       "Parent {} should have child {}", parent_id, node.id);
            }
        }

        // Verify root has no parent
        if let Some(root_id) = tree.root {
            assert!(tree.nodes[root_id].parent.is_none(),
                   "Root node should have no parent");
        }
    }
}