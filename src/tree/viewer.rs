use std::collections::{HashMap, HashSet};

use super::{NodeId, Tree, TreeNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextSearchType {
    Contains,
    StartsWith,
    EndsWith,
    Matches,
    Regex,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberSearchType {
    Equals,
    NotEquals,
    GreaterThan,
    EqualsOrGreaterThan,
    LessThan,
    EqualsOrLessThan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelectionMode {
    Clade,
    Nodes,
    Tips,
    Taxa,
}

#[derive(Debug, Clone)]
pub struct TreeViewer {
    trees: Vec<Tree>,
    original_trees: Vec<Tree>, // Store original trees for rerooting
    rooted_trees: Vec<Option<Tree>>, // Store rerooted versions
    unordered_trees: Vec<Option<Tree>>, // Store trees before ordering
    current_tree_index: usize,
    selected_nodes: HashSet<NodeId>,
    selected_tips: HashSet<NodeId>,
    selection_mode: SelectionMode,
    pub zoom: f32,
    pub vertical_expansion: f32,
}

#[derive(Clone)]
pub struct TreeSnapshot {
    index: usize,
    tree: Tree,
}

impl TreeViewer {
    pub fn new() -> Self {
        Self {
            trees: Vec::new(),
            original_trees: Vec::new(),
            rooted_trees: Vec::new(),
            unordered_trees: Vec::new(),
            current_tree_index: 0,
            selected_nodes: HashSet::new(),
            selected_tips: HashSet::new(),
            selection_mode: SelectionMode::Taxa,
            zoom: 1.0,
            vertical_expansion: 1.0,
        }
    }

    pub fn set_trees(&mut self, trees: Vec<Tree>) {
        self.original_trees = trees.clone();
        self.rooted_trees = vec![None; trees.len()];
        self.unordered_trees = vec![None; trees.len()];
        self.trees = trees;
        self.current_tree_index = 0;
        self.clear_selection();
    }

    pub fn add_tree(&mut self, tree: Tree) {
        self.original_trees.push(tree.clone());
        self.rooted_trees.push(None);
        self.unordered_trees.push(None);
        self.trees.push(tree);
    }

    pub fn trees(&self) -> &[Tree] {
        &self.trees
    }

    pub fn current_tree(&self) -> Option<&Tree> {
        self.trees.get(self.current_tree_index)
    }

    pub fn current_tree_mut(&mut self) -> Option<&mut Tree> {
        self.trees.get_mut(self.current_tree_index)
    }

    pub fn current_tree_index(&self) -> usize {
        self.current_tree_index
    }

    pub fn tree_count(&self) -> usize {
        self.trees.len()
    }

    pub fn show_tree(&mut self, index: usize) {
        if index < self.trees.len() {
            self.current_tree_index = index;
            self.clear_selection();
        }
    }

    pub fn snapshot_current_tree(&self) -> Option<TreeSnapshot> {
        self.current_tree().cloned().map(|tree| TreeSnapshot {
            index: self.current_tree_index,
            tree,
        })
    }

    pub fn restore_snapshot(&mut self, snapshot: TreeSnapshot) {
        if snapshot.index < self.trees.len() {
            self.trees[snapshot.index] = snapshot.tree;
            self.show_tree(snapshot.index);
        }
    }

    pub fn show_next_tree(&mut self) {
        if self.current_tree_index < self.trees.len() - 1 {
            self.current_tree_index += 1;
            self.clear_selection();
        }
    }

    pub fn show_previous_tree(&mut self) {
        if self.current_tree_index > 0 {
            self.current_tree_index -= 1;
            self.clear_selection();
        }
    }

    pub fn has_selection(&self) -> bool {
        !self.selected_nodes.is_empty() || !self.selected_tips.is_empty()
    }

    pub fn selected_nodes(&self) -> &HashSet<NodeId> {
        &self.selected_nodes
    }

    pub fn selected_tips(&self) -> &HashSet<NodeId> {
        &self.selected_tips
    }

    pub fn clear_selection(&mut self) {
        self.selected_nodes.clear();
        self.selected_tips.clear();
    }

    pub fn selection_mode(&self) -> SelectionMode {
        self.selection_mode
    }

    pub fn set_selection_mode(&mut self, mode: SelectionMode) {
        self.selection_mode = mode;
        // TODO: Convert selection between modes
    }

    pub fn select_branch_node(&mut self, node_id: NodeId) {
        self.clear_selection();
        self.selected_nodes.insert(node_id);
    }

    pub fn select_clade_nodes(&mut self, node_id: NodeId) {
        let nodes_to_select = self.current_tree().map(|tree| {
            let mut stack = vec![node_id];
            let mut nodes = Vec::new();
            while let Some(current) = stack.pop() {
                nodes.push(current);
                stack.extend(tree.nodes[current].children.iter().copied());
            }
            nodes
        });

        self.clear_selection();
        if let Some(nodes) = nodes_to_select {
            for id in nodes {
                self.selected_nodes.insert(id);
            }
        }
    }

    pub fn select_tip_label(&mut self, node_id: NodeId) {
        let ancestors = self.current_tree().map(|tree| {
            let mut lineage = Vec::new();
            let mut current = tree.nodes[node_id].parent;
            while let Some(parent) = current {
                lineage.push(parent);
                current = tree.nodes[parent].parent;
            }
            lineage
        });

        self.clear_selection();
        self.selected_tips.insert(node_id);
        if let Some(lineage) = ancestors {
            for id in lineage {
                self.selected_nodes.insert(id);
            }
        }
    }

    pub fn select_clade_tips(&mut self, node_id: NodeId) {
        let (nodes, tips) = if let Some(tree) = self.current_tree() {
            let mut stack = vec![node_id];
            let mut nodes = Vec::new();
            let mut tips = Vec::new();
            while let Some(current) = stack.pop() {
                nodes.push(current);
                let node = &tree.nodes[current];
                if node.is_leaf() {
                    tips.push(current);
                } else {
                    stack.extend(node.children.iter().copied());
                }
            }
            if let Some(parent) = tree.nodes[node_id].parent {
                nodes.push(parent);
            }
            (nodes, tips)
        } else {
            (Vec::new(), Vec::new())
        };

        self.clear_selection();
        for id in nodes {
            self.selected_nodes.insert(id);
        }
        for id in tips {
            self.selected_tips.insert(id);
        }
    }

    pub fn select_taxa(
        &mut self,
        attribute_name: Option<&str>,
        search_type: TextSearchType,
        search_string: &str,
        case_sensitive: bool,
    ) {
        if let Some(tree) = self.current_tree() {
            let external_node_ids: Vec<NodeId> =
                tree.external_nodes().iter().map(|n| n.id).collect();
            let mut selected_ids = Vec::new();

            for node_id in external_node_ids {
                if let Some(node) = tree.node(node_id) {
                    if Self::matches_taxon(
                        node,
                        attribute_name,
                        search_type,
                        search_string,
                        case_sensitive,
                    ) {
                        selected_ids.push(node_id);
                    }
                }
            }

            self.clear_selection();
            for node_id in selected_ids {
                self.selected_tips.insert(node_id);
            }
        }
    }

    pub fn select_nodes(
        &mut self,
        attribute_name: Option<&str>,
        search_type: TextSearchType,
        search_string: &str,
        case_sensitive: bool,
    ) {
        if let Some(tree) = self.current_tree() {
            let node_ids: Vec<NodeId> = tree.nodes.iter().map(|n| n.id).collect();
            let mut selected_ids = Vec::new();

            for node_id in node_ids {
                if let Some(node) = tree.node(node_id) {
                    if Self::matches_node(
                        node,
                        attribute_name,
                        search_type,
                        search_string,
                        case_sensitive,
                    ) {
                        selected_ids.push(node_id);
                    }
                }
            }

            self.clear_selection();
            for node_id in selected_ids {
                self.selected_nodes.insert(node_id);
            }
        }
    }

    pub fn select_taxa_numeric(
        &mut self,
        attribute_name: &str,
        search_type: NumberSearchType,
        search_value: f64,
    ) {
        if let Some(tree) = self.current_tree() {
            let external_node_ids: Vec<NodeId> =
                tree.external_nodes().iter().map(|n| n.id).collect();
            let mut selected_ids = Vec::new();

            for node_id in external_node_ids {
                if let Some(node) = tree.node(node_id) {
                    if Self::matches_numeric(node, attribute_name, search_type, search_value) {
                        selected_ids.push(node_id);
                    }
                }
            }

            self.clear_selection();
            for node_id in selected_ids {
                self.selected_tips.insert(node_id);
            }
        }
    }

    pub fn select_nodes_numeric(
        &mut self,
        attribute_name: &str,
        search_type: NumberSearchType,
        search_value: f64,
    ) {
        if let Some(tree) = self.current_tree() {
            let node_ids: Vec<NodeId> = tree.nodes.iter().map(|n| n.id).collect();
            let mut selected_ids = Vec::new();

            for node_id in node_ids {
                if let Some(node) = tree.node(node_id) {
                    if Self::matches_numeric(node, attribute_name, search_type, search_value) {
                        selected_ids.push(node_id);
                    }
                }
            }

            self.clear_selection();
            for node_id in selected_ids {
                self.selected_nodes.insert(node_id);
            }
        }
    }

    pub fn select_all(&mut self) {
        if let Some(tree) = self.current_tree() {
            match self.selection_mode {
                SelectionMode::Taxa | SelectionMode::Tips => {
                    let external_node_ids: Vec<NodeId> =
                        tree.external_nodes().iter().map(|n| n.id).collect();
                    for node_id in external_node_ids {
                        self.selected_tips.insert(node_id);
                    }
                }
                SelectionMode::Nodes | SelectionMode::Clade => {
                    let node_ids: Vec<NodeId> = tree.nodes.iter().map(|n| n.id).collect();
                    for node_id in node_ids {
                        self.selected_nodes.insert(node_id);
                    }
                }
            }
        }
    }

    pub fn zoom(&self) -> f32 {
        self.zoom
    }

    pub fn set_zoom(&mut self, zoom: f32) {
        self.zoom = zoom.max(1.0).min(10.0);
    }

    pub fn vertical_expansion(&self) -> f32 {
        self.vertical_expansion
    }

    pub fn set_vertical_expansion(&mut self, expansion: f32) {
        self.vertical_expansion = expansion.max(1.0).min(2.0);
    }

    pub fn set_layout_type(&mut self, layout_type: crate::tree::layout::TreeLayoutType) {
        // Layout type is handled by the tree layout system, this is a placeholder
        // for future implementation where layout type might affect viewer state
    }

    pub fn reroot_at_branch(&mut self, node_id: NodeId) {
        // Get original tree for this index
        if let Some(original_tree) = self.original_trees.get(self.current_tree_index).cloned() {
            if node_id >= original_tree.nodes.len() {
                return;
            }

            // Get the parent of the selected node to identify the branch
            let parent_id = original_tree.nodes[node_id].parent;
            if parent_id.is_none() {
                // Cannot reroot at root branch
                return;
            }

            // Create a new tree with virtual root inserted
            let rooted_tree = Self::create_rooted_tree_with_virtual_root(
                &original_tree,
                node_id,
                parent_id.unwrap(),
            );

            if let Some(rooted) = rooted_tree {
                // Store the rooted tree
                self.rooted_trees[self.current_tree_index] = Some(rooted.clone());
                self.trees[self.current_tree_index] = rooted;
                // Clear unordered tree state since tree structure changed
                self.unordered_trees[self.current_tree_index] = None;
                self.clear_selection();
            }
        }
    }

    pub fn reroot_at_node(&mut self, node_id: NodeId) {
        // Backward compatibility - reroot at the branch leading to this node
        self.reroot_at_branch(node_id);
    }

    pub fn midpoint_root(&mut self) {
        if let Some(tree) = self.trees.get_mut(self.current_tree_index) {
            let adjacency = Self::build_adjacency(tree);
            if adjacency.is_empty() {
                return;
            }

            let edge_lengths = Self::collect_edge_lengths(tree);
            let start = tree
                .nodes
                .iter()
                .find(|node| node.is_leaf())
                .map(|node| node.id)
                .unwrap_or(0);

            let (first_leaf, _, _) = Self::farthest_leaf(&adjacency, start);
            let (_second_leaf, diameter, path) = Self::farthest_leaf(&adjacency, first_leaf);

            if path.is_empty() || diameter <= f64::EPSILON {
                return;
            }

            let midpoint_distance = diameter / 2.0;
            let mut traversed = 0.0;
            let mut new_root = path[0];

            for window in path.windows(2) {
                let a = window[0];
                let b = window[1];
                let length = edge_lengths.get(&(a, b)).copied().unwrap_or(1.0);

                if traversed + length >= midpoint_distance {
                    let offset = midpoint_distance - traversed;
                    new_root = if offset <= length / 2.0 { a } else { b };
                    break;
                } else {
                    traversed += length;
                    new_root = b;
                }
            }

            if tree.nodes.get(new_root).is_some() {
                Self::reroot_tree(tree, new_root, &adjacency, &edge_lengths);
                // Clear unordered tree state since tree structure changed
                self.unordered_trees[self.current_tree_index] = None;
                self.clear_selection();
            }
        }
    }

    fn build_adjacency(tree: &Tree) -> Vec<Vec<(NodeId, f64)>> {
        if tree.nodes.is_empty() {
            return Vec::new();
        }

        let mut adjacency = vec![Vec::new(); tree.nodes.len()];
        for node in &tree.nodes {
            if let Some(parent) = node.parent {
                let length = node.length.unwrap_or(1.0);
                adjacency[node.id].push((parent, length));
                adjacency[parent].push((node.id, length));
            }
        }

        adjacency
    }

    fn collect_edge_lengths(tree: &Tree) -> HashMap<(NodeId, NodeId), f64> {
        let mut lengths = HashMap::new();
        for node in &tree.nodes {
            if let Some(parent) = node.parent {
                let value = node.length.unwrap_or(1.0);
                lengths.insert((node.id, parent), value);
                lengths.insert((parent, node.id), value);
            }
        }
        lengths
    }

    fn farthest_leaf(
        adjacency: &[Vec<(NodeId, f64)>],
        start: NodeId,
    ) -> (NodeId, f64, Vec<NodeId>) {
        if adjacency.is_empty() {
            return (start, 0.0, vec![start]);
        }

        let mut parents: Vec<Option<NodeId>> = vec![None; adjacency.len()];
        let mut best_node = start;
        let mut best_distance = -1.0;

        fn dfs(
            node: NodeId,
            parent: Option<NodeId>,
            distance: f64,
            adjacency: &[Vec<(NodeId, f64)>],
            parents: &mut [Option<NodeId>],
            best_node: &mut NodeId,
            best_distance: &mut f64,
        ) {
            parents[node] = parent;
            let mut has_child = false;
            for &(next, weight) in &adjacency[node] {
                if Some(next) == parent {
                    continue;
                }
                has_child = true;
                dfs(
                    next,
                    Some(node),
                    distance + weight,
                    adjacency,
                    parents,
                    best_node,
                    best_distance,
                );
            }

            let is_leaf = !has_child;
            if (is_leaf && parent.is_some()) || adjacency[node].is_empty() {
                if distance > *best_distance {
                    *best_distance = distance;
                    *best_node = node;
                }
            }
        }

        dfs(
            start,
            None,
            0.0,
            adjacency,
            &mut parents,
            &mut best_node,
            &mut best_distance,
        );

        if best_distance < 0.0 {
            return (start, 0.0, vec![start]);
        }

        let mut path = Vec::new();
        let mut current = best_node;
        path.push(current);
        while let Some(parent) = parents[current] {
            current = parent;
            path.push(current);
        }
        path.reverse();

        (best_node, best_distance, path)
    }

    fn create_rooted_tree_with_virtual_root(
        original_tree: &Tree,
        child_node_id: NodeId,
        parent_node_id: NodeId,
    ) -> Option<Tree> {
        // Clone the original tree to work with
        let mut new_tree = original_tree.clone();

        // Get the branch length between child and parent
        let branch_length = new_tree.nodes[child_node_id].length.unwrap_or(0.0);

        // Create a virtual root node
        let virtual_root_id = new_tree.nodes.len();
        let mut virtual_root = TreeNode::new(virtual_root_id, Some("__virtual_root__".to_string()), None);

        // Add the virtual root to nodes first
        new_tree.nodes.push(virtual_root);

        // Build adjacency list that includes the virtual root
        let mut adjacency = vec![Vec::new(); new_tree.nodes.len()];
        let mut edge_lengths = HashMap::new();

        // Build adjacency for original tree
        for node in &original_tree.nodes {
            if let Some(parent) = node.parent {
                let length = node.length.unwrap_or(1.0);
                // Skip the edge between child and parent that we're splitting
                if (node.id == child_node_id && parent == parent_node_id) ||
                   (node.id == parent_node_id && parent == child_node_id) {
                    continue;
                }
                adjacency[node.id].push((parent, length));
                adjacency[parent].push((node.id, length));
                edge_lengths.insert((node.id, parent), length);
                edge_lengths.insert((parent, node.id), length);
            }
        }

        // Add edges from virtual root to the two subtrees
        let half_length = branch_length / 2.0;
        adjacency[virtual_root_id].push((child_node_id, half_length));
        adjacency[child_node_id].push((virtual_root_id, half_length));
        adjacency[virtual_root_id].push((parent_node_id, half_length));
        adjacency[parent_node_id].push((virtual_root_id, half_length));

        edge_lengths.insert((virtual_root_id, child_node_id), half_length);
        edge_lengths.insert((child_node_id, virtual_root_id), half_length);
        edge_lengths.insert((virtual_root_id, parent_node_id), half_length);
        edge_lengths.insert((parent_node_id, virtual_root_id), half_length);

        // Now reorient the entire tree from the virtual root
        let mut visited = vec![false; new_tree.nodes.len()];
        Self::orient_tree(
            &mut new_tree,
            virtual_root_id,
            None,
            &adjacency,
            &edge_lengths,
            &mut visited,
        );

        // Set the virtual root as the new root
        new_tree.root = Some(virtual_root_id);

        Some(new_tree)
    }

    fn reroot_tree(
        tree: &mut Tree,
        new_root: NodeId,
        adjacency: &[Vec<(NodeId, f64)>],
        edge_lengths: &HashMap<(NodeId, NodeId), f64>,
    ) {
        if adjacency.is_empty() {
            tree.root = Some(new_root);
            return;
        }

        let mut visited = vec![false; tree.nodes.len()];
        Self::orient_tree(tree, new_root, None, adjacency, edge_lengths, &mut visited);
        tree.root = Some(new_root);
    }

    fn orient_tree(
        tree: &mut Tree,
        node: NodeId,
        parent: Option<NodeId>,
        adjacency: &[Vec<(NodeId, f64)>],
        edge_lengths: &HashMap<(NodeId, NodeId), f64>,
        visited: &mut [bool],
    ) {
        if visited[node] {
            return;
        }
        visited[node] = true;

        let neighbors: Vec<NodeId> = adjacency[node]
            .iter()
            .filter_map(|(next, _)| {
                if Some(*next) == parent {
                    None
                } else {
                    Some(*next)
                }
            })
            .collect();

        if let Some(entry) = tree.nodes.get_mut(node) {
            entry.parent = parent;
            entry.length = parent.and_then(|p| edge_lengths.get(&(node, p)).copied());
            if parent.is_none() {
                entry.length = None;
            }
            entry.children.clear();
            entry.children.extend(neighbors.iter().copied());
        }

        for next in neighbors {
            Self::orient_tree(tree, next, Some(node), adjacency, edge_lengths, visited);
        }
    }

    fn matches_taxon(
        node: &TreeNode,
        attribute_name: Option<&str>,
        search_type: TextSearchType,
        search_string: &str,
        case_sensitive: bool,
    ) -> bool {
        let query = if case_sensitive {
            search_string.to_string()
        } else {
            search_string.to_lowercase()
        };

        if let Some(attr_name) = attribute_name {
            if attr_name == "!name" {
                if let Some(name) = &node.name {
                    return Self::matches_text(name, &query, search_type, case_sensitive);
                }
            } else if let Some(value) = node.get_attribute(attr_name) {
                return Self::matches_text(value, &query, search_type, case_sensitive);
            }
        } else {
            // Search all attributes
            if let Some(name) = &node.name {
                if Self::matches_text(name, &query, search_type, case_sensitive) {
                    return true;
                }
            }
            for value in node.attributes.values() {
                if Self::matches_text(value, &query, search_type, case_sensitive) {
                    return true;
                }
            }
        }

        false
    }

    fn matches_node(
        node: &TreeNode,
        attribute_name: Option<&str>,
        search_type: TextSearchType,
        search_string: &str,
        case_sensitive: bool,
    ) -> bool {
        let query = if case_sensitive {
            search_string.to_string()
        } else {
            search_string.to_lowercase()
        };

        if let Some(attr_name) = attribute_name {
            if let Some(value) = node.get_attribute(attr_name) {
                return Self::matches_text(value, &query, search_type, case_sensitive);
            }
        } else {
            // Search all attributes
            for value in node.attributes.values() {
                if Self::matches_text(value, &query, search_type, case_sensitive) {
                    return true;
                }
            }
        }

        false
    }

    fn matches_numeric(
        node: &TreeNode,
        attribute_name: &str,
        search_type: NumberSearchType,
        search_value: f64,
    ) -> bool {
        let value = if attribute_name == "!length" {
            node.length
        } else if attribute_name == "!height" {
            // TODO: Calculate node height
            None
        } else {
            node.get_numeric_attribute(attribute_name)
        };

        if let Some(val) = value {
            match search_type {
                NumberSearchType::Equals => val == search_value,
                NumberSearchType::NotEquals => val != search_value,
                NumberSearchType::GreaterThan => val > search_value,
                NumberSearchType::EqualsOrGreaterThan => val >= search_value,
                NumberSearchType::LessThan => val < search_value,
                NumberSearchType::EqualsOrLessThan => val <= search_value,
            }
        } else {
            false
        }
    }

    fn matches_text(
        text: &str,
        query: &str,
        search_type: TextSearchType,
        case_sensitive: bool,
    ) -> bool {
        let target = if case_sensitive {
            text.to_string()
        } else {
            text.to_lowercase()
        };

        match search_type {
            TextSearchType::Contains => target.contains(query),
            TextSearchType::StartsWith => target.starts_with(query),
            TextSearchType::EndsWith => target.ends_with(query),
            TextSearchType::Matches => target == query,
            TextSearchType::Regex => {
                // TODO: Implement regex matching
                false
            }
        }
    }

    /// Apply node ordering to the current tree
    /// If `increasing` is true, smaller clades come first; otherwise larger clades come first
    pub fn apply_node_ordering(&mut self, increasing: bool) {
        // Save current tree before ordering if not already saved
        if self.unordered_trees.get(self.current_tree_index).map_or(true, |t| t.is_none()) {
            if let Some(tree) = self.trees.get(self.current_tree_index).cloned() {
                if self.current_tree_index < self.unordered_trees.len() {
                    self.unordered_trees[self.current_tree_index] = Some(tree);
                }
            }
        }

        // Apply ordering
        if let Some(tree) = self.trees.get_mut(self.current_tree_index) {
            tree.order_nodes(increasing);
        }
    }

    /// Restore tree to unordered state
    pub fn restore_unordered_tree(&mut self) {
        if let Some(Some(unordered)) = self.unordered_trees.get(self.current_tree_index).cloned() {
            self.trees[self.current_tree_index] = unordered;
            self.unordered_trees[self.current_tree_index] = None;
        }
    }

    /// Clear the saved unordered tree state
    pub fn clear_unordered_tree(&mut self) {
        if self.current_tree_index < self.unordered_trees.len() {
            self.unordered_trees[self.current_tree_index] = None;
        }
    }
}
