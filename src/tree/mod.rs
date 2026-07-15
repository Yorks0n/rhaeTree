use std::collections::{BTreeMap, BTreeSet, HashMap};

use phylotree::tree::{Node as PhyloNode, Tree as PhyloTree};

pub mod cairo_renderer;
pub mod layout;
pub mod painter;
pub mod scene_graph;
pub mod skia_renderer;
pub mod viewer;

pub type NodeId = phylotree::tree::NodeId;
pub(crate) const SERIALIZED_NODE_ID_ATTRIBUTE: &str = "!rhaetreeId";

#[derive(Debug, Clone)]
pub enum NodeNumericValue {
    Scalar(f64),
    Range { min: f64, max: f64 },
    Values(Vec<f64>),
}

impl NodeNumericValue {
    pub fn as_scalar(&self) -> Option<f64> {
        match self {
            Self::Scalar(value) => Some(*value),
            Self::Range { .. } => None,
            Self::Values(values) => {
                if values.len() == 1 {
                    Some(values[0])
                } else {
                    None
                }
            }
        }
    }

    pub fn as_range(&self) -> Option<(f64, f64)> {
        match self {
            Self::Scalar(value) => Some((*value, *value)),
            Self::Range { min, max } => Some((*min, *max)),
            Self::Values(values) => {
                if values.is_empty() {
                    None
                } else {
                    let mut min = f64::INFINITY;
                    let mut max = f64::NEG_INFINITY;

                    for value in values {
                        if value.is_finite() {
                            min = min.min(*value);
                            max = max.max(*value);
                        }
                    }

                    if min.is_finite() && max.is_finite() {
                        Some((min, max))
                    } else {
                        None
                    }
                }
            }
        }
    }
}

/// Representation of a phylogenetic tree with an explicit node list.
#[derive(Debug, Clone)]
pub struct Tree {
    pub id: usize,
    pub label: Option<String>,
    pub newick: String,
    pub root: Option<NodeId>,
    pub nodes: Vec<TreeNode>,
    pub attributes: HashMap<String, String>,
    pub phylo: PhyloTree,
}

impl Tree {
    pub fn new(id: usize, label: Option<String>, newick: String, phylo: PhyloTree) -> Self {
        let mut root = phylo.get_root().ok();
        let mut nodes = Self::build_nodes_from_phylo(&phylo);
        Self::restore_serialized_node_ids(&mut nodes, &mut root);
        Self {
            id,
            label,
            newick,
            root,
            nodes,
            attributes: HashMap::new(),
            phylo,
        }
    }

    /// Calculate the number of leaf descendants for each node
    fn calculate_clade_sizes(&self) -> Vec<usize> {
        let mut sizes = vec![0; self.nodes.len()];

        // Post-order traversal to calculate sizes
        fn calculate_size(node_id: NodeId, nodes: &[TreeNode], sizes: &mut [usize]) -> usize {
            let node = &nodes[node_id];

            if node.is_leaf() {
                sizes[node_id] = 1;
                return 1;
            }

            let mut size = 0;
            for &child_id in &node.children {
                size += calculate_size(child_id, nodes, sizes);
            }

            sizes[node_id] = size;
            size
        }

        if let Some(root_id) = self.root {
            calculate_size(root_id, &self.nodes, &mut sizes);
        }

        sizes
    }

    /// Order all nodes' children by clade size
    /// If `increasing` is true, smaller clades come first; otherwise larger clades come first
    pub fn order_nodes(&mut self, increasing: bool) {
        let sizes = self.calculate_clade_sizes();

        // Sort children of each node based on clade sizes
        for node in &mut self.nodes {
            if node.children.len() > 1 {
                node.children.sort_by(|&a, &b| {
                    let size_a = sizes[a];
                    let size_b = sizes[b];

                    if increasing {
                        size_a.cmp(&size_b)
                    } else {
                        size_b.cmp(&size_a)
                    }
                });
            }
        }
    }

    /// Rotate a node's subtree by recursively reversing the order of all descendant nodes
    /// This reverses the order of all tip nodes under the selected branch
    pub fn rotate_node(&mut self, node_id: NodeId) {
        // Recursively rotate all descendants first (post-order)
        let children: Vec<NodeId> = if let Some(node) = self.nodes.get(node_id) {
            node.children.clone()
        } else {
            return;
        };

        // Recursively rotate all children
        for child_id in &children {
            self.rotate_node(*child_id);
        }

        // Then reverse the children of this node
        if let Some(node) = self.nodes.get_mut(node_id) {
            node.children.reverse();
        }
    }

    /// Apply equal branch transformation: all branches have the same length
    pub fn apply_equal_transform(&mut self) {
        for node in &mut self.nodes {
            if node.parent.is_some() {
                node.length = Some(1.0);
            }
        }
    }

    /// Calculate the distance from root to each node
    fn calculate_distances_from_root(&self) -> Vec<f64> {
        let mut distances = vec![0.0; self.nodes.len()];

        fn calculate_distance(
            node_id: NodeId,
            current_distance: f64,
            nodes: &[TreeNode],
            distances: &mut [f64],
        ) {
            distances[node_id] = current_distance;

            let node = &nodes[node_id];
            for &child_id in &node.children {
                let branch_length = nodes[child_id].length.unwrap_or(1.0);
                calculate_distance(child_id, current_distance + branch_length, nodes, distances);
            }
        }

        if let Some(root_id) = self.root {
            calculate_distance(root_id, 0.0, &self.nodes, &mut distances);
        }

        distances
    }

    /// Apply cladogram transformation: equal branch lengths with tips aligned
    pub fn apply_cladogram_transform(&mut self) {
        // First, set all branches to equal length
        for node in &mut self.nodes {
            if node.parent.is_some() {
                node.length = Some(1.0);
            }
        }

        // Calculate distances from root to each node
        let distances_from_root = self.calculate_distances_from_root();

        // Find the maximum distance to any leaf
        let max_distance = self
            .nodes
            .iter()
            .enumerate()
            .filter(|(_, node)| node.is_leaf())
            .map(|(id, _)| distances_from_root[id])
            .fold(0.0f64, |a, b| a.max(b));

        if max_distance <= 0.0 {
            return;
        }

        // For each leaf, extend its branch length to align with the furthest leaf
        for i in 0..self.nodes.len() {
            if self.nodes[i].is_leaf() && self.nodes[i].parent.is_some() {
                let current_distance = distances_from_root[i];
                let extension = max_distance - current_distance;

                if extension > 0.0 {
                    let current_length = self.nodes[i].length.unwrap_or(1.0);
                    self.nodes[i].length = Some(current_length + extension);
                }
            }
        }
    }

    /// Apply proportional transformation: scale branches to align tips while maintaining proportions
    pub fn apply_proportional_transform(&mut self) {
        // Calculate distances from root to each node
        let distances_from_root = self.calculate_distances_from_root();

        // Find the maximum distance to any leaf
        let max_distance = self
            .nodes
            .iter()
            .enumerate()
            .filter(|(_, node)| node.is_leaf())
            .map(|(id, _)| distances_from_root[id])
            .fold(0.0f64, |a, b| a.max(b));

        if max_distance <= 0.0 {
            return;
        }

        // For each leaf, extend its branch length to align with the furthest leaf
        // This maintains the proportions of the original tree
        for i in 0..self.nodes.len() {
            if self.nodes[i].is_leaf() && self.nodes[i].parent.is_some() {
                let current_distance = distances_from_root[i];
                let extension = max_distance - current_distance;

                if extension > 0.0 {
                    let current_length = self.nodes[i].length.unwrap_or(1.0);
                    self.nodes[i].length = Some(current_length + extension);
                }
            }
        }
    }

    #[allow(dead_code)]
    pub fn node(&self, id: NodeId) -> Option<&TreeNode> {
        self.nodes.get(id)
    }

    pub fn node_mut(&mut self, id: NodeId) -> Option<&mut TreeNode> {
        self.nodes.get_mut(id)
    }

    pub fn leaf_count(&self) -> usize {
        self.nodes.iter().filter(|node| node.is_leaf()).count()
    }

    pub fn external_nodes(&self) -> Vec<&TreeNode> {
        self.nodes.iter().filter(|node| node.is_leaf()).collect()
    }

    pub fn node_numeric_attribute_keys(&self) -> Vec<String> {
        let mut keys: BTreeSet<String> = BTreeSet::new();
        for node in &self.nodes {
            for key in node.numeric_attributes.keys() {
                keys.insert(key.clone());
            }
        }
        keys.into_iter().collect()
    }

    pub fn node_attribute_keys(&self) -> Vec<String> {
        let mut keys: BTreeSet<String> = BTreeSet::new();
        for node in &self.nodes {
            for key in node.attributes.keys() {
                keys.insert(key.clone());
            }
        }
        keys.into_iter().collect()
    }

    pub fn node_numeric_range_keys(&self) -> Vec<String> {
        let mut keys: BTreeSet<String> = BTreeSet::new();
        for node in &self.nodes {
            for (key, value) in &node.numeric_attributes {
                if let Some((min, max)) = value.as_range() {
                    if min.is_finite() && max.is_finite() && (max - min).abs() > f64::EPSILON {
                        keys.insert(key.clone());
                    }
                }
            }
        }
        keys.into_iter().collect()
    }

    fn build_nodes_from_phylo(phylo: &PhyloTree) -> Vec<TreeNode> {
        let mut nodes = Vec::with_capacity(phylo.size());
        for idx in 0..phylo.size() {
            match phylo.get(&idx) {
                Ok(node) => nodes.push(TreeNode::from_phylo(node)),
                Err(_) => nodes.push(TreeNode::new(idx, None, None)),
            }
        }
        nodes
    }

    fn restore_serialized_node_ids(nodes: &mut Vec<TreeNode>, root: &mut Option<NodeId>) {
        let node_count = nodes.len();
        let old_to_saved = (|| {
            let mut mapping = vec![NodeId::MAX; node_count];
            let mut seen = vec![false; node_count];

            for node in nodes.iter() {
                if node.id >= node_count {
                    return None;
                }
                let saved_id = node
                    .attributes
                    .get(SERIALIZED_NODE_ID_ATTRIBUTE)?
                    .parse::<NodeId>()
                    .ok()?;
                if saved_id >= node_count || seen[saved_id] {
                    return None;
                }
                seen[saved_id] = true;
                mapping[node.id] = saved_id;
            }
            Some(mapping)
        })();
        let Some(old_to_saved) = old_to_saved else {
            Self::remove_serialized_node_ids(nodes);
            return;
        };

        let old_root = *root;
        let mut reordered: Vec<Option<TreeNode>> = vec![None; node_count];
        for mut node in std::mem::take(nodes) {
            let saved_id = old_to_saved[node.id];
            node.id = saved_id;
            node.parent = node.parent.map(|parent| old_to_saved[parent]);
            for child in &mut node.children {
                *child = old_to_saved[*child];
            }
            node.attributes.remove(SERIALIZED_NODE_ID_ATTRIBUTE);
            node.numeric_attributes.remove(SERIALIZED_NODE_ID_ATTRIBUTE);
            reordered[saved_id] = Some(node);
        }

        *nodes = reordered
            .into_iter()
            .map(|node| node.expect("validated serialized node id"))
            .collect();
        *root = old_root.map(|old_id| old_to_saved[old_id]);
    }

    fn remove_serialized_node_ids(nodes: &mut [TreeNode]) {
        for node in nodes {
            node.attributes.remove(SERIALIZED_NODE_ID_ATTRIBUTE);
            node.numeric_attributes.remove(SERIALIZED_NODE_ID_ATTRIBUTE);
        }
    }
}

/// Node within a phylogenetic tree.
#[derive(Debug, Clone)]
pub struct TreeNode {
    pub id: NodeId,
    pub name: Option<String>,
    pub label: Option<String>, // User-defined label for annotation
    pub length: Option<f64>,
    pub parent: Option<NodeId>,
    pub children: Vec<NodeId>,
    pub attributes: HashMap<String, String>,
    pub numeric_attributes: HashMap<String, NodeNumericValue>,
}

impl TreeNode {
    pub fn new(id: NodeId, name: Option<String>, length: Option<f64>) -> Self {
        Self {
            id,
            name,
            label: None,
            length,
            parent: None,
            children: Vec::new(),
            attributes: HashMap::new(),
            numeric_attributes: HashMap::new(),
        }
    }

    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    pub fn set_attribute(&mut self, key: String, value: String) {
        if key.eq_ignore_ascii_case("label") {
            self.label = Some(value.clone());
        }
        self.attributes.insert(key.clone(), value.clone());
        self.update_numeric_attribute(&key, &value);
    }

    pub fn get_attribute(&self, key: &str) -> Option<&String> {
        self.attributes.get(key)
    }

    pub fn get_numeric_attribute(&self, key: &str) -> Option<f64> {
        self.numeric_attributes
            .get(key)
            .and_then(NodeNumericValue::as_scalar)
    }

    pub fn numeric_range_attribute(&self, key: &str) -> Option<(f64, f64)> {
        self.numeric_attributes
            .get(key)
            .and_then(NodeNumericValue::as_range)
    }

    pub(crate) fn from_phylo(node: &PhyloNode) -> Self {
        let name = node.name.as_deref().map(|name| {
            if name.len() >= 2 && name.starts_with('"') && name.ends_with('"') {
                name[1..name.len() - 1].replace("\"\"", "\"")
            } else {
                name.to_string()
            }
        });
        let mut tree_node = TreeNode::new(node.id, name, node.parent_edge);
        tree_node.parent = node.parent;
        tree_node.children = node.children.clone();
        if let Some(comment) = node.comment.as_deref() {
            tree_node.apply_comment(comment);
        }
        tree_node
    }

    fn apply_comment(&mut self, comment: &str) {
        for (key, value) in parse_comment_attributes(comment) {
            self.set_attribute(key, value);
        }
    }

    fn update_numeric_attribute(&mut self, key: &str, value: &str) {
        if let Some(values) = parse_numeric_values(value) {
            if values.is_empty() {
                self.numeric_attributes.remove(key);
            } else if values.len() == 1 {
                self.numeric_attributes
                    .insert(key.to_string(), NodeNumericValue::Scalar(values[0]));
            } else if values.len() == 2 {
                let a = values[0];
                let b = values[1];
                let min = a.min(b);
                let max = a.max(b);
                self.numeric_attributes
                    .insert(key.to_string(), NodeNumericValue::Range { min, max });
            } else {
                self.numeric_attributes
                    .insert(key.to_string(), NodeNumericValue::Values(values));
            }
        } else if let Ok(number) = value.trim().parse::<f64>() {
            self.numeric_attributes
                .insert(key.to_string(), NodeNumericValue::Scalar(number));
        } else {
            self.numeric_attributes.remove(key);
        }
    }
}

fn parse_comment_attributes(comment: &str) -> Vec<(String, String)> {
    let mut attributes = Vec::new();

    for segment in comment.split('&') {
        let trimmed_segment = segment.trim();
        if trimmed_segment.is_empty() {
            continue;
        }

        for part in split_top_level(trimmed_segment, ',') {
            let trimmed_part = part.trim();
            if trimmed_part.is_empty() {
                continue;
            }

            let (raw_key, raw_value) = match trimmed_part.split_once('=') {
                Some((key, value)) => (key.trim(), value.trim()),
                None => (trimmed_part.trim(), "true"),
            };

            if raw_key.is_empty() {
                continue;
            }

            let key = raw_key.trim_matches(|c| c == '\'' || c == '"').to_string();
            let value = decode_annotation_value(raw_value);

            if !key.is_empty() {
                attributes.push((key, value));
            }
        }
    }

    attributes
}

fn split_top_level(input: &str, delimiter: char) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut depth = 0i32;
    let mut quote: Option<char> = None;
    let mut escaped = false;

    for ch in input.chars() {
        if let Some(active_quote) = quote {
            current.push(ch);
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == active_quote {
                quote = None;
            }
            continue;
        }

        match ch {
            '\'' | '"' => {
                quote = Some(ch);
                current.push(ch);
            }
            '{' | '[' | '(' => {
                depth += 1;
                current.push(ch);
            }
            '}' | ']' | ')' => {
                if depth > 0 {
                    depth -= 1;
                }
                current.push(ch);
            }
            _ if ch == delimiter && depth == 0 => {
                let trimmed = current.trim();
                if !trimmed.is_empty() {
                    parts.push(trimmed.to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }

    let trimmed = current.trim();
    if !trimmed.is_empty() {
        parts.push(trimmed.to_string());
    }

    parts
}

fn decode_annotation_value(raw: &str) -> String {
    let trimmed = raw.trim();
    let Some(quote) = trimmed.chars().next() else {
        return String::new();
    };
    if !matches!(quote, '\'' | '"') || !trimmed.ends_with(quote) || trimmed.len() < 2 {
        return trimmed.to_string();
    }

    let inner = &trimmed[quote.len_utf8()..trimmed.len() - quote.len_utf8()];
    let mut decoded = String::with_capacity(inner.len());
    let mut chars = inner.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            decoded.push(ch);
            continue;
        }

        match chars.next() {
            Some('n') => decoded.push('\n'),
            Some('r') => decoded.push('\r'),
            Some('t') => decoded.push('\t'),
            Some('u') => {
                let escape: String = chars.by_ref().take(4).collect();
                if escape == "005D" {
                    decoded.push(']');
                } else {
                    decoded.push_str("\\u");
                    decoded.push_str(&escape);
                }
            }
            Some(next @ ('\\' | '\'' | '"')) => decoded.push(next),
            Some(next) => {
                decoded.push('\\');
                decoded.push(next);
            }
            None => decoded.push('\\'),
        }
    }
    decoded
}

fn parse_numeric_values(value: &str) -> Option<Vec<f64>> {
    let mut trimmed = value.trim();
    if trimmed.is_empty() {
        return None;
    }

    while matches!(trimmed.chars().next(), Some('{') | Some('[') | Some('('))
        && matches!(trimmed.chars().last(), Some('}') | Some(']') | Some(')'))
    {
        trimmed = &trimmed[1..trimmed.len() - 1];
        trimmed = trimmed.trim();
        if trimmed.is_empty() {
            break;
        }
    }

    let cleaned = trimmed.replace(',', " ");
    let mut values = Vec::new();

    for token in cleaned.split_whitespace() {
        let candidate = token.trim_matches(|c| c == '\'' || c == '"');
        if candidate.is_empty() {
            continue;
        }
        if let Ok(number) = candidate.parse::<f64>() {
            values.push(number);
        } else if let Some(stripped) = candidate.strip_suffix('%') {
            if let Ok(number) = stripped.parse::<f64>() {
                values.push(number);
            }
        }
    }

    if values.is_empty() {
        None
    } else {
        Some(values)
    }
}

/// Container for the full contents of an imported file.
#[derive(Debug, Clone)]
pub struct TreeBundle {
    pub format: TreeFileFormat,
    pub trees: Vec<Tree>,
    pub metadata: BTreeMap<String, String>,
}

impl TreeBundle {
    pub fn new(format: TreeFileFormat, trees: Vec<Tree>) -> Self {
        Self {
            format,
            trees,
            metadata: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TreeFileFormat {
    Newick,
    Nexus,
    Rtr,
}
