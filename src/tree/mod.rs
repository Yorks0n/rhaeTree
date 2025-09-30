use std::collections::{BTreeMap, HashMap};

use phylotree::tree::{Node as PhyloNode, Tree as PhyloTree};

pub mod layout;
pub mod painter;
pub mod viewer;

pub type NodeId = phylotree::tree::NodeId;

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
        let root = phylo.get_root().ok();
        let nodes = Self::build_nodes_from_phylo(&phylo);
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

    #[allow(dead_code)]
    pub fn node(&self, id: NodeId) -> Option<&TreeNode> {
        self.nodes.get(id)
    }

    pub fn node_mut(&mut self, id: NodeId) -> Option<&mut TreeNode> {
        self.nodes.get_mut(id)
    }

    pub fn root(&self) -> Option<&TreeNode> {
        self.root.and_then(|id| self.nodes.get(id))
    }

    pub fn leaf_count(&self) -> usize {
        self.nodes.iter().filter(|node| node.is_leaf()).count()
    }

    pub fn external_nodes(&self) -> Vec<&TreeNode> {
        self.nodes.iter().filter(|node| node.is_leaf()).collect()
    }

    pub fn internal_nodes(&self) -> Vec<&TreeNode> {
        self.nodes.iter().filter(|node| !node.is_leaf()).collect()
    }

    pub fn layout(&self) -> Option<layout::TreeLayout> {
        layout::TreeLayout::from_tree_default(self)
    }

    pub fn set_attribute(&mut self, key: String, value: String) {
        self.attributes.insert(key, value);
    }

    pub fn get_attribute(&self, key: &str) -> Option<&String> {
        self.attributes.get(key)
    }

    pub fn phylotree(&self) -> &PhyloTree {
        &self.phylo
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
}

/// Node within a phylogenetic tree.
#[derive(Debug, Clone)]
pub struct TreeNode {
    pub id: NodeId,
    pub name: Option<String>,
    pub length: Option<f64>,
    pub parent: Option<NodeId>,
    pub children: Vec<NodeId>,
    pub attributes: HashMap<String, String>,
}

impl TreeNode {
    pub fn new(id: NodeId, name: Option<String>, length: Option<f64>) -> Self {
        Self {
            id,
            name,
            length,
            parent: None,
            children: Vec::new(),
            attributes: HashMap::new(),
        }
    }

    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    pub fn set_attribute(&mut self, key: String, value: String) {
        self.attributes.insert(key, value);
    }

    pub fn get_attribute(&self, key: &str) -> Option<&String> {
        self.attributes.get(key)
    }

    pub fn get_numeric_attribute(&self, key: &str) -> Option<f64> {
        self.attributes.get(key).and_then(|s| s.parse::<f64>().ok())
    }

    pub(crate) fn from_phylo(node: &PhyloNode) -> Self {
        let mut tree_node = TreeNode::new(node.id, node.name.clone(), node.parent_edge);
        tree_node.parent = node.parent;
        tree_node.children = node.children.clone();
        tree_node
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
}
