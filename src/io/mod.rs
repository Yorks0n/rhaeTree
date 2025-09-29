use std::fs;
use std::path::Path;

use anyhow::{anyhow, bail, Context, Result};
use phylotree::tree::Tree as PhyloTree;

use crate::tree::{Tree, TreeBundle, TreeFileFormat};

pub fn load_trees(path: &Path) -> Result<TreeBundle> {
    let raw = fs::read_to_string(path)
        .with_context(|| format!("failed to read tree file: {}", path.display()))?;

    let format = detect_format(&raw);
    let trees = match format {
        TreeFileFormat::Newick => parse_newick(&raw)?,
        TreeFileFormat::Nexus => parse_nexus(&raw)?,
    };

    if trees.is_empty() {
        bail!("tree file did not contain any trees");
    }

    Ok(TreeBundle::new(format, trees))
}

fn detect_format(raw: &str) -> TreeFileFormat {
    if raw.to_ascii_uppercase().contains("#NEXUS") {
        TreeFileFormat::Nexus
    } else {
        TreeFileFormat::Newick
    }
}

fn parse_newick(raw: &str) -> Result<Vec<Tree>> {
    let mut trees = Vec::new();

    for chunk in raw.split_inclusive(';') {
        let candidate = chunk.trim();
        if candidate.is_empty() {
            continue;
        }
        if !candidate.ends_with(';') {
            continue;
        }

        let newick = normalise_newick(candidate);
        let index = trees.len();
        let tree = build_tree(index, None, newick)?;
        trees.push(tree);
    }

    Ok(trees)
}

fn parse_nexus(raw: &str) -> Result<Vec<Tree>> {
    let mut trees = Vec::new();

    for line in raw.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('[') {
            continue;
        }

        if trimmed.to_ascii_lowercase().starts_with("tree ") {
            let (label, newick) = parse_nexus_tree_line(trimmed)?;
            let index = trees.len();
            let tree = build_tree(index, label, newick)?;
            trees.push(tree);
        }
    }

    Ok(trees)
}

fn build_tree(index: usize, label: Option<String>, newick: String) -> Result<Tree> {
    let phylo = PhyloTree::from_newick(&newick)
        .map_err(|err| anyhow!("failed to parse newick tree: {err}"))?;
    let canonical_newick = phylo.to_newick().unwrap_or_else(|_| newick.clone());

    Ok(Tree::new(index, label, canonical_newick, phylo))
}

fn parse_nexus_tree_line(line: &str) -> Result<(Option<String>, String)> {
    let mut parts = line.splitn(2, '=');
    let lhs = parts
        .next()
        .ok_or_else(|| anyhow!("missing tree identifier in nexus line: {line}"))?;
    let rhs = parts
        .next()
        .ok_or_else(|| anyhow!("missing tree definition in nexus line: {line}"))?;

    let label = lhs
        .split_whitespace()
        .nth(1)
        .map(|value| value.trim().trim_matches('"').to_owned());

    let mut payload = rhs.trim();
    if payload.starts_with("[&") {
        if let Some(idx) = payload.find(']') {
            payload = payload[idx + 1..].trim();
        }
    }

    let newick = normalise_newick(payload);
    Ok((label, newick))
}

fn normalise_newick(raw: &str) -> String {
    let mut cleaned = raw.trim().trim_end_matches(';').trim().to_owned();
    cleaned.push(';');
    cleaned
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_simple_newick() {
        let input = "(A:0.1,B:0.2);";
        let trees = parse_newick(input).unwrap();
        assert_eq!(trees.len(), 1);
        let tree = &trees[0];
        assert_eq!(tree.newick, "(A:0.1,B:0.2);");
        assert_eq!(tree.leaf_count(), 2);
        assert!(tree.root.is_some());
        assert_eq!(tree.phylo.size(), tree.nodes.len());
    }

    #[test]
    fn parses_simple_nexus() {
        let input = "#NEXUS\nBEGIN TREES;\nTREE tree1 = [&R] (A:0.1,B:0.2);\nEND;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 1);
        assert_eq!(trees[0].label.as_deref(), Some("tree1"));
        assert_eq!(trees[0].leaf_count(), 2);
    }
}
