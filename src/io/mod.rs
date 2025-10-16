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
    // Check first non-empty, non-comment line
    for line in raw.lines() {
        let trimmed = line.trim();

        // Skip empty lines
        if trimmed.is_empty() {
            continue;
        }

        // Skip pure comment lines
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            continue;
        }

        // Check for NEXUS format indicator
        if trimmed.to_ascii_uppercase().starts_with("#NEXUS") {
            return TreeFileFormat::Nexus;
        }

        // If first significant line doesn't start with #NEXUS, it's likely Newick
        // Common Newick patterns: starts with '(' or a taxon name
        if trimmed.starts_with('(') || trimmed.contains("(") && trimmed.contains(")") {
            return TreeFileFormat::Newick;
        }

        // Additional Nexus indicators
        let upper = trimmed.to_ascii_uppercase();
        if upper.starts_with("BEGIN ") || upper.starts_with("TREE ") {
            return TreeFileFormat::Nexus;
        }
    }

    // Default to Newick if uncertain
    TreeFileFormat::Newick
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
    let mut in_trees_block = false;
    let mut current_tree_lines = Vec::new();

    for line in raw.lines() {
        let trimmed = line.trim();

        // Skip empty lines and comments
        if trimmed.is_empty() {
            continue;
        }

        // Handle comments (can be anywhere in the line)
        let line_without_comment = if let Some(comment_start) = trimmed.find('[') {
            if let Some(comment_end) = trimmed[comment_start..].find(']') {
                // Remove inline comment
                let before = &trimmed[..comment_start];
                let after = &trimmed[comment_start + comment_end + 1..];
                format!("{}{}", before, after).trim().to_string()
            } else {
                // Comment continues to next line, skip rest of line
                trimmed[..comment_start].trim().to_string()
            }
        } else {
            trimmed.to_string()
        };

        if line_without_comment.is_empty() {
            continue;
        }

        let upper_line = line_without_comment.to_ascii_uppercase();

        // Check for TREES block start
        if upper_line.starts_with("BEGIN TREES") {
            in_trees_block = true;
            continue;
        }

        // Check for block end
        if upper_line.starts_with("END") || upper_line.starts_with("ENDBLOCK") {
            in_trees_block = false;

            // Process any accumulated tree lines
            if !current_tree_lines.is_empty() {
                let full_tree_line = current_tree_lines.join(" ");
                if let Ok((label, newick)) = parse_nexus_tree_line(&full_tree_line) {
                    let index = trees.len();
                    if let Ok(tree) = build_tree(index, label, newick) {
                        trees.push(tree);
                    }
                }
                current_tree_lines.clear();
            }
            continue;
        }

        // Process lines within TREES block
        if in_trees_block {
            let lower_line = line_without_comment.to_ascii_lowercase();

            // Handle TRANSLATE command (often present in BEAST/MrBayes output)
            if lower_line.starts_with("translate") {
                // Skip translate block for now - phylotree should handle taxon names
                continue;
            }

            // Handle tree definitions
            if lower_line.starts_with("tree ") || lower_line.starts_with("utree ") {
                // If we have a previous tree being accumulated, process it first
                if !current_tree_lines.is_empty() {
                    let full_tree_line = current_tree_lines.join(" ");
                    if let Ok((label, newick)) = parse_nexus_tree_line(&full_tree_line) {
                        let index = trees.len();
                        if let Ok(tree) = build_tree(index, label, newick) {
                            trees.push(tree);
                        }
                    }
                    current_tree_lines.clear();
                }

                // Start accumulating new tree
                current_tree_lines.push(line_without_comment.clone());

                // Check if this line contains a complete tree (ends with semicolon)
                if line_without_comment.ends_with(';') {
                    let full_tree_line = current_tree_lines.join(" ");
                    if let Ok((label, newick)) = parse_nexus_tree_line(&full_tree_line) {
                        let index = trees.len();
                        if let Ok(tree) = build_tree(index, label, newick) {
                            trees.push(tree);
                        }
                    }
                    current_tree_lines.clear();
                }
            } else if !current_tree_lines.is_empty() {
                // Continue accumulating multi-line tree definition
                current_tree_lines.push(line_without_comment.clone());

                if line_without_comment.ends_with(';') {
                    let full_tree_line = current_tree_lines.join(" ");
                    if let Ok((label, newick)) = parse_nexus_tree_line(&full_tree_line) {
                        let index = trees.len();
                        if let Ok(tree) = build_tree(index, label, newick) {
                            trees.push(tree);
                        }
                    }
                    current_tree_lines.clear();
                }
            }
        }
    }

    // Handle any remaining tree lines
    if !current_tree_lines.is_empty() {
        let full_tree_line = current_tree_lines.join(" ");
        if let Ok((label, newick)) = parse_nexus_tree_line(&full_tree_line) {
            let index = trees.len();
            if let Ok(tree) = build_tree(index, label, newick) {
                trees.push(tree);
            }
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
    // Handle both "TREE" and "UTREE" keywords
    let tree_start = if line.to_ascii_lowercase().starts_with("tree ") {
        5
    } else if line.to_ascii_lowercase().starts_with("utree ") {
        6
    } else {
        return Err(anyhow!("Invalid tree line: {line}"));
    };

    let tree_def = &line[tree_start..].trim();

    // Split by '=' to separate label from tree definition
    let mut parts = tree_def.splitn(2, '=');
    let label_part = parts
        .next()
        .ok_or_else(|| anyhow!("missing tree identifier in nexus line: {line}"))?;
    let tree_part = parts
        .next()
        .ok_or_else(|| anyhow!("missing tree definition in nexus line: {line}"))?;

    // Extract label (can be quoted or unquoted, and may contain asterisk for rooted trees)
    let label = {
        let cleaned_label = label_part.trim().trim_start_matches('*');
        if cleaned_label.is_empty() {
            None
        } else {
            Some(
                cleaned_label
                    .trim_matches('"')
                    .trim_matches('\'')
                    .to_owned()
            )
        }
    };

    // Process tree definition
    let mut payload = tree_part.trim();

    // Remove trailing semicolon if present
    payload = payload.trim_end_matches(';').trim();

    // Handle BEAST/FigTree annotations [&R] or other metadata
    while payload.starts_with('[') {
        if let Some(end_idx) = payload.find(']') {
            payload = payload[end_idx + 1..].trim();
        } else {
            break;
        }
    }

    // Handle branch length annotations in the Newick string
    let newick = process_newick_annotations(payload);
    let newick = normalise_newick(&newick);

    Ok((label, newick))
}

// Helper function to process annotations within the Newick string
fn process_newick_annotations(newick: &str) -> String {
    let mut result = String::new();
    let mut in_annotation = false;
    let mut depth = 0;

    for ch in newick.chars() {
        match ch {
            '[' if !in_annotation => {
                in_annotation = true;
                depth = 1;
            },
            '[' if in_annotation => {
                depth += 1;
            },
            ']' if in_annotation => {
                depth -= 1;
                if depth == 0 {
                    in_annotation = false;
                }
            },
            _ if !in_annotation => {
                result.push(ch);
            },
            _ => {
                // Skip annotation content
            }
        }
    }

    result
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
    fn detects_format_correctly() {
        assert_eq!(detect_format("#NEXUS\nBEGIN TREES;"), TreeFileFormat::Nexus);
        assert_eq!(detect_format("(A:0.1,B:0.2);"), TreeFileFormat::Newick);
        assert_eq!(detect_format("   #nexus   \n"), TreeFileFormat::Nexus);
        assert_eq!(detect_format("[comment]\n(A,B);"), TreeFileFormat::Newick);
    }

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
    fn parses_multiple_newick() {
        let input = "(A:0.1,B:0.2);\n(C:0.3,D:0.4);\n";
        let trees = parse_newick(input).unwrap();
        assert_eq!(trees.len(), 2);
    }

    #[test]
    fn parses_simple_nexus() {
        let input = "#NEXUS\nBEGIN TREES;\nTREE tree1 = [&R] (A:0.1,B:0.2);\nEND;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 1);
        assert_eq!(trees[0].label.as_deref(), Some("tree1"));
        assert_eq!(trees[0].leaf_count(), 2);
    }

    #[test]
    fn parses_nexus_with_annotations() {
        let input = "#NEXUS
BEGIN TREES;
    TREE tree1 = [&R] ((A[&rate=0.5]:0.1,B:0.2)[&posterior=0.99]:0.3,C:0.4);
END;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 1);
        assert_eq!(trees[0].label.as_deref(), Some("tree1"));
        // Annotations should be stripped from the newick string
        assert!(!trees[0].newick.contains("[&"));
    }

    #[test]
    fn parses_nexus_multiline_tree() {
        let input = "#NEXUS
BEGIN TREES;
    TREE tree1 =
        (A:0.1,
         B:0.2);
END;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 1);
        assert_eq!(trees[0].label.as_deref(), Some("tree1"));
    }

    #[test]
    fn parses_nexus_with_comments() {
        let input = "#NEXUS
[This is a comment]
BEGIN TREES;
    TREE tree1 = (A:0.1,B:0.2); [inline comment]
    [Another comment]
    TREE tree2 = (C:0.3,D:0.4);
END;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 2);
        assert_eq!(trees[0].label.as_deref(), Some("tree1"));
        assert_eq!(trees[1].label.as_deref(), Some("tree2"));
    }

    #[test]
    fn parses_nexus_utree() {
        let input = "#NEXUS
BEGIN TREES;
    UTREE tree1 = (A:0.1,B:0.2);
END;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 1);
        assert_eq!(trees[0].label.as_deref(), Some("tree1"));
    }

    #[test]
    fn handles_quoted_labels() {
        let input = "#NEXUS
BEGIN TREES;
    TREE 'my tree' = (A:0.1,B:0.2);
    TREE \"another tree\" = (C:0.3,D:0.4);
END;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 2);
        assert_eq!(trees[0].label.as_deref(), Some("my tree"));
        assert_eq!(trees[1].label.as_deref(), Some("another tree"));
    }
}
