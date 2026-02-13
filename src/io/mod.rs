use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use anyhow::{anyhow, bail, Context, Result};
use phylotree::tree::{NewickFormat, Tree as PhyloTree};

use crate::tree::{Tree, TreeBundle, TreeFileFormat};

pub fn load_trees(path: &Path) -> Result<TreeBundle> {
    let raw = fs::read_to_string(path)
        .with_context(|| format!("failed to read tree file: {}", path.display()))?;

    let format = detect_format(path, &raw);
    let (trees, metadata) = match format {
        TreeFileFormat::Newick => (parse_newick(&raw)?, BTreeMap::new()),
        TreeFileFormat::Nexus => (parse_nexus(&raw)?, BTreeMap::new()),
        TreeFileFormat::Rtr => parse_rtr(&raw)?,
    };

    if trees.is_empty() {
        bail!("tree file did not contain any trees");
    }

    let mut bundle = TreeBundle::new(format, trees);
    bundle.metadata = metadata;
    Ok(bundle)
}

fn detect_format(path: &Path, raw: &str) -> TreeFileFormat {
    if path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.eq_ignore_ascii_case("rtr"))
        .unwrap_or(false)
    {
        return TreeFileFormat::Rtr;
    }

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
        if upper.starts_with("BEGIN RHAETREE") {
            return TreeFileFormat::Rtr;
        }
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
    let mut in_translate_block = false;
    let mut translate_map = BTreeMap::new();
    let mut current_tree = String::new();
    let mut current_translate = String::new();

    for line in raw.lines() {
        let trimmed = line.trim();

        // Skip empty lines
        if trimmed.is_empty() {
            continue;
        }

        let upper_line = trimmed.to_ascii_uppercase();

        // Check for TREES block start
        if upper_line.starts_with("BEGIN TREES") {
            in_trees_block = true;
            continue;
        }

        // Check for block end
        if upper_line.starts_with("END") || upper_line.starts_with("ENDBLOCK") {
            in_trees_block = false;
            in_translate_block = false;
            current_translate.clear();

            // Process any accumulated tree definition.
            if !current_tree.is_empty() {
                if let Ok((label, newick)) = parse_nexus_tree_line(&current_tree) {
                    let index = trees.len();
                    if let Ok(mut tree) = build_tree(index, label, newick) {
                        apply_translate_map_to_tree(&mut tree, &translate_map);
                        trees.push(tree);
                    }
                }
                current_tree.clear();
            }
            continue;
        }

        // Process lines within TREES block
        if in_trees_block {
            let lower_line = trimmed.to_ascii_lowercase();

            // Handle TRANSLATE command (often present in BEAST/MrBayes output)
            if in_translate_block || lower_line.starts_with("translate") {
                if !current_translate.is_empty() {
                    current_translate.push(' ');
                }
                current_translate.push_str(trimmed);

                if statement_complete(&current_translate) {
                    parse_nexus_translate_statement(&current_translate, &mut translate_map);
                    current_translate.clear();
                    in_translate_block = false;
                } else {
                    in_translate_block = true;
                }
                continue;
            }

            if current_tree.is_empty() {
                // Ignore non-tree lines and standalone comments.
                if lower_line.starts_with('[') {
                    continue;
                }
                if !(lower_line.starts_with("tree ") || lower_line.starts_with("utree ")) {
                    continue;
                }
            }

            if !current_tree.is_empty() {
                current_tree.push(' ');
            }
            current_tree.push_str(trimmed);

            if statement_complete(&current_tree) {
                let statement = statement_prefix_until_semicolon(&current_tree);
                if let Ok((label, newick)) = parse_nexus_tree_line(statement) {
                    let index = trees.len();
                    if let Ok(mut tree) = build_tree(index, label, newick) {
                        apply_translate_map_to_tree(&mut tree, &translate_map);
                        trees.push(tree);
                    }
                }
                current_tree.clear();
            }
        }
    }

    // Handle any remaining tree lines.
    if !current_tree.is_empty() {
        if let Ok((label, newick)) = parse_nexus_tree_line(&current_tree) {
            let index = trees.len();
            if let Ok(mut tree) = build_tree(index, label, newick) {
                apply_translate_map_to_tree(&mut tree, &translate_map);
                trees.push(tree);
            }
        }
    }

    Ok(trees)
}

fn parse_rtr(raw: &str) -> Result<(Vec<Tree>, BTreeMap<String, String>)> {
    let trees = parse_nexus(raw)?;
    let metadata = parse_rtr_settings_block(raw);
    Ok((trees, metadata))
}

fn parse_rtr_settings_block(raw: &str) -> BTreeMap<String, String> {
    let mut settings = BTreeMap::new();
    let mut in_block = false;

    for line in raw.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let upper = trimmed.to_ascii_uppercase();
        if upper.starts_with("BEGIN RHAETREE")
        {
            in_block = true;
            continue;
        }
        if in_block && (upper.starts_with("END") || upper.starts_with("ENDBLOCK")) {
            break;
        }
        if !in_block {
            continue;
        }

        if !trimmed.to_ascii_lowercase().starts_with("set ") {
            continue;
        }

        let body = trimmed[4..].trim().trim_end_matches(';').trim();
        let mut parts = body.splitn(2, '=');
        let Some(key) = parts.next().map(str::trim) else {
            continue;
        };
        let Some(value) = parts.next().map(str::trim) else {
            continue;
        };
        let value = value.trim_matches('"').trim_matches('\'').to_string();
        if !key.is_empty() {
            settings.insert(key.to_string(), value);
        }
    }

    settings
}

pub fn save_rtr(
    path: &Path,
    trees: &[Tree],
    settings: &BTreeMap<String, String>,
) -> Result<()> {
    if trees.is_empty() {
        bail!("no trees to export");
    }

    let mut out = String::new();
    out.push_str("#NEXUS\n");

    let taxa = taxa_labels_from_tree(&trees[0]);
    out.push_str("begin taxa;\n");
    out.push_str(&format!("\tdimensions ntax={};\n", taxa.len()));
    out.push_str("\ttaxlabels\n");
    for label in taxa {
        out.push_str("\t");
        out.push_str(&quote_taxon_label(&label));
        out.push('\n');
    }
    out.push_str("\t;\n");
    out.push_str("end;\n\n");

    out.push_str("begin trees;\n");
    for (i, tree) in trees.iter().enumerate() {
        let tree_name = tree
            .label
            .clone()
            .unwrap_or_else(|| format!("tree_{}", i + 1));
        let mut newick = tree.newick.trim().to_string();
        if !newick.ends_with(';') {
            newick.push(';');
        }
        out.push_str(&format!(
            "\ttree {} = [&R] {}\n",
            quote_taxon_label(&tree_name),
            newick
        ));
    }
    out.push_str("end;\n\n");

    out.push_str("begin rhaetree;\n");
    out.push_str("\tset rtr.version=\"1\";\n");
    for (key, value) in settings {
        out.push_str(&format!("\tset {}={};\n", key, format_setting_value(value)));
    }
    out.push_str("end;\n");

    fs::write(path, out).with_context(|| format!("failed to write RTR file: {}", path.display()))
}

fn taxa_labels_from_tree(tree: &Tree) -> Vec<String> {
    tree.nodes
        .iter()
        .filter(|n| n.is_leaf())
        .map(|n| {
            n.name
                .clone()
                .or_else(|| n.label.clone())
                .unwrap_or_else(|| format!("tip_{}", n.id))
        })
        .collect()
}

fn quote_taxon_label(label: &str) -> String {
    if label.is_empty() {
        return "''".to_string();
    }
    let needs_quote = label.chars().any(|c| {
        c.is_whitespace()
            || matches!(
                c,
                '(' | ')' | '[' | ']' | '{' | '}' | ':' | ';' | ',' | '\'' | '"' | '='
            )
    });
    if needs_quote {
        format!("'{}'", label.replace('\'', "''"))
    } else {
        label.to_string()
    }
}

fn format_setting_value(value: &str) -> String {
    if value.eq_ignore_ascii_case("true")
        || value.eq_ignore_ascii_case("false")
        || value.eq_ignore_ascii_case("null")
        || value.parse::<f64>().is_ok()
        || value.starts_with('#')
    {
        value.to_string()
    } else {
        format!("\"{}\"", value.replace('"', "\\\""))
    }
}

fn build_tree(index: usize, label: Option<String>, newick: String) -> Result<Tree> {
    match parse_phylo_with_fallback(&newick) {
        Ok((phylo, canonical_newick)) => Ok(Tree::new(index, label, canonical_newick, phylo)),
        Err(err) => Err(anyhow!("failed to parse newick tree: {err}")),
    }
}

fn parse_phylo_with_fallback(newick: &str) -> Result<(PhyloTree, String)> {
    if let Ok(phylo) = PhyloTree::from_newick(newick) {
        let canonical_newick = phylo
            .to_formatted_newick(NewickFormat::NoComments)
            .unwrap_or_else(|_| newick.to_string());
        return Ok((phylo, canonical_newick));
    }

    // Compatibility fallback for legacy FigTree-style nested label annotations:
    // [&label="[&label=0.98]"] -> [&label=0.98]
    let sanitized = sanitize_nested_label_annotations(newick);
    if sanitized != newick {
        let phylo = PhyloTree::from_newick(&sanitized)
            .map_err(|err| anyhow!("failed to parse sanitized newick tree: {err}"))?;
        let canonical_newick = phylo
            .to_formatted_newick(NewickFormat::NoComments)
            .unwrap_or(sanitized);
        return Ok((phylo, canonical_newick));
    }

    PhyloTree::from_newick(newick)
        .map(|phylo| {
            let canonical_newick = phylo
                .to_formatted_newick(NewickFormat::NoComments)
                .unwrap_or_else(|_| newick.to_string());
            (phylo, canonical_newick)
        })
        .map_err(|err| anyhow!("{err}"))
}

fn sanitize_nested_label_annotations(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut i = 0usize;
    while i < s.len() {
        let rem = &s[i..];
        if rem.starts_with('[') {
            if let Some(end_rel) = matching_bracket_end(rem) {
                let comment = &rem[..end_rel];
                if let Some(value) = extract_nested_label_value(comment) {
                    out.push_str("[&label=");
                    out.push_str(value);
                    out.push(']');
                } else {
                    out.push_str(comment);
                }
                i += end_rel;
                continue;
            }
        }
        let ch = rem.chars().next().unwrap_or('\0');
        out.push(ch);
        i += ch.len_utf8();
    }
    out
}

fn extract_nested_label_value(comment: &str) -> Option<&str> {
    let prefix = "[&label=\"[&label=";
    let suffix = "]\"]";
    if comment.starts_with(prefix) && comment.ends_with(suffix) {
        let start = prefix.len();
        let end = comment.len().saturating_sub(suffix.len());
        if start < end {
            return Some(&comment[start..end]);
        }
    }
    None
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
                    .to_owned(),
            )
        }
    };

    // Process tree definition
    let mut payload = tree_part.trim();

    // Truncate at the first true statement terminator.
    if let Some(idx) = first_statement_semicolon(payload) {
        payload = &payload[..idx];
    }
    payload = payload.trim();

    // Handle BEAST/FigTree annotations [&R] or other metadata
    while payload.starts_with('[') {
        if let Some(end_idx) = matching_bracket_end(payload) {
            payload = payload[end_idx..].trim_start();
        } else {
            break;
        }
    }

    // Normalise to ensure we have a trailing semicolon, but preserve annotations for downstream parsing
    let newick = normalise_newick(payload);

    Ok((label, newick))
}

fn parse_nexus_translate_statement(statement: &str, translate_map: &mut BTreeMap<String, String>) {
    let mut body = statement.trim();
    if body.len() >= "translate".len() && body[..9].eq_ignore_ascii_case("translate") {
        body = body[9..].trim_start();
    }
    if let Some(idx) = first_statement_semicolon(body) {
        body = &body[..idx];
    }

    for entry in split_unquoted_commas(body) {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let Some((raw_key, raw_value)) = split_once_unquoted_whitespace(entry) else {
            continue;
        };
        let key = normalize_nexus_token(raw_key);
        let value = normalize_nexus_token(raw_value);
        if !key.is_empty() && !value.is_empty() {
            translate_map.insert(key, value);
        }
    }
}

fn apply_translate_map_to_tree(tree: &mut Tree, translate_map: &BTreeMap<String, String>) {
    if translate_map.is_empty() {
        return;
    }
    for node in &mut tree.nodes {
        if !node.is_leaf() {
            continue;
        }
        let Some(name) = node.name.as_ref() else {
            continue;
        };
        if let Some(mapped) = translate_map.get(name) {
            node.name = Some(mapped.clone());
        }
    }
}

fn split_unquoted_commas(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut prev = '\0';
    for (i, ch) in s.char_indices() {
        if ch == '\'' && !in_double && prev != '\\' {
            in_single = !in_single;
        } else if ch == '"' && !in_single && prev != '\\' {
            in_double = !in_double;
        } else if ch == ',' && !in_single && !in_double {
            parts.push(&s[start..i]);
            start = i + ch.len_utf8();
        }
        prev = ch;
    }
    if start <= s.len() {
        parts.push(&s[start..]);
    }
    parts
}

fn split_once_unquoted_whitespace(s: &str) -> Option<(&str, &str)> {
    let mut in_single = false;
    let mut in_double = false;
    let mut prev = '\0';
    for (i, ch) in s.char_indices() {
        if ch == '\'' && !in_double && prev != '\\' {
            in_single = !in_single;
        } else if ch == '"' && !in_single && prev != '\\' {
            in_double = !in_double;
        } else if ch.is_whitespace() && !in_single && !in_double {
            let key = &s[..i];
            let value = s[i..].trim_start();
            if !key.is_empty() && !value.is_empty() {
                return Some((key, value));
            }
            return None;
        }
        prev = ch;
    }
    None
}

fn normalize_nexus_token(token: &str) -> String {
    let token = token.trim();
    if token.len() >= 2
        && ((token.starts_with('\'') && token.ends_with('\''))
            || (token.starts_with('"') && token.ends_with('"')))
    {
        let inner = &token[1..token.len() - 1];
        if token.starts_with('\'') {
            return inner.replace("''", "'");
        }
        return inner.replace("\"\"", "\"");
    }
    token.to_string()
}

fn statement_complete(s: &str) -> bool {
    first_statement_semicolon(s).is_some()
}

fn statement_prefix_until_semicolon(s: &str) -> &str {
    if let Some(idx) = first_statement_semicolon(s) {
        &s[..=idx]
    } else {
        s
    }
}

fn first_statement_semicolon(s: &str) -> Option<usize> {
    let mut in_single = false;
    let mut in_double = false;
    let mut bracket_depth = 0usize;
    let mut prev = '\0';
    for (i, ch) in s.char_indices() {
        if ch == '\'' && !in_double && prev != '\\' {
            in_single = !in_single;
        } else if ch == '"' && !in_single && prev != '\\' {
            in_double = !in_double;
        } else if !in_single && !in_double {
            if ch == '[' {
                bracket_depth = bracket_depth.saturating_add(1);
            } else if ch == ']' {
                bracket_depth = bracket_depth.saturating_sub(1);
            } else if ch == ';' && bracket_depth == 0 {
                return Some(i);
            }
        }
        prev = ch;
    }
    None
}

fn matching_bracket_end(s: &str) -> Option<usize> {
    if !s.starts_with('[') {
        return None;
    }
    let mut in_single = false;
    let mut in_double = false;
    let mut depth = 0usize;
    let mut prev = '\0';
    for (i, ch) in s.char_indices() {
        if ch == '\'' && !in_double && prev != '\\' {
            in_single = !in_single;
        } else if ch == '"' && !in_single && prev != '\\' {
            in_double = !in_double;
        } else if !in_single && !in_double {
            if ch == '[' {
                depth += 1;
            } else if ch == ']' {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(i + ch.len_utf8());
                }
            }
        }
        prev = ch;
    }
    None
}

// Helper function to process annotations within the Newick string
fn normalise_newick(raw: &str) -> String {
    let mut cleaned = raw.trim().trim_end_matches(';').trim().to_owned();
    cleaned.push(';');
    cleaned
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn detects_format_correctly() {
        assert_eq!(
            detect_format(Path::new("x.nex"), "#NEXUS\nBEGIN TREES;"),
            TreeFileFormat::Nexus
        );
        assert_eq!(
            detect_format(Path::new("x.tre"), "(A:0.1,B:0.2);"),
            TreeFileFormat::Newick
        );
        assert_eq!(
            detect_format(Path::new("x.nex"), "   #nexus   \n"),
            TreeFileFormat::Nexus
        );
        assert_eq!(
            detect_format(Path::new("x.tre"), "[comment]\n(A,B);"),
            TreeFileFormat::Newick
        );
        assert_eq!(
            detect_format(Path::new("x.rtr"), "#NEXUS\nBEGIN TREES;"),
            TreeFileFormat::Rtr
        );
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
    fn parses_nexus_with_hpd_annotations() {
        let input = "#NEXUS
BEGIN TREES;
    UTREE 1 = (((Sly: 1.108043, (Mtr: 1.025109, (Ppr: 0.931515, Ath: 0.931515) [&95%HPD={0.884857, 0.976439}]: 0.093594) [&95%HPD={1.00855, 1.04577}]: 0.082934) [&95%HPD={1.08281, 1.1334}]: 0.084596, (Atr: 0.501430, ((Bvu: 0.393534, (Sol: 0.277897, (Cqu_subA: 0.074266, Cqu_subB: 0.074266) [&95%HPD={0.0514245, 0.0989838}]: 0.203632) [&95%HPD={0.217687, 0.342218}]: 0.115637) [&95%HPD={0.32271, 0.466057}]: 0.029666, (Ham: 0.335063, ((Sgl_subA: 0.011259, Sgl_subB: 0.011259) [&95%HPD={0.0076616, 0.0151649}]: 0.243167, (Sbi_subA: 0.060486, Sbi_subB: 0.060486) [&95%HPD={0.0417489, 0.0801152}]: 0.193939) [&95%HPD={0.19932, 0.311098}]: 0.080638) [&95%HPD={0.27219, 0.400269}]: 0.088137) [&95%HPD={0.349973, 0.498591}]: 0.078230) [&95%HPD={0.417616, 0.590171}]: 0.691209) [&95%HPD={1.17014, 1.20772}]: 0.343069, Osa: 1.535707) [&95%HPD={1.42368, 1.64135}];
END;";

        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 1);
        let tree = &trees[0];

        assert!(tree
            .node_numeric_attribute_keys()
            .contains(&"95%HPD".to_string()));

        let ranges: Vec<(f64, f64)> = tree
            .nodes
            .iter()
            .filter_map(|node| node.numeric_range_attribute("95%HPD"))
            .collect();

        assert!(!ranges.is_empty());

        let has_expected_range = ranges
            .iter()
            .any(|(min, max)| (min - 1.42368).abs() < 1e-5 && (max - 1.64135).abs() < 1e-5);

        assert!(has_expected_range);
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

    #[test]
    fn parses_rtr_settings_block() {
        let input = "#NEXUS
BEGIN TREES;
    TREE tree1 = (A:0.1,B:0.2);
END;
BEGIN RHAETREE;
    set layout.type=\"radial\";
    set painter.branchLineWidth=2.5;
END;";
        let (trees, metadata) = parse_rtr(input).unwrap();
        assert_eq!(trees.len(), 1);
        assert_eq!(metadata.get("layout.type").map(String::as_str), Some("radial"));
        assert_eq!(
            metadata.get("painter.branchLineWidth").map(String::as_str),
            Some("2.5")
        );
    }

    #[test]
    fn parses_nexus_with_nested_bracket_in_label_annotation() {
        let input = "#NEXUS
BEGIN TREES;
    tree tree_1 = [&R] ((A:0.1,B:0.2)[&label=\"[&label=0.98]\"]:0.3,C:0.4);
END;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 1);
        assert_eq!(trees[0].label.as_deref(), Some("tree_1"));
        assert_eq!(trees[0].leaf_count(), 3);
    }

    #[test]
    fn parses_nexus_with_translate_map() {
        let input = "#NEXUS
BEGIN TREES;
    Translate
        1 Alpha_taxon,
        2 'Beta taxon';
    TREE tree1 = [&R] (1:0.1,2:0.2);
END;";
        let trees = parse_nexus(input).unwrap();
        assert_eq!(trees.len(), 1);

        let mut leaf_names: Vec<String> = trees[0]
            .nodes
            .iter()
            .filter(|n| n.is_leaf())
            .filter_map(|n| n.name.clone())
            .collect();
        leaf_names.sort();

        assert_eq!(leaf_names, vec!["Alpha_taxon".to_string(), "Beta taxon".to_string()]);
    }
}
