use std::borrow::Cow;
use std::collections::BTreeSet;

use super::{Tree, TreeNode};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum InternalLabelMode {
    #[default]
    Auto,
    Names,
    Support,
    SupportPair,
    Bootstrap,
    Posterior,
    FastTreeLocal,
    IqTreeAlrtUfboot,
}

impl InternalLabelMode {
    pub const ALL: [Self; 8] = [
        Self::Auto,
        Self::Names,
        Self::Support,
        Self::SupportPair,
        Self::Bootstrap,
        Self::Posterior,
        Self::FastTreeLocal,
        Self::IqTreeAlrtUfboot,
    ];

    pub fn key(self) -> &'static str {
        match self {
            Self::Auto => "auto",
            Self::Names => "names",
            Self::Support => "support",
            Self::SupportPair => "support_pair",
            Self::Bootstrap => "bootstrap",
            Self::Posterior => "posterior",
            Self::FastTreeLocal => "fasttree_local",
            Self::IqTreeAlrtUfboot => "iqtree_alrt_ufboot",
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::Auto => "Auto (numeric support)",
            Self::Names => "Node names",
            Self::Support => "Single support value",
            Self::SupportPair => "Two support values (a/b)",
            Self::Bootstrap => "Bootstrap (0-100)",
            Self::Posterior => "Posterior probability (0-1)",
            Self::FastTreeLocal => "FastTree local support (0-1)",
            Self::IqTreeAlrtUfboot => "IQ-TREE: SH-aLRT / UFBoot",
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|mode| mode.key() == value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeScope {
    All,
    Tips,
    Internal,
}

impl AttributeScope {
    pub fn includes(self, node: &TreeNode) -> bool {
        match self {
            Self::All => true,
            Self::Tips => node.is_leaf(),
            Self::Internal => !node.is_leaf(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeKind {
    Numeric,
    Range,
    Categorical,
    Mixed,
}

#[derive(Debug, Clone)]
pub struct AttributeInfo {
    pub key: String,
    pub kind: AttributeKind,
    pub present: usize,
    pub total: usize,
}

pub fn attribute_label(key: &str) -> &str {
    match key {
        "!name" => "Original name",
        "!label" => "Annotation label",
        "!support" => "Internal support",
        "!support.1" => "Internal support 1",
        "!support.2" => "Internal support 2",
        "!length" => "Branch length",
        _ => key,
    }
}

fn support_number(value: &str) -> Option<f64> {
    let number = value.trim().parse::<f64>().ok()?;
    (number.is_finite() && (0.0..=100.0).contains(&number)).then_some(number)
}

impl Tree {
    /// These are virtual fields: the original name and explicit annotations are
    /// never rewritten or relabelled as a particular support algorithm.
    pub fn attribute_value<'a>(&'a self, node: &'a TreeNode, key: &str) -> Option<Cow<'a, str>> {
        // Explicitly named annotations always win over a chosen interpretation.
        if !key.starts_with('!') {
            if let Some(value) = node.get_attribute(key) {
                return Some(Cow::Borrowed(value));
            }
            let derived = match (self.internal_label_mode, key) {
                (InternalLabelMode::Bootstrap, "bootstrap") => Some("!support"),
                (InternalLabelMode::Posterior, "posterior") => Some("!support"),
                (InternalLabelMode::FastTreeLocal, "local_support") => Some("!support"),
                (InternalLabelMode::IqTreeAlrtUfboot, "SH-aLRT") => Some("!support.1"),
                (InternalLabelMode::IqTreeAlrtUfboot, "UFboot") => Some("!support.2"),
                _ => None,
            };
            if let Some(derived) = derived {
                return self.attribute_value(node, derived);
            }
        }
        match key {
            "!name" => node.name.as_deref().map(Cow::Borrowed),
            "!label" => node.label.as_deref().map(Cow::Borrowed),
            "!length" => node
                .length
                .filter(|n| n.is_finite())
                .map(|n| Cow::Owned(n.to_string())),
            "!support" | "!support.1" | "!support.2" => {
                if node.is_leaf() || self.internal_label_mode == InternalLabelMode::Names {
                    return None;
                }
                let raw = node.name.as_deref().or(node.label.as_deref())?;
                if self.internal_label_mode == InternalLabelMode::Auto
                    && node.name.is_some()
                    && node.name_quoted
                {
                    return None;
                }
                let paired = matches!(
                    self.internal_label_mode,
                    InternalLabelMode::SupportPair | InternalLabelMode::IqTreeAlrtUfboot
                );
                if key == "!support" && !paired {
                    let number = support_number(raw)?;
                    if matches!(
                        self.internal_label_mode,
                        InternalLabelMode::Posterior | InternalLabelMode::FastTreeLocal
                    ) && number > 1.0
                    {
                        return None;
                    }
                    return Some(Cow::Borrowed(raw));
                }
                if matches!(
                    self.internal_label_mode,
                    InternalLabelMode::Support
                        | InternalLabelMode::Bootstrap
                        | InternalLabelMode::Posterior
                        | InternalLabelMode::FastTreeLocal
                ) {
                    return None;
                }
                let (first, second) = raw.split_once('/')?;
                support_number(first)?;
                support_number(second)?;
                match key {
                    "!support.1" => Some(Cow::Borrowed(first.trim())),
                    "!support.2" => Some(Cow::Borrowed(second.trim())),
                    _ => None,
                }
            }
            _ => node.get_attribute(key).map(|s| Cow::Borrowed(s.as_str())),
        }
    }

    pub fn numeric_attribute_value(&self, node: &TreeNode, key: &str) -> Option<f64> {
        if !key.starts_with('!') && node.attributes.contains_key(key) {
            return node.get_numeric_attribute(key).filter(|n| n.is_finite());
        }
        let value = self.attribute_value(node, key)?;
        let number = value.trim().parse::<f64>().ok()?;
        number.is_finite().then_some(number)
    }

    pub fn attribute_catalogue(&self, scope: AttributeScope) -> Vec<AttributeInfo> {
        let nodes: Vec<_> = self.nodes.iter().filter(|n| scope.includes(n)).collect();
        let mut keys: BTreeSet<_> = self
            .node_attribute_keys()
            .into_iter()
            .filter(|k| !k.starts_with("!rhaetree"))
            .collect();
        keys.extend(
            [
                "!name",
                "!label",
                "!length",
                "!support",
                "!support.1",
                "!support.2",
            ]
            .map(str::to_string),
        );
        keys.extend(
            [
                "bootstrap",
                "posterior",
                "local_support",
                "SH-aLRT",
                "UFboot",
            ]
            .map(str::to_string),
        );
        keys.into_iter()
            .filter_map(|key| {
                let mut kinds = [false; 3];
                let mut present = 0;
                for node in &nodes {
                    let Some(_) = self
                        .attribute_value(node, &key)
                        .filter(|v| !v.trim().is_empty())
                    else {
                        continue;
                    };
                    present += 1;
                    let index = if self.numeric_attribute_value(node, &key).is_some() {
                        0
                    } else if node.numeric_attributes.get(&key).is_some_and(|v| {
                        !matches!(v, super::NodeNumericValue::Scalar(_)) && v.as_range().is_some()
                    }) {
                        1
                    } else {
                        2
                    };
                    kinds[index] = true;
                }
                if present == 0 {
                    return None;
                }
                let kind = match kinds {
                    [true, false, false] => AttributeKind::Numeric,
                    [false, true, false] => AttributeKind::Range,
                    [false, false, true] => AttributeKind::Categorical,
                    _ => AttributeKind::Mixed,
                };
                Some(AttributeInfo {
                    key,
                    kind,
                    present,
                    total: nodes.len(),
                })
            })
            .collect()
    }
}
