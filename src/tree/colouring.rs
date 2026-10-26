use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};

use eframe::egui::Color32;

use super::attributes::AttributeScope;
use super::{NodeId, Tree};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum ColourMode {
    #[default]
    Auto,
    Continuous,
    Categorical,
}

impl ColourMode {
    pub fn key(self) -> &'static str {
        match self {
            Self::Auto => "auto",
            Self::Continuous => "continuous",
            Self::Categorical => "categorical",
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "auto" => Some(Self::Auto),
            "continuous" => Some(Self::Continuous),
            "categorical" => Some(Self::Categorical),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ColourMapping {
    pub attribute: Option<String>,
    pub mode: ColourMode,
    pub low: Color32,
    pub high: Color32,
    pub missing: Color32,
    pub range: Option<(f64, f64)>,
}

impl Default for ColourMapping {
    fn default() -> Self {
        Self {
            attribute: None,
            mode: ColourMode::Auto,
            low: Color32::from_rgb(35, 110, 165),
            high: Color32::from_rgb(210, 75, 45),
            missing: Color32::from_gray(160),
            range: None,
        }
    }
}

impl ColourMapping {
    pub fn hash_state<H: Hasher>(&self, hasher: &mut H) {
        self.attribute.hash(hasher);
        self.mode.hash(hasher);
        self.low.to_array().hash(hasher);
        self.high.to_array().hash(hasher);
        self.missing.to_array().hash(hasher);
        self.range
            .map(|(min, max)| (min.to_bits(), max.to_bits()))
            .hash(hasher);
    }

    /// Resolve once per scene, rather than scanning all values for every branch.
    pub fn resolve(&self, tree: &Tree, scope: AttributeScope) -> Vec<Option<Color32>> {
        let mut colors = vec![None; tree.nodes.len()];
        let Some(key) = self.attribute.as_deref() else {
            return colors;
        };
        let values: Vec<_> = tree.nodes.iter().filter(|n| scope.includes(n)).collect();
        let numeric = match self.mode {
            ColourMode::Continuous => true,
            ColourMode::Categorical => false,
            ColourMode::Auto => {
                let present: Vec<_> = values
                    .iter()
                    .filter(|node| {
                        tree.attribute_value(node, key)
                            .is_some_and(|v| !v.trim().is_empty())
                    })
                    .collect();
                !present.is_empty()
                    && present
                        .iter()
                        .all(|node| tree.numeric_attribute_value(node, key).is_some())
            }
        };
        let range = self
            .range
            .filter(|(min, max)| min.is_finite() && max.is_finite() && min <= max)
            .or_else(|| {
                let mut min = f64::INFINITY;
                let mut max = f64::NEG_INFINITY;
                for node in &values {
                    if let Some(v) = tree.numeric_attribute_value(node, key) {
                        min = min.min(v);
                        max = max.max(v);
                    }
                }
                min.is_finite().then_some((min, max))
            });
        for node in values {
            let color = if numeric {
                match (tree.numeric_attribute_value(node, key), range) {
                    (Some(value), Some((min, max))) => {
                        let t = if min == max {
                            0.5
                        } else {
                            ((value - min) / (max - min)).clamp(0.0, 1.0)
                        };
                        interpolate(self.low, self.high, t as f32)
                    }
                    _ => self.missing,
                }
            } else {
                tree.attribute_value(node, key)
                    .filter(|v| !v.trim().is_empty())
                    .map(|v| category_color(&v))
                    .unwrap_or(self.missing)
            };
            colors[node.id] = Some(color);
        }
        colors
    }

    pub fn write_settings(&self, prefix: &str, settings: &mut BTreeMap<String, String>) {
        settings.insert(
            format!("{prefix}.attribute"),
            self.attribute.clone().unwrap_or_default(),
        );
        settings.insert(format!("{prefix}.mode"), self.mode.key().to_string());
        for (key, color) in [
            ("low", self.low),
            ("high", self.high),
            ("missing", self.missing),
        ] {
            settings.insert(
                format!("{prefix}.{key}"),
                format!(
                    "#{:02X}{:02X}{:02X}{:02X}",
                    color.r(),
                    color.g(),
                    color.b(),
                    color.a()
                ),
            );
        }
        if let Some((min, max)) = self.range {
            settings.insert(format!("{prefix}.min"), min.to_string());
            settings.insert(format!("{prefix}.max"), max.to_string());
        }
    }

    pub fn read_settings(&mut self, prefix: &str, settings: &BTreeMap<String, String>) {
        *self = Self::default();
        self.attribute = settings
            .get(&format!("{prefix}.attribute"))
            .filter(|s| !s.is_empty())
            .cloned();
        if let Some(mode) = settings
            .get(&format!("{prefix}.mode"))
            .and_then(|v| ColourMode::parse(v))
        {
            self.mode = mode;
        }
        for (key, target) in [
            ("low", &mut self.low),
            ("high", &mut self.high),
            ("missing", &mut self.missing),
        ] {
            if let Some(raw) = settings
                .get(&format!("{prefix}.{key}"))
                .and_then(|s| s.strip_prefix('#'))
            {
                if raw.len() == 8 {
                    if let Ok(v) = u32::from_str_radix(raw, 16) {
                        *target = Color32::from_rgba_unmultiplied(
                            (v >> 24) as u8,
                            (v >> 16) as u8,
                            (v >> 8) as u8,
                            v as u8,
                        );
                    }
                }
            }
        }
        self.range = settings
            .get(&format!("{prefix}.min"))
            .and_then(|s| s.parse::<f64>().ok())
            .zip(
                settings
                    .get(&format!("{prefix}.max"))
                    .and_then(|s| s.parse::<f64>().ok()),
            )
            .filter(|(min, max)| min.is_finite() && max.is_finite() && min <= max);
    }
}

pub fn mapped_color(colors: &[Option<Color32>], node: NodeId, fallback: Color32) -> Color32 {
    colors.get(node).copied().flatten().unwrap_or(fallback)
}

fn interpolate(low: Color32, high: Color32, t: f32) -> Color32 {
    let channel = |a: u8, b: u8| (a as f32 + (b as f32 - a as f32) * t).round() as u8;
    Color32::from_rgba_unmultiplied(
        channel(low.r(), high.r()),
        channel(low.g(), high.g()),
        channel(low.b(), high.b()),
        channel(low.a(), high.a()),
    )
}

fn category_color(value: &str) -> Color32 {
    // Stable across tree switching and category additions; independent of HashMap order.
    let hash = value.bytes().fold(2166136261u32, |h, b| {
        (h ^ u32::from(b)).wrapping_mul(16777619)
    });
    eframe::egui::ecolor::Hsva::new((hash % 360) as f32 / 360.0, 0.65, 0.78, 1.0).into()
}
