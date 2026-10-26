use eframe::egui;

use crate::tree::attributes::{attribute_label, AttributeKind, AttributeScope, InternalLabelMode};
use crate::tree::colouring::{ColourMapping, ColourMode};
use crate::tree::Tree;

use super::FigTreeGui;

impl FigTreeGui {
    pub(super) fn attribute_controls(&mut self, ui: &mut egui::Ui) {
        let Some(tree) = self.tree_viewer.current_tree() else {
            return;
        };
        let mut label_mode = tree.internal_label_mode;
        let mut branch = self.tree_painter.branch_colour_mapping.clone();
        let mut tip_label = self.tree_painter.tip_label_colour_mapping.clone();
        let mut tip_shape = self.tree_painter.tip_shape_colour_mapping.clone();
        let mut node_shape = self.tree_painter.node_shape_colour_mapping.clone();

        ui.separator();
        ui.label("Internal labels in this tree:");
        egui::ComboBox::from_id_salt("internal_label_mode")
            .selected_text(label_mode.label())
            .show_ui(ui, |ui| {
                for mode in InternalLabelMode::ALL {
                    ui.selectable_value(&mut label_mode, mode, mode.label());
                }
            });
        ui.small("Original labels are preserved. Auto keeps quoted numeric names as names. Support 1/2 retain the original a/b order.");
        if label_mode != tree.internal_label_mode {
            self.record_undo_step();
            self.tree_viewer.set_internal_label_mode(label_mode);
            self.sync_annotation_snapshot();
            self.invalidate_layer_caches();
            return;
        }

        colour_controls(ui, "Branches", tree, AttributeScope::All, &mut branch);
        colour_controls(ui, "Tip labels", tree, AttributeScope::Tips, &mut tip_label);
        colour_controls(ui, "Tip shapes", tree, AttributeScope::Tips, &mut tip_shape);
        colour_controls(
            ui,
            "Node shapes",
            tree,
            AttributeScope::Internal,
            &mut node_shape,
        );
        ui.small("Manual colours override attribute colours. Categories keep their colours across trees.");
        if branch != self.tree_painter.branch_colour_mapping
            || tip_label != self.tree_painter.tip_label_colour_mapping
            || tip_shape != self.tree_painter.tip_shape_colour_mapping
            || node_shape != self.tree_painter.node_shape_colour_mapping
        {
            self.record_undo_step();
            self.tree_painter.branch_colour_mapping = branch;
            self.tree_painter.tip_label_colour_mapping = tip_label;
            self.tree_painter.tip_shape_colour_mapping = tip_shape;
            self.tree_painter.node_shape_colour_mapping = node_shape;
        }
        self.selected_attribute_controls(ui);
    }

    fn selected_attribute_controls(&mut self, ui: &mut egui::Ui) {
        let selected = self
            .tree_viewer
            .selected_tips()
            .iter()
            .min()
            .copied()
            .or(self.tree_viewer.selected_branch())
            .or_else(|| self.tree_viewer.selected_nodes().iter().min().copied());
        let Some(node_id) = selected else {
            return;
        };
        let Some(node) = self
            .tree_viewer
            .current_tree()
            .and_then(|tree| tree.node(node_id))
        else {
            return;
        };
        let mut fields: Vec<_> = node
            .attributes
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        fields.sort();
        let mut apply = false;
        ui.collapsing(format!("Selected node {node_id}: attributes"), |ui| {
            for (key, value) in &fields {
                if ui
                    .selectable_label(self.attribute_edit_key == *key, format!("{key} = {value}"))
                    .clicked()
                {
                    self.attribute_edit_key = key.clone();
                    self.attribute_edit_value = value.clone();
                }
            }
            ui.label("Field:");
            ui.text_edit_singleline(&mut self.attribute_edit_key);
            ui.label("Value:");
            ui.text_edit_singleline(&mut self.attribute_edit_value);
            let valid_key = !self.attribute_edit_key.is_empty()
                && !self.attribute_edit_key.starts_with('!')
                && self
                    .attribute_edit_key
                    .chars()
                    .all(|c| c.is_alphanumeric() || "_.%/-".contains(c));
            ui.small("Use a field such as posterior, bootstrap, host or location.");
            apply = ui
                .add_enabled(valid_key, egui::Button::new("Set attribute"))
                .clicked();
        });
        if apply {
            self.record_undo_step();
            self.tree_viewer.set_node_attribute(
                node_id,
                &self.attribute_edit_key,
                &self.attribute_edit_value,
            );
            self.sync_annotation_snapshot();
            self.invalidate_layer_caches();
        }
    }

    fn sync_annotation_snapshot(&mut self) {
        if let (Some(snapshot), Some(tree)) = (
            &mut self.user_root_snapshot,
            self.tree_viewer.current_tree(),
        ) {
            snapshot.sync_annotations(self.tree_viewer.current_tree_index(), tree);
        }
    }
}

fn colour_controls(
    ui: &mut egui::Ui,
    title: &str,
    tree: &Tree,
    scope: AttributeScope,
    mapping: &mut ColourMapping,
) {
    ui.push_id(title, |ui| {
        ui.label(format!("{title} colour by:"));
        attribute_selector(
            ui,
            "colour_attribute",
            tree,
            scope,
            &mut mapping.attribute,
            "Fixed / manual",
        );
        if mapping.attribute.is_none() {
            return;
        }
        egui::ComboBox::from_id_salt("colour_mode")
            .selected_text(mapping.mode.key())
            .show_ui(ui, |ui| {
                for mode in [
                    ColourMode::Auto,
                    ColourMode::Continuous,
                    ColourMode::Categorical,
                ] {
                    ui.selectable_value(&mut mapping.mode, mode, mode.key());
                }
            });
        let numeric = mapping.mode == ColourMode::Continuous
            || (mapping.mode == ColourMode::Auto
                && tree.attribute_catalogue(scope).iter().any(|info| {
                    Some(&info.key) == mapping.attribute.as_ref()
                        && info.kind == AttributeKind::Numeric
                }));
        if numeric {
            ui.horizontal(|ui| {
                ui.label("Low / high:");
                ui.color_edit_button_srgba(&mut mapping.low);
                ui.color_edit_button_srgba(&mut mapping.high);
            });
            let mut automatic = mapping.range.is_none();
            if ui
                .checkbox(&mut automatic, "Auto range in this tree")
                .changed()
            {
                mapping.range = if automatic { None } else { Some((0.0, 100.0)) };
            }
            if let Some((min, max)) = &mut mapping.range {
                ui.horizontal(|ui| {
                    ui.add(egui::DragValue::new(min).prefix("Min ").speed(0.01));
                    ui.add(egui::DragValue::new(max).prefix("Max ").speed(0.01));
                });
                if !min.is_finite() || !max.is_finite() || *min > *max {
                    ui.colored_label(
                        egui::Color32::DARK_RED,
                        "Invalid range; automatic range is used.",
                    );
                }
            }
        }
        ui.horizontal(|ui| {
            ui.label("Missing value:");
            ui.color_edit_button_srgba(&mut mapping.missing);
        });
    });
}

pub(super) fn attribute_selector(
    ui: &mut egui::Ui,
    id: &str,
    tree: &Tree,
    scope: AttributeScope,
    selected: &mut Option<String>,
    none_label: &str,
) {
    let catalogue = tree.attribute_catalogue(scope);
    egui::ComboBox::from_id_salt(id)
        .selected_text(
            selected
                .as_deref()
                .map(attribute_label)
                .unwrap_or(none_label),
        )
        .show_ui(ui, |ui| {
            ui.selectable_value(selected, None, none_label);
            for info in &catalogue {
                ui.selectable_value(
                    selected,
                    Some(info.key.clone()),
                    format!(
                        "{} ({:?}, {}/{})",
                        attribute_label(&info.key),
                        info.kind,
                        info.present,
                        info.total
                    ),
                );
            }
        });
    if let Some(key) = selected {
        if !catalogue.iter().any(|info| info.key == *key) {
            ui.small("This field is absent in the current tree.");
        }
    }
}
