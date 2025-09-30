use std::path::PathBuf;

use eframe::egui::collapsing_header::{paint_default_icon, CollapsingState};
use eframe::egui::{self, color_picker, Color32};
use log::{error, info};
use rfd::FileDialog;

use crate::app::AppConfig;
use crate::io;
use crate::tree::painter::{TipLabelDisplay, TipLabelFontFamily, TipLabelNumberFormat};
use crate::tree::viewer::{SelectionMode, TextSearchType, TreeSnapshot, TreeViewer};
use crate::tree::{NodeId, TreeBundle};
use crate::ui;

pub struct FigTreeGui {
    config: AppConfig,
    bundle: Option<TreeBundle>,
    tree_viewer: TreeViewer,
    status: String,
    last_error: Option<String>,
    export_feedback: Option<String>,
    current_layout: crate::tree::layout::TreeLayoutType,
    tree_painter: crate::tree::painter::TreePainter,
    panel_states: PanelStates,
    color_picker_open: bool,
    color_picker_hex_input: String,
    color_picker_mode: ColorValueMode,
    color_picker_color: Color32,
    color_picker_origin: Option<egui::Pos2>,
    color_picker_popup_open: bool,
    color_picker_popup_rect: Option<egui::Rect>,
    filter_text: String,
    filter_mode: FilterMode,
    root_tree_enabled: bool,
    root_method: RootMethod,
    user_root_snapshot: Option<TreeSnapshot>,
    order_nodes_enabled: bool,
    node_ordering: NodeOrdering,
    transform_branches_enabled: bool,
    branch_transform: BranchTransform,
}

#[derive(Default)]
pub struct PanelStates {
    layout_expanded: bool,
    appearance_expanded: bool,
    node_labels_expanded: bool,
    tip_labels_expanded: bool,
    branch_labels_expanded: bool,
    scale_bar_expanded: bool,
    trees_expanded: bool,
    transform_expanded: bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ColorValueMode {
    Hex,
    Rgb,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum RootMethod {
    UserSelection,
    Midpoint,
}

impl RootMethod {
    fn label(self) -> &'static str {
        match self {
            Self::UserSelection => "User Selection",
            Self::Midpoint => "Midpoint",
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FilterMode {
    Contains,
    StartsWith,
    EndsWith,
    Matches,
    Regex,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum NodeOrdering {
    Increasing,
    Decreasing,
}

impl NodeOrdering {
    fn label(self) -> &'static str {
        match self {
            Self::Increasing => "increasing",
            Self::Decreasing => "decreasing",
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum BranchTransform {
    Equal,
    Cladogram,
    Proportional,
}

impl BranchTransform {
    fn label(self) -> &'static str {
        match self {
            Self::Equal => "equal",
            Self::Cladogram => "cladogram",
            Self::Proportional => "proportional",
        }
    }
}

impl FilterMode {
    fn label(self) -> &'static str {
        match self {
            Self::Contains => "contains",
            Self::StartsWith => "starts with",
            Self::EndsWith => "ends with",
            Self::Matches => "matches",
            Self::Regex => "regular expression",
        }
    }

    fn to_text_search(self) -> TextSearchType {
        match self {
            Self::Contains => TextSearchType::Contains,
            Self::StartsWith => TextSearchType::StartsWith,
            Self::EndsWith => TextSearchType::EndsWith,
            Self::Matches => TextSearchType::Matches,
            Self::Regex => TextSearchType::Regex,
        }
    }
}

fn parse_hex_color(input: &str) -> Option<Color32> {
    let digits = input.trim().trim_start_matches('#');
    if digits.len() == 6 {
        if let Ok(value) = u32::from_str_radix(digits, 16) {
            return Some(Color32::from_rgb(
                ((value >> 16) & 0xFF) as u8,
                ((value >> 8) & 0xFF) as u8,
                (value & 0xFF) as u8,
            ));
        }
    }
    None
}

impl FigTreeGui {
    pub fn new(_cc: &eframe::CreationContext<'_>, config: AppConfig) -> Self {
        let mut app = Self {
            config,
            bundle: None,
            tree_viewer: TreeViewer::new(),
            status: String::from("Load a tree file to begin."),
            last_error: None,
            export_feedback: None,
            current_layout: crate::tree::layout::TreeLayoutType::Rectangular,
            tree_painter: crate::tree::painter::TreePainter::default(),
            panel_states: PanelStates {
                layout_expanded: true,
                appearance_expanded: true,
                tip_labels_expanded: true,
                node_labels_expanded: true,
                branch_labels_expanded: false,
                scale_bar_expanded: false,
                trees_expanded: true,
                transform_expanded: true,
            },
            color_picker_open: false,
            color_picker_hex_input: String::new(),
            color_picker_mode: ColorValueMode::Hex,
            color_picker_color: Color32::WHITE,
            color_picker_origin: None,
            color_picker_popup_open: false,
            color_picker_popup_rect: None,
            filter_text: String::new(),
            filter_mode: FilterMode::Contains,
            root_tree_enabled: false,
            root_method: RootMethod::UserSelection,
            user_root_snapshot: None,
            order_nodes_enabled: false,
            node_ordering: NodeOrdering::Increasing,
            transform_branches_enabled: false,
            branch_transform: BranchTransform::Proportional,
        };

        if let Some(path) = app.config.tree_path.clone() {
            if let Err(err) = app.load_from_path(path.clone()) {
                error!("Failed to load {}: {}", path.display(), err);
            }
        }

        app.color_picker_color = app.tree_painter.highlight_color;

        app
    }

    fn apply_root_configuration(&mut self, previous_method: Option<RootMethod>) {
        if !self.root_tree_enabled {
            if let Some(snapshot) = self.user_root_snapshot.take() {
                self.tree_viewer.restore_snapshot(snapshot);
            }
            return;
        }

        match self.root_method {
            RootMethod::UserSelection => {
                if matches!(previous_method, Some(RootMethod::Midpoint)) {
                    if let Some(snapshot) = self.user_root_snapshot.take() {
                        self.tree_viewer.restore_snapshot(snapshot);
                    }
                } else {
                    info!("Rooting tree using user selection (pending implementation)");
                }
            }
            RootMethod::Midpoint => {
                if matches!(previous_method, Some(RootMethod::UserSelection))
                    || (previous_method.is_none() && self.user_root_snapshot.is_none())
                {
                    self.user_root_snapshot = self.tree_viewer.snapshot_current_tree();
                }
                self.tree_viewer.midpoint_root();
            }
        }
    }

    fn selected_branch_node(&self) -> Option<NodeId> {
        let tree = self.tree_viewer.current_tree()?;
        let selected_nodes = self.tree_viewer.selected_nodes();
        for node_id in selected_nodes {
            if let Some(node) = tree.nodes.get(*node_id) {
                if let Some(parent) = node.parent {
                    if !selected_nodes.contains(&parent) {
                        return Some(*node_id);
                    }
                }
            }
        }
        None
    }

    fn apply_filter(&mut self) {
        if self.filter_text.trim().is_empty() {
            self.tree_viewer.clear_selection();
            return;
        }

        let mode = self.filter_mode.to_text_search();
        self.tree_viewer.set_selection_mode(SelectionMode::Taxa);
        self.tree_viewer
            .select_taxa(Some("!name"), mode, &self.filter_text, false);
    }

    fn selection_current_color(&self) -> Color32 {
        if let Some(&tip) = self.tree_viewer.selected_tips().iter().next() {
            return self
                .tree_painter
                .tip_label_color_override(tip)
                .unwrap_or(self.tree_painter.label_color);
        }

        if let Some(tree) = self.tree_viewer.current_tree() {
            if let Some(&node_id) = self
                .tree_viewer
                .selected_nodes()
                .iter()
                .find(|id| tree.nodes[**id].parent.is_some())
            {
                return self
                    .tree_painter
                    .branch_color_override(node_id)
                    .unwrap_or(self.tree_painter.branch_stroke.color);
            }
        }

        self.tree_painter.branch_stroke.color
    }

    fn apply_color_to_selection(&mut self, color: Color32) {
        if !self.tree_viewer.selected_tips().is_empty() {
            for tip in self.tree_viewer.selected_tips() {
                self.tree_painter.set_tip_label_color(*tip, color);
            }
            return;
        }

        if self.tree_viewer.selected_nodes().is_empty() {
            return;
        }

        let nodes: Vec<crate::tree::NodeId> =
            self.tree_viewer.selected_nodes().iter().copied().collect();
        let branch_nodes: Vec<crate::tree::NodeId> =
            if let Some(tree) = self.tree_viewer.current_tree() {
                nodes
                    .into_iter()
                    .filter(|id| tree.nodes[*id].parent.is_some())
                    .collect()
            } else {
                Vec::new()
            };

        for node_id in branch_nodes {
            self.tree_painter.set_branch_color(node_id, color);
        }
    }

    fn load_from_path(&mut self, path: PathBuf) -> Result<(), String> {
        match io::load_trees(&path) {
            Ok(bundle) => {
                self.status = format!(
                    "Loaded {:?} file with {} tree(s).",
                    bundle.format,
                    bundle.trees.len()
                );
                info!("Loaded tree file {}", path.display());
                self.last_error = None;
                if let Some(output) = self.config.output.clone() {
                    match ui::export(&bundle, &self.config, &output) {
                        Ok(_) => {
                            self.export_feedback =
                                Some(format!("Export stub invoked for {}", output.display()));
                        }
                        Err(err) => {
                            self.export_feedback = Some(format!("Export failed: {err}"));
                        }
                    }
                } else {
                    self.export_feedback = None;
                }
                self.tree_viewer.set_trees(bundle.trees.clone());
                self.tree_painter.clear_color_overrides();
                self.config.tree_path = Some(path);
                self.bundle = Some(bundle);
                if !self.filter_text.trim().is_empty() {
                    self.apply_filter();
                }
                Ok(())
            }
            Err(err) => {
                let message = err.to_string();
                self.status = String::from("Failed to load tree file.");
                self.last_error = Some(message.clone());
                Err(message)
            }
        }
    }

    fn open_file_dialog(&mut self) {
        if let Some(path) = FileDialog::new()
            .add_filter(
                "Tree files",
                &["tree", "tre", "trees", "nexus", "nex", "newick", "nwk"],
            )
            .pick_file()
        {
            if let Err(err) = self.load_from_path(path) {
                self.last_error = Some(err);
            }
        }
    }

    fn draw_tree_canvas(&mut self, ui: &mut egui::Ui) {
        if let Some(mut tree) = self.tree_viewer.current_tree().cloned() {
            // Apply branch transformation if enabled
            if self.transform_branches_enabled {
                match self.branch_transform {
                    BranchTransform::Equal => tree.apply_equal_transform(),
                    BranchTransform::Cladogram => tree.apply_cladogram_transform(),
                    BranchTransform::Proportional => tree.apply_proportional_transform(),
                }
            }

            if let Some(layout) =
                crate::tree::layout::TreeLayout::from_tree(&tree, self.current_layout)
                    .map(|layout| layout.with_tip_labels(
                        &tree,
                        self.tree_painter.show_tip_labels,
                        self.tree_painter.tip_label_font_size,
                        self.tree_painter.tip_label_font_family.into_font_family(),
                    ))
            {
                // Use 90% of available height for the tree, with minimum of 400px
                let raw_height = ui.available_height();
                // Only apply expansion for Rectangular and Slanted layouts
                let expansion = if matches!(
                    self.current_layout,
                    crate::tree::layout::TreeLayoutType::Rectangular
                        | crate::tree::layout::TreeLayoutType::Slanted
                ) {
                    self.tree_viewer.vertical_expansion()
                } else {
                    1.0
                };
                let base_height = if raw_height.is_finite() {
                    (raw_height * 0.9).max(400.0).min(1200.0)
                } else {
                    600.0
                };
                let desired_height = (base_height * expansion).max(400.0);

                // Use full available width minus some padding
                let raw_width = ui.available_width();
                let available_width = if raw_width.is_finite() {
                    (raw_width - 20.0).max(400.0)
                } else {
                    800.0
                };

                // Only apply zoom for Rectangular and Slanted layouts
                let zoom = if matches!(
                    self.current_layout,
                    crate::tree::layout::TreeLayoutType::Rectangular
                        | crate::tree::layout::TreeLayoutType::Slanted
                ) {
                    self.tree_viewer.zoom.max(1.0)
                } else {
                    1.0
                };
                let canvas_width = available_width * zoom;
                let canvas_height = desired_height * zoom;

                let (response, painter) = ui.allocate_painter(
                    egui::vec2(canvas_width, canvas_height),
                    egui::Sense::click(),
                );
                let rect = response.rect;

                // Dynamic margins based on tree size
                let margin_x = (canvas_width * 0.05).max(20.0).min(60.0);
                let margin_y = (canvas_height * 0.05).max(20.0).min(40.0);
                let margin = egui::Vec2::new(margin_x, margin_y);

                let inner = {
                    let candidate = rect.shrink2(margin);
                    if candidate.is_positive() {
                        candidate
                    } else {
                        rect
                    }
                };
                let scale_x = if layout.width <= f32::EPSILON {
                    inner.width().max(1.0)
                } else {
                    inner.width().max(1.0) / layout.width
                };
                let scale_y = if layout.height <= f32::EPSILON {
                    inner.height().max(1.0)
                } else {
                    inner.height().max(1.0) / layout.height
                };
                let to_screen = |pos: (f32, f32)| -> egui::Pos2 {
                    match self.current_layout {
                        crate::tree::layout::TreeLayoutType::Circular | crate::tree::layout::TreeLayoutType::Radial | crate::tree::layout::TreeLayoutType::Daylight => {
                            // 对于圆形、径向和Daylight布局，使用统一的缩放因子保持形状
                            let available_radius = inner.width().min(inner.height()) * 0.45;
                            let layout_radius = (layout.width.max(layout.height) * 0.5).max(1e-6);
                            let scale = available_radius / layout_radius;

                            // 计算画布中心
                            let center_x = inner.left() + inner.width() * 0.5;
                            let center_y = inner.top() + inner.height() * 0.5;

                            // 将布局坐标转换为相对于画布中心的坐标
                            let x = center_x + (pos.0 - layout.width * 0.5) * scale;
                            let y = center_y + (pos.1 - layout.height * 0.5) * scale;

                            egui::pos2(x, y)
                        }
                        _ => {
                            // 其他布局使用标准坐标转换
                            let x = inner.left() + pos.0 * scale_x;
                            let y = inner.top() + pos.1 * scale_y;
                            egui::pos2(x, y)
                        }
                    }
                };

                fn distance_to_segment(point: egui::Pos2, seg: (egui::Pos2, egui::Pos2)) -> f32 {
                    let ap = point - seg.0;
                    let ab = seg.1 - seg.0;
                    let len_sq = ab.length_sq();
                    if len_sq <= f32::EPSILON {
                        return ap.length();
                    }
                    let t = (ap.dot(ab) / len_sq).clamp(0.0, 1.0);
                    let closest = seg.0 + ab * t;
                    (point - closest).length()
                }


                let selected_nodes = self.tree_viewer.selected_nodes().clone();
                let selected_tips = self.tree_viewer.selected_tips().clone();

                let tip_label_hits = self.tree_painter.paint_tree(
                    &painter,
                    &tree,
                    &layout,
                    &selected_nodes,
                    &selected_tips,
                    rect,
                    margin,
                );

                // Handle click interactions after painting
                if response.clicked() {
                    if let Some(pointer_pos) = response.interact_pointer_pos() {
                        use std::cmp::Ordering;

                        let mut handled = false;
                        let mode = self.tree_viewer.selection_mode();

                        // Try tip label clicks first
                        if matches!(mode, SelectionMode::Taxa | SelectionMode::Tips) {
                            if let Some(hit) = tip_label_hits
                                .iter()
                                .find(|hit| hit.contains(pointer_pos, 2.0))
                            {
                                self.tree_viewer.select_tip_label(hit.node_id);
                                handled = true;
                            }
                        }

                        // Try tip node clicks if label click didn't work
                        if !handled && matches!(mode, SelectionMode::Taxa | SelectionMode::Tips) {
                            let tip_hit_radius = self.tree_painter.node_radius + 6.0;
                            if let Some(closest_tip) = tree.external_nodes()
                                .iter()
                                .filter_map(|node| {
                                    let node_pos = to_screen(layout.positions[node.id]);
                                    let distance = (pointer_pos - node_pos).length();
                                    if distance <= tip_hit_radius {
                                        Some((node.id, distance))
                                    } else {
                                        None
                                    }
                                })
                                .min_by(|a, b| {
                                    a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal)
                                })
                            {
                                self.tree_viewer.select_tip_label(closest_tip.0);
                                handled = true;
                            }
                        }

                        // Try branch clicks if no tip was clicked
                        if !handled {
                            let branch_hit_radius = 8.0_f32;
                            let mut best_branch = None;
                            let mut best_distance = f32::INFINITY;

                            // Use continuous_branches for all layout types if available
                            if !layout.continuous_branches.is_empty() {
                                for branch in &layout.continuous_branches {
                                    if branch.points.len() >= 2 {
                                        // Check distance to multi-segment line
                                        for i in 0..branch.points.len() - 1 {
                                            let p1 = to_screen(branch.points[i]);
                                            let p2 = to_screen(branch.points[i + 1]);
                                            let distance = distance_to_segment(pointer_pos, (p1, p2));
                                            if distance <= branch_hit_radius && distance < best_distance {
                                                best_branch = Some(branch.child);
                                                best_distance = distance;
                                            }
                                        }
                                    }
                                }
                            } else {
                                // Fallback to edges if no continuous_branches
                                for (parent, child) in &layout.edges {
                                    let parent_pos = to_screen(layout.positions[*parent]);
                                    let child_pos = to_screen(layout.positions[*child]);

                                    let distance = distance_to_segment(pointer_pos, (parent_pos, child_pos));
                                    if distance <= branch_hit_radius && distance < best_distance {
                                        best_branch = Some(*child);
                                        best_distance = distance;
                                    }
                                }
                            }

                            if let Some(branch_child) = best_branch {
                                match mode {
                                    SelectionMode::Nodes => {
                                        self.tree_viewer.select_branch_node(branch_child);
                                    }
                                    SelectionMode::Clade => {
                                        self.tree_viewer.select_clade_nodes(branch_child);
                                    }
                                    SelectionMode::Taxa | SelectionMode::Tips => {
                                        self.tree_viewer.select_clade_tips(branch_child);
                                    }
                                }
                                handled = true;
                            }
                        }

                        if handled {
                            ui.ctx().request_repaint();
                        } else {
                            self.tree_viewer.clear_selection();
                            ui.ctx().request_repaint();
                        }
                    }
                }
            } else {
                ui.label("Tree structure unavailable for rendering.");
            }
        }
    }
}

impl eframe::App for FigTreeGui {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Minimal top menu with only file operations
        egui::TopBottomPanel::top("figtree_menu")
            .min_height(28.0)
            .max_height(32.0)
            .show(ctx, |ui| {
                ui.columns(3, |columns| {
                    columns[0].horizontal(|ui| {
                        if ui.small_button("📂 Open").clicked() {
                            self.open_file_dialog();
                        }

                        if let Some(path) = self.config.tree_path.clone() {
                            if ui.small_button("🔄 Reload").clicked() {
                                if let Err(err) = self.load_from_path(path) {
                                    self.last_error = Some(err);
                                }
                            }
                        }

                        let has_selection = !self.tree_viewer.selected_tips().is_empty()
                            || !self.tree_viewer.selected_nodes().is_empty();
                        let color_response =
                            ui.add_enabled(has_selection, egui::Button::new("🎨 Color"));
                        if color_response.clicked() {
                            let initial = self.selection_current_color();
                            self.color_picker_color = initial;
                            self.color_picker_hex_input = format!(
                                "#{:02X}{:02X}{:02X}",
                                initial.r(),
                                initial.g(),
                                initial.b()
                            );
                            self.color_picker_mode = ColorValueMode::Hex;
                            self.color_picker_open = true;
                            self.color_picker_origin = Some(color_response.rect.right_top());
                            self.color_picker_popup_open = false;
                            self.color_picker_popup_rect = None;
                        }

                        ui.add_space(6.0);

                        let branch_target = self.selected_branch_node();
                        let reroot_response =
                            ui.add_enabled(branch_target.is_some(), egui::Button::new("⟳ Reroot"));
                        if reroot_response.clicked() {
                            if let Some(target) = branch_target {
                                self.tree_viewer.reroot_at_node(target);
                                self.color_picker_open = false;
                                self.color_picker_popup_open = false;
                                self.color_picker_popup_rect = None;
                                self.color_picker_origin = None;
                                if let Some(tree) = self.tree_viewer.current_tree() {
                                    if let Some(node) = tree.nodes.get(target) {
                                        let label = node
                                            .name
                                            .clone()
                                            .unwrap_or_else(|| format!("Node {}", target));
                                        self.status = format!("Re-rooted tree at {label}.");
                                    }
                                }
                                ctx.request_repaint();
                            }
                        }
                    });

                    columns[1].horizontal(|ui| {
                        let mode = self.tree_viewer.selection_mode();
                        let options = [
                            (SelectionMode::Nodes, "Node"),
                            (SelectionMode::Clade, "Clade"),
                            (SelectionMode::Taxa, "Taxa"),
                        ];
                        let button_width = 70.0;
                        for (idx, (selection, label)) in options.iter().enumerate() {
                            let button = egui::Button::new(*label).selected(mode == *selection);
                            if ui
                                .add_sized(
                                    egui::vec2(button_width, ui.spacing().interact_size.y),
                                    button,
                                )
                                .clicked()
                            {
                                self.tree_viewer.set_selection_mode(*selection);
                                self.tree_viewer.clear_selection();
                            }
                            if idx + 1 != options.len() {
                                ui.add_space(4.0);
                            }
                        }
                    });

                    columns[2].with_layout(
                        egui::Layout::right_to_left(egui::Align::Center),
                        |ui| {
                            let text_response = ui.add(
                                egui::TextEdit::singleline(&mut self.filter_text)
                                    .hint_text("Filter")
                                    .desired_width(160.0),
                            );
                            if text_response.changed() {
                                self.apply_filter();
                            }

                            ui.add_space(6.0);

                            let mut mode = self.filter_mode;
                            let combo_response = egui::ComboBox::from_id_salt("filter_mode_combo")
                                .selected_text(format!("🔍 {}", mode.label()))
                                .show_ui(ui, |ui| {
                                    let mut changed = false;
                                    changed |= ui
                                        .selectable_value(
                                            &mut mode,
                                            FilterMode::Contains,
                                            "contains",
                                        )
                                        .changed();
                                    changed |= ui
                                        .selectable_value(
                                            &mut mode,
                                            FilterMode::StartsWith,
                                            "starts with",
                                        )
                                        .changed();
                                    changed |= ui
                                        .selectable_value(
                                            &mut mode,
                                            FilterMode::EndsWith,
                                            "ends with",
                                        )
                                        .changed();
                                    changed |= ui
                                        .selectable_value(&mut mode, FilterMode::Matches, "matches")
                                        .changed();
                                    changed |= ui
                                        .selectable_value(
                                            &mut mode,
                                            FilterMode::Regex,
                                            "regular expression",
                                        )
                                        .changed();
                                    changed
                                })
                                .inner;

                            let combo_changed = combo_response.unwrap_or(false);

                            if combo_changed && self.filter_mode != mode {
                                self.filter_mode = mode;
                                self.apply_filter();
                            }
                        },
                    );
                });
            });

        if self.color_picker_open {
            let mut open = self.color_picker_open;
            let mut color = self.color_picker_color;
            let mut window = egui::Window::new("")
                .resizable(false)
                .collapsible(false)
                .title_bar(false)
                .default_width(110.0);

            if let Some(anchor) = self.color_picker_origin {
                window = window.default_pos(anchor + egui::vec2(12.0, 0.0));
            }

            let mut request_popup_open = false;
            let mut window_rect: Option<egui::Rect> = None;

            let window_output = window.open(&mut open).show(ctx, |ui| {
                ui.horizontal(|ui| {
                    let toggle_label = match self.color_picker_mode {
                        ColorValueMode::Hex => "Show RGB",
                        ColorValueMode::Rgb => "Show HEX",
                    };
                    if ui.button(toggle_label).clicked() {
                        self.color_picker_mode = match self.color_picker_mode {
                            ColorValueMode::Hex => ColorValueMode::Rgb,
                            ColorValueMode::Rgb => ColorValueMode::Hex,
                        };
                        if matches!(self.color_picker_mode, ColorValueMode::Hex) {
                            self.color_picker_hex_input =
                                format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
                        }
                    }

                    match self.color_picker_mode {
                        ColorValueMode::Hex => {
                            let response = ui.add(
                                egui::TextEdit::singleline(&mut self.color_picker_hex_input)
                                    .desired_width(60.0),
                            );
                            if response.changed() {
                                if let Some(parsed) = parse_hex_color(&self.color_picker_hex_input)
                                {
                                    color = parsed;
                                    self.color_picker_hex_input = format!(
                                        "#{:02X}{:02X}{:02X}",
                                        color.r(),
                                        color.g(),
                                        color.b()
                                    );
                                }
                            }
                        }
                        ColorValueMode::Rgb => {
                            ui.label(format!("RGB: {}, {}, {}", color.r(), color.g(), color.b()));
                        }
                    }
                });

                ui.separator();

                ui.horizontal(|ui| {
                    ui.label("Preview:");
                    let preview = egui::Button::new(" ")
                        .fill(color)
                        .min_size(egui::vec2(28.0, 28.0));
                    if ui.add(preview).clicked() {
                        request_popup_open = true;
                    }
                });

                ui.separator();
                ui.horizontal(|ui| {
                    if ui.button("Apply").clicked() {
                        self.apply_color_to_selection(color);
                        self.color_picker_open = false;
                        ctx.request_repaint();
                        self.color_picker_popup_open = false;
                    }
                    if ui.button("Cancel").clicked() {
                        self.color_picker_open = false;
                        self.color_picker_popup_open = false;
                    }
                });
            });

            if let Some(output) = window_output {
                window_rect = Some(output.response.rect);
            }

            if open {
                self.color_picker_popup_rect = window_rect;
                if request_popup_open {
                    self.color_picker_popup_open = true;
                }
            } else {
                self.color_picker_popup_open = false;
                self.color_picker_popup_rect = None;
            }

            if self.color_picker_popup_open {
                if let Some(parent_rect) = self.color_picker_popup_rect {
                    let mut popup_color = color;
                    let popup_pos = parent_rect.right_top() + egui::vec2(12.0, 0.0);
                    egui::Area::new("color_picker_popup".into())
                        .order(egui::Order::Foreground)
                        .fixed_pos(popup_pos)
                        .show(ctx, |ui| {
                            egui::Frame::popup(ui.style()).show(ui, |ui| {
                                ui.set_width(140.0);
                                let mut popup_changed = false;
                                ui.scope(|ui| {
                                    let mut style = (**ui.style()).clone();
                                    style.spacing.slider_width *= 0.6;
                                    style.spacing.item_spacing *= 0.8;
                                    ui.set_style(style);

                                    if color_picker::color_picker_color32(
                                        ui,
                                        &mut popup_color,
                                        color_picker::Alpha::Opaque,
                                    ) {
                                        popup_changed = true;
                                    }
                                });

                                ui.separator();
                                if ui.button("Close").clicked() {
                                    self.color_picker_popup_open = false;
                                }

                                if popup_changed {
                                    self.color_picker_hex_input = format!(
                                        "#{:02X}{:02X}{:02X}",
                                        popup_color.r(),
                                        popup_color.g(),
                                        popup_color.b()
                                    );
                                }
                            });
                        });

                    if self.color_picker_popup_open
                        && (popup_color.r() != color.r()
                            || popup_color.g() != color.g()
                            || popup_color.b() != color.b())
                    {
                        color = popup_color;
                        self.color_picker_hex_input =
                            format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
                    }
                } else {
                    self.color_picker_popup_open = false;
                }
            }

            self.color_picker_color = color;
            self.color_picker_open = open && self.color_picker_open;
            if self.color_picker_open {
                self.color_picker_hex_input =
                    format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
            } else {
                self.color_picker_origin = None;
                self.color_picker_popup_open = false;
                self.color_picker_popup_rect = None;
            }
        } else {
            self.color_picker_popup_open = false;
            self.color_picker_popup_rect = None;
        }

        // Left sidebar for controls (narrow)
        egui::SidePanel::left("controls_panel")
            .resizable(true)
            .default_width(220.0)
            .min_width(180.0)
            .max_width(280.0)
            .show(ctx, |ui| {
                ui.heading("Controls");

                // Add scroll area for the control panels
                egui::ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .show(ui, |ui| {

                // Layout Panel (expanded by default)
                let layout_response = egui::CollapsingHeader::new("Layout")
                    .default_open(self.panel_states.layout_expanded)
                    .show(ui, |ui| {
                        ui.label("Layout Type:");
                        let old_layout = self.current_layout;
                        egui::ComboBox::from_id_salt("layout_combo")
                            .selected_text(format!("{:?}", self.current_layout))
                            .show_ui(ui, |ui| {
                                ui.selectable_value(
                                    &mut self.current_layout,
                                    crate::tree::layout::TreeLayoutType::Rectangular,
                                    "Rectangular",
                                );
                                ui.selectable_value(
                                    &mut self.current_layout,
                                    crate::tree::layout::TreeLayoutType::Circular,
                                    "Circular",
                                );
                                ui.selectable_value(
                                    &mut self.current_layout,
                                    crate::tree::layout::TreeLayoutType::Radial,
                                    "Radial",
                                );
                                ui.selectable_value(
                                    &mut self.current_layout,
                                    crate::tree::layout::TreeLayoutType::Slanted,
                                    "Slanted",
                                );
                                ui.selectable_value(
                                    &mut self.current_layout,
                                    crate::tree::layout::TreeLayoutType::Daylight,
                                    "Daylight",
                                );
                            });

                        // Auto-disable Align Tip Labels when switching to unsupported layouts
                        if old_layout != self.current_layout && self.tree_painter.align_tip_labels {
                            if !matches!(
                                self.current_layout,
                                crate::tree::layout::TreeLayoutType::Rectangular
                                    | crate::tree::layout::TreeLayoutType::Circular
                                    | crate::tree::layout::TreeLayoutType::Slanted
                            ) {
                                self.tree_painter.align_tip_labels = false;
                            }
                        }

                        // Zoom and Expansion sliders - only show for Rectangular and Slanted
                        if matches!(
                            self.current_layout,
                            crate::tree::layout::TreeLayoutType::Rectangular
                                | crate::tree::layout::TreeLayoutType::Slanted
                        ) {
                            ui.label("Zoom:");
                            ui.add(
                                egui::Slider::new(&mut self.tree_viewer.zoom, 1.0..=10.0)
                                    .show_value(false),
                            );

                            ui.label("Expansion:");
                            ui.add(
                                egui::Slider::new(&mut self.tree_viewer.vertical_expansion, 1.0..=2.0)
                                    .show_value(false),
                            );
                        }

                        // Align Tip Labels checkbox - only show for Rectangular, Circular, Slanted
                        if matches!(
                            self.current_layout,
                            crate::tree::layout::TreeLayoutType::Rectangular
                                | crate::tree::layout::TreeLayoutType::Circular
                                | crate::tree::layout::TreeLayoutType::Slanted
                        ) {
                            ui.separator();
                            let mut align_tip_labels = self.tree_painter.align_tip_labels;
                            if ui
                                .checkbox(&mut align_tip_labels, "Align Tip Labels")
                                .on_hover_text(
                                    "Extend branches with dashed lines to align all tip labels at the same position"
                                )
                                .changed()
                            {
                                self.tree_painter.align_tip_labels = align_tip_labels;
                            }
                        }
                    });
                if layout_response.header_response.clicked() {
                    self.panel_states.layout_expanded = !self.panel_states.layout_expanded;
                }

                // Appearance Panel
                let appearance_response = egui::CollapsingHeader::new("Appearance")
                    .default_open(self.panel_states.appearance_expanded)
                    .show(ui, |ui| {
                        ui.label("Line Width:");
                        ui.add(
                            egui::Slider::new(
                                &mut self.tree_painter.branch_stroke.width,
                                0.5..=5.0,
                            )
                            .show_value(false),
                        );

                        ui.label("Node Size:");
                        ui.add(
                            egui::Slider::new(&mut self.tree_painter.node_radius, 1.0..=10.0)
                                .show_value(false),
                        );
                    });
                if appearance_response.header_response.clicked() {
                    self.panel_states.appearance_expanded = !self.panel_states.appearance_expanded;
                }

                let tip_header_id = ui.make_persistent_id("controls_tip_labels");
                let mut tip_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    tip_header_id,
                    self.panel_states.tip_labels_expanded,
                );

                let tip_header_response = ui.horizontal(|ui| {
                    let toggle = tip_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_tip_labels = self.tree_painter.show_tip_labels;
                    if ui.checkbox(&mut show_tip_labels, "").changed() {
                        self.tree_painter.show_tip_labels = show_tip_labels;
                    }

                    let label_response =
                        ui.add(egui::Label::new("Tip Labels").sense(egui::Sense::click()));
                    if label_response.clicked() {
                        tip_state.toggle(ui);
                    }

                    toggle
                });

                tip_state.show_body_indented(&tip_header_response.response, ui, |ui| {
                    ui.vertical(|ui| {
                        ui.horizontal(|ui| {
                            ui.label("Display:");
                            egui::ComboBox::from_id_salt("tip_labels_display")
                                .selected_text(self.tree_painter.tip_label_display.label())
                                .show_ui(ui, |ui| {
                                    ui.selectable_value(
                                        &mut self.tree_painter.tip_label_display,
                                        TipLabelDisplay::Names,
                                        "Names",
                                    );
                                    ui.selectable_value(
                                        &mut self.tree_painter.tip_label_display,
                                        TipLabelDisplay::BranchLength,
                                        "Branch Length",
                                    );
                                });
                        });

                        ui.horizontal(|ui| {
                            ui.label("Font Size:");
                            ui.add(
                                egui::DragValue::new(&mut self.tree_painter.tip_label_font_size)
                                    .speed(0.25)
                                    .range(6.0..=48.0),
                            );
                        });

                        ui.horizontal(|ui| {
                            ui.label("Font:");
                            ui.menu_button(
                                self.tree_painter.tip_label_font_family.display_name(),
                                |ui| {
                                    for option in [
                                        TipLabelFontFamily::Proportional,
                                        TipLabelFontFamily::Monospace,
                                    ] {
                                        let selected =
                                            self.tree_painter.tip_label_font_family == option;
                                        if ui
                                            .selectable_label(selected, option.display_name())
                                            .clicked()
                                        {
                                            self.tree_painter.tip_label_font_family = option;
                                            ui.close();
                                        }
                                    }
                                },
                            );
                        });

                        let is_numeric = self.tree_painter.tip_label_display.is_numeric();

                        ui.horizontal(|ui| {
                            ui.label("Format:");
                            ui.add_enabled_ui(is_numeric, |ui| {
                                egui::ComboBox::from_id_salt("tip_labels_format")
                                    .selected_text(self.tree_painter.tip_label_format.label())
                                    .show_ui(ui, |ui| {
                                        ui.selectable_value(
                                            &mut self.tree_painter.tip_label_format,
                                            TipLabelNumberFormat::Decimal,
                                            "Decimal",
                                        );
                                        ui.selectable_value(
                                            &mut self.tree_painter.tip_label_format,
                                            TipLabelNumberFormat::Scientific,
                                            "Scientific",
                                        );
                                        ui.selectable_value(
                                            &mut self.tree_painter.tip_label_format,
                                            TipLabelNumberFormat::Percentage,
                                            "Percentage",
                                        );
                                    });
                            });
                        });

                        let decimal_active = is_numeric
                            && matches!(
                                self.tree_painter.tip_label_format,
                                TipLabelNumberFormat::Decimal
                            );

                        ui.horizontal(|ui| {
                            ui.label("Sig. Digits:");
                            ui.add_enabled_ui(decimal_active, |ui| {
                                ui.add(
                                    egui::DragValue::new(
                                        &mut self.tree_painter.tip_label_precision,
                                    )
                                    .speed(0.25)
                                    .range(0.0..=10.0),
                                );
                            });
                        });
                    });
                });
                self.panel_states.tip_labels_expanded = tip_state.is_open();

                let node_header_id = ui.make_persistent_id("controls_node_labels");
                let mut node_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    node_header_id,
                    self.panel_states.node_labels_expanded,
                );

                let header_response = ui.horizontal(|ui| {
                    let toggle = node_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_labels = self.tree_painter.show_node_labels;
                    if ui.checkbox(&mut show_labels, "").changed() {
                        self.tree_painter.show_node_labels = show_labels;
                    }

                    let label_response =
                        ui.add(egui::Label::new("Node Labels").sense(egui::Sense::click()));
                    if label_response.clicked() {
                        node_state.toggle(ui);
                    }

                    toggle
                });

                node_state.show_body_indented(&header_response.response, ui, |_ui| {});
                self.panel_states.node_labels_expanded = node_state.is_open();

                let branch_header_id = ui.make_persistent_id("controls_branch_labels");
                let mut branch_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    branch_header_id,
                    self.panel_states.branch_labels_expanded,
                );

                let branch_header_response = ui.horizontal(|ui| {
                    let toggle = branch_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_branch = self.tree_painter.show_branch_labels;
                    if ui.checkbox(&mut show_branch, "").changed() {
                        self.tree_painter.show_branch_labels = show_branch;
                    }

                    let label_response =
                        ui.add(egui::Label::new("Branch Labels").sense(egui::Sense::click()));
                    if label_response.clicked() {
                        branch_state.toggle(ui);
                    }

                    toggle
                });

                branch_state.show_body_indented(&branch_header_response.response, ui, |_ui| {});
                self.panel_states.branch_labels_expanded = branch_state.is_open();

                let scale_header_id = ui.make_persistent_id("controls_scale_bar");
                let mut scale_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    scale_header_id,
                    self.panel_states.scale_bar_expanded,
                );

                let scale_header_response = ui.horizontal(|ui| {
                    let toggle = scale_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_scale = self.tree_painter.show_scale_bar;
                    if ui.checkbox(&mut show_scale, "").changed() {
                        self.tree_painter.show_scale_bar = show_scale;
                    }

                    let label_response =
                        ui.add(egui::Label::new("Scale Bar").sense(egui::Sense::click()));
                    if label_response.clicked() {
                        scale_state.toggle(ui);
                    }

                    toggle
                });

                scale_state.show_body_indented(&scale_header_response.response, ui, |_ui| {});
                self.panel_states.scale_bar_expanded = scale_state.is_open();

                // Trees Panel
                let trees_response = egui::CollapsingHeader::new("Trees")
                    .default_open(self.panel_states.trees_expanded)
                    .show(ui, |ui| {
                        // Tree navigation
                        if self.tree_viewer.tree_count() > 1 {
                            ui.label("Navigation:");
                            ui.horizontal(|ui| {
                                if ui.button("← Previous").clicked() {
                                    self.tree_viewer.show_previous_tree();
                                }
                                ui.label(format!(
                                    "{} of {}",
                                    self.tree_viewer.current_tree_index() + 1,
                                    self.tree_viewer.tree_count()
                                ));
                                if ui.button("Next →").clicked() {
                                    self.tree_viewer.show_next_tree();
                                }
                            });
                        }

                        // Selection info
                        if self.tree_viewer.has_selection() {
                            ui.label(format!(
                                "Selected: {} nodes, {} tips",
                                self.tree_viewer.selected_nodes().len(),
                                self.tree_viewer.selected_tips().len()
                            ));
                            if ui.button("Clear Selection").clicked() {
                                self.tree_viewer.clear_selection();
                            }
                        }

                        let mut root_enabled = self.root_tree_enabled;
                        if ui.checkbox(&mut root_enabled, "Root tree").changed() {
                            self.root_tree_enabled = root_enabled;
                            self.apply_root_configuration(None);
                        }

                        let mut method = self.root_method;
                        let combo_changed = ui
                            .add_enabled_ui(self.root_tree_enabled, |ui| {
                                let mut changed = false;
                                egui::ComboBox::from_id_salt("root_tree_method")
                                    .selected_text(method.label())
                                    .show_ui(ui, |ui| {
                                        changed |= ui
                                            .selectable_value(
                                                &mut method,
                                                RootMethod::UserSelection,
                                                RootMethod::UserSelection.label(),
                                            )
                                            .changed();
                                        changed |= ui
                                            .selectable_value(
                                                &mut method,
                                                RootMethod::Midpoint,
                                                RootMethod::Midpoint.label(),
                                            )
                                            .changed();
                                    });
                                changed
                            })
                            .inner;

                        if combo_changed && self.root_method != method {
                            let previous_method = self.root_method;
                            self.root_method = method;
                            self.apply_root_configuration(Some(previous_method));
                        }

                        ui.separator();

                        // Order nodes checkbox and combobox
                        let mut order_enabled = self.order_nodes_enabled;
                        if ui.checkbox(&mut order_enabled, "Order nodes").changed() {
                            let previous_enabled = self.order_nodes_enabled;
                            self.order_nodes_enabled = order_enabled;

                            if order_enabled && !previous_enabled {
                                // Apply ordering when enabling
                                let increasing = matches!(self.node_ordering, NodeOrdering::Increasing);
                                self.tree_viewer.apply_node_ordering(increasing);
                            } else if !order_enabled && previous_enabled {
                                // Restore unordered tree when disabling
                                self.tree_viewer.restore_unordered_tree();
                            }
                        }

                        ui.horizontal(|ui| {
                            ui.label("Ordering:");
                            let mut ordering = self.node_ordering;
                            ui.add_enabled_ui(self.order_nodes_enabled, |ui| {
                                egui::ComboBox::from_id_salt("node_ordering")
                                    .selected_text(ordering.label())
                                    .show_ui(ui, |ui| {
                                        ui.selectable_value(
                                            &mut ordering,
                                            NodeOrdering::Increasing,
                                            NodeOrdering::Increasing.label(),
                                        );
                                        ui.selectable_value(
                                            &mut ordering,
                                            NodeOrdering::Decreasing,
                                            NodeOrdering::Decreasing.label(),
                                        );
                                    });
                            });
                            if self.order_nodes_enabled && ordering != self.node_ordering {
                                self.node_ordering = ordering;
                                // Apply ordering when changed
                                let increasing = matches!(ordering, NodeOrdering::Increasing);
                                self.tree_viewer.apply_node_ordering(increasing);
                            }
                        });

                        ui.separator();

                        // Transform branches checkbox and combobox
                        let mut transform_enabled = self.transform_branches_enabled;
                        if ui.checkbox(&mut transform_enabled, "Transform branches").changed() {
                            self.transform_branches_enabled = transform_enabled;
                        }

                        ui.horizontal(|ui| {
                            ui.label("Transform:");
                            let mut transform = self.branch_transform;
                            ui.add_enabled_ui(self.transform_branches_enabled, |ui| {
                                egui::ComboBox::from_id_salt("branch_transform")
                                    .selected_text(transform.label())
                                    .show_ui(ui, |ui| {
                                        ui.selectable_value(
                                            &mut transform,
                                            BranchTransform::Equal,
                                            BranchTransform::Equal.label(),
                                        );
                                        ui.selectable_value(
                                            &mut transform,
                                            BranchTransform::Cladogram,
                                            BranchTransform::Cladogram.label(),
                                        );
                                        ui.selectable_value(
                                            &mut transform,
                                            BranchTransform::Proportional,
                                            BranchTransform::Proportional.label(),
                                        );
                                    });
                            });
                            if transform != self.branch_transform {
                                self.branch_transform = transform;
                            }
                        });
                    });
                if trees_response.header_response.clicked() {
                    self.panel_states.trees_expanded = !self.panel_states.trees_expanded;
                }

                // Transform Panel
                let transform_response = egui::CollapsingHeader::new("Transform")
                    .default_open(self.panel_states.transform_expanded)
                    .show(ui, |ui| {
                        ui.horizontal(|ui| {
                            if ui.button("Midpoint Root").clicked() {
                                self.tree_viewer.midpoint_root();
                            }
                            if ui.button("Reset").clicked() {
                                self.tree_viewer.zoom = 1.0;
                                self.tree_viewer.vertical_expansion = 1.0;
                            }
                        });
                    });
                if transform_response.header_response.clicked() {
                    self.panel_states.transform_expanded = !self.panel_states.transform_expanded;
                }

                ui.separator();
                ui.toggle_value(&mut self.config.fast_mode, "Fast mode");
                    }); // End of ScrollArea
            });

        // Main area for tree display (wide)
        egui::CentralPanel::default().show(ctx, |ui| {
            if let Some(_bundle) = &self.bundle {
                if let Some(tree) = self.tree_viewer.current_tree() {
                    let caption = tree
                        .label
                        .as_ref()
                        .map(|label| label.as_str())
                        .unwrap_or("Selected tree");
                    ui.label(format!("Viewing: {caption} ({} leaves)", tree.leaf_count()));
                    egui::ScrollArea::both()
                        .auto_shrink([false, false])
                        .show(ui, |ui| {
                            self.draw_tree_canvas(ui);
                        });
                }
            }

            if let Some(message) = &self.export_feedback {
                ui.separator();
                ui.label(message);
            }

            if let Some(err) = &self.last_error {
                ui.separator();
                ui.colored_label(egui::Color32::from_rgb(200, 0, 0), format!("Error: {err}"));
            }
        });

        egui::TopBottomPanel::bottom("figtree_status").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.label(format!(
                    "Fast mode: {}",
                    if self.config.fast_mode { "on" } else { "off" }
                ));
                if let Some(path) = &self.config.tree_path {
                    ui.separator();
                    ui.label(format!("Current file: {}", path.display()));
                }
            });
        });
    }
}
