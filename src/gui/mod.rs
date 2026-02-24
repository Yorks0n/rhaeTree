use std::path::PathBuf;
use std::process::Command;
use std::{
    collections::hash_map::DefaultHasher,
    collections::{BTreeMap, HashSet},
    hash::{Hash, Hasher},
};

use eframe::egui::collapsing_header::{paint_default_icon, CollapsingState};
use eframe::egui::{self, color_picker, Color32};
use log::{error, info};
use rfd::FileDialog;

use crate::app::{AppConfig, ExportFormat};
use crate::io;
use crate::tree::painter::{
    BranchLabelDisplay, NodeLabelDisplay, ShapeColorMode, ShapeSizeMode, ShapeType,
    TipLabelDisplay, TipLabelFontFamily, TipLabelNumberFormat,
};
use crate::tree::scene_graph::SceneLayer;
use crate::tree::skia_renderer::SkiaTreeRenderer;
use crate::tree::vello_renderer::VelloTreeRenderer;
use crate::tree::viewer::{SelectionMode, TextSearchType, TreeSnapshot, TreeViewer};
use crate::tree::{NodeId, Tree, TreeBundle, TreeFileFormat};
use crate::ui;

pub struct FigTreeGui {
    config: AppConfig,
    bundle: Option<TreeBundle>,
    tree_viewer: TreeViewer,
    status: String,
    last_error: Option<String>,
    load_warning_dialog: Option<String>,
    export_feedback: Option<String>,
    current_layout: crate::tree::layout::TreeLayoutType,
    tree_painter: crate::tree::painter::TreePainter,
    panel_states: PanelStates,
    color_picker_open: bool,
    color_picker_hex_input: String,
    color_picker_mode: ColorValueMode,
    color_picker_panel: ColorPanelMode,
    color_picker_target: ColorPickerTarget,
    color_picker_color: Color32,
    color_picker_origin: Option<egui::Pos2>,
    color_picker_popup_open: bool,
    color_picker_popup_rect: Option<egui::Rect>,
    highlight_picker_open: bool,
    highlight_picker_hex_input: String,
    highlight_picker_mode: ColorValueMode,
    highlight_picker_color: Color32,
    highlight_picker_origin: Option<egui::Pos2>,
    highlight_picker_popup_open: bool,
    highlight_picker_popup_rect: Option<egui::Rect>,
    node_bar_picker_open: bool,
    node_bar_picker_hex_input: String,
    node_bar_picker_mode: ColorValueMode,
    node_bar_picker_color: Color32,
    node_bar_picker_origin: Option<egui::Pos2>,
    node_bar_picker_popup_open: bool,
    node_bar_picker_popup_rect: Option<egui::Rect>,
    filter_text: String,
    filter_mode: FilterMode,
    pending_filter_auto_scroll: bool,
    root_tree_enabled: bool,
    root_method: RootMethod,
    user_root_snapshot: Option<TreeSnapshot>,
    order_nodes_enabled: bool,
    node_ordering: NodeOrdering,
    transform_branches_enabled: bool,
    branch_transform: BranchTransform,
    pending_export_path: Option<(PathBuf, crate::app::ExportFormat)>,
    rtr_save_path: Option<PathBuf>,
    tree_canvas_rect: Option<egui::Rect>,
    base_tree_texture: Option<egui::TextureHandle>,
    decor_tree_texture: Option<egui::TextureHandle>,
    interaction_tree_texture: Option<egui::TextureHandle>,
    base_render_signature: Option<u64>,
    decor_render_signature: Option<u64>,
    interaction_render_signature: Option<u64>,
    cached_tip_label_hits_local: Vec<crate::tree::painter::TipLabelHit>,
    render_backend: RenderBackend,
    skia_renderer: SkiaTreeRenderer,
    vello_renderer: VelloTreeRenderer,
    pixels_per_point: f32,
    highlighted_clade: Option<NodeId>,
    annotate_dialog_open: bool,
    annotate_node_id: Option<NodeId>,
    annotate_text: String,
    annotate_field: AnnotateField,
    last_saved_annotate_field: AnnotateField,
    recent_colors: Vec<Color32>,
    undo_stack: Vec<EditState>,
    redo_stack: Vec<EditState>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum AnnotateField {
    Names,
    Labels,
}

impl AnnotateField {
    fn label(self) -> &'static str {
        match self {
            Self::Names => "Names",
            Self::Labels => "Labels",
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct PanelStates {
    layout_expanded: bool,
    appearance_expanded: bool,
    node_labels_expanded: bool,
    tip_labels_expanded: bool,
    branch_labels_expanded: bool,
    scale_bar_expanded: bool,
    tip_shapes_expanded: bool,
    node_shapes_expanded: bool,
    node_bars_expanded: bool,
    trees_expanded: bool,
}

#[derive(Clone, Copy)]
enum ControlSection {
    Layout,
    Trees,
    Appearance,
    LabelsTip,
    LabelsNode,
    LabelsBranch,
    ShapesTip,
    ShapesNode,
    NodeBars,
    ScaleBar,
}

impl PanelStates {
    fn expand_only(&mut self, section: ControlSection) {
        self.layout_expanded = false;
        self.trees_expanded = false;
        self.appearance_expanded = false;
        self.tip_labels_expanded = false;
        self.node_labels_expanded = false;
        self.branch_labels_expanded = false;
        self.tip_shapes_expanded = false;
        self.node_shapes_expanded = false;
        self.node_bars_expanded = false;
        self.scale_bar_expanded = false;

        match section {
            ControlSection::Layout => self.layout_expanded = true,
            ControlSection::Trees => self.trees_expanded = true,
            ControlSection::Appearance => self.appearance_expanded = true,
            ControlSection::LabelsTip => self.tip_labels_expanded = true,
            ControlSection::LabelsNode => self.node_labels_expanded = true,
            ControlSection::LabelsBranch => self.branch_labels_expanded = true,
            ControlSection::ShapesTip => self.tip_shapes_expanded = true,
            ControlSection::ShapesNode => self.node_shapes_expanded = true,
            ControlSection::NodeBars => self.node_bars_expanded = true,
            ControlSection::ScaleBar => self.scale_bar_expanded = true,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ColorValueMode {
    Hex,
    Rgb,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ColorPanelMode {
    Custom,
    Library,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ColorPickerTarget {
    Selection,
    TipShapeFixed,
    NodeShapeFixed,
    TipShapeSelection,
    NodeShapeSelection,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum FilterMode {
    Contains,
    StartsWith,
    EndsWith,
    Matches,
    Regex,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum RenderBackend {
    Skia,
    Vello,
}

#[derive(Clone)]
struct EditState {
    tree_viewer: TreeViewer,
    tree_painter: crate::tree::painter::TreePainter,
    current_layout: crate::tree::layout::TreeLayoutType,
    root_tree_enabled: bool,
    root_method: RootMethod,
    user_root_snapshot: Option<TreeSnapshot>,
    order_nodes_enabled: bool,
    node_ordering: NodeOrdering,
    transform_branches_enabled: bool,
    branch_transform: BranchTransform,
}

impl RenderBackend {
    fn label(self) -> &'static str {
        match self {
            Self::Skia => "Skia",
            Self::Vello => "Vello (Experimental)",
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

fn parse_hex_color_alpha(input: &str) -> Option<Color32> {
    let digits = input.trim().trim_start_matches('#');
    if digits.len() == 6 {
        return parse_hex_color(digits);
    }
    if digits.len() == 8 {
        if let Ok(value) = u32::from_str_radix(digits, 16) {
            return Some(Color32::from_rgba_unmultiplied(
                ((value >> 24) & 0xFF) as u8,
                ((value >> 16) & 0xFF) as u8,
                ((value >> 8) & 0xFF) as u8,
                (value & 0xFF) as u8,
            ));
        }
    }
    None
}

fn color_to_hex_rgba(color: Color32) -> String {
    format!(
        "#{:02X}{:02X}{:02X}{:02X}",
        color.r(),
        color.g(),
        color.b(),
        color.a()
    )
}

fn parse_bool(s: &str) -> Option<bool> {
    match s.trim().to_ascii_lowercase().as_str() {
        "true" | "1" | "yes" | "on" => Some(true),
        "false" | "0" | "no" | "off" => Some(false),
        _ => None,
    }
}

fn apply_bool_setting(settings: &BTreeMap<String, String>, key: &str, target: &mut bool) {
    if let Some(v) = settings.get(key).and_then(|s| parse_bool(s)) {
        *target = v;
    }
}

fn apply_color_setting(settings: &BTreeMap<String, String>, key: &str, target: &mut Color32) {
    if let Some(v) = settings.get(key).and_then(|s| parse_hex_color_alpha(s)) {
        *target = v;
    }
}

fn layout_type_name(value: crate::tree::layout::TreeLayoutType) -> &'static str {
    match value {
        crate::tree::layout::TreeLayoutType::Rectangular => "rectangular",
        crate::tree::layout::TreeLayoutType::Circular => "circular",
        crate::tree::layout::TreeLayoutType::Radial => "radial",
        crate::tree::layout::TreeLayoutType::Slanted => "slanted",
        crate::tree::layout::TreeLayoutType::Cladogram => "cladogram",
        crate::tree::layout::TreeLayoutType::Phylogram => "phylogram",
        crate::tree::layout::TreeLayoutType::Daylight => "daylight",
    }
}

fn parse_layout_type(s: &str) -> Option<crate::tree::layout::TreeLayoutType> {
    match s.trim().to_ascii_lowercase().as_str() {
        "rectangular" => Some(crate::tree::layout::TreeLayoutType::Rectangular),
        "circular" => Some(crate::tree::layout::TreeLayoutType::Circular),
        "radial" => Some(crate::tree::layout::TreeLayoutType::Radial),
        "slanted" => Some(crate::tree::layout::TreeLayoutType::Slanted),
        "cladogram" => Some(crate::tree::layout::TreeLayoutType::Cladogram),
        "phylogram" => Some(crate::tree::layout::TreeLayoutType::Phylogram),
        "daylight" => Some(crate::tree::layout::TreeLayoutType::Daylight),
        _ => None,
    }
}

fn selection_mode_name(value: SelectionMode) -> &'static str {
    match value {
        SelectionMode::Clade => "clade",
        SelectionMode::Nodes => "nodes",
        SelectionMode::Tips => "tips",
        SelectionMode::Taxa => "taxa",
    }
}

fn parse_selection_mode(s: &str) -> Option<SelectionMode> {
    match s.trim().to_ascii_lowercase().as_str() {
        "clade" => Some(SelectionMode::Clade),
        "nodes" => Some(SelectionMode::Nodes),
        "tips" => Some(SelectionMode::Tips),
        "taxa" => Some(SelectionMode::Taxa),
        _ => None,
    }
}

fn render_backend_name(value: RenderBackend) -> &'static str {
    match value {
        RenderBackend::Skia => "skia",
        RenderBackend::Vello => "vello",
    }
}

fn parse_render_backend(s: &str) -> Option<RenderBackend> {
    match s.trim().to_ascii_lowercase().as_str() {
        "skia" => Some(RenderBackend::Skia),
        "vello" => Some(RenderBackend::Vello),
        _ => None,
    }
}

fn root_method_name(value: RootMethod) -> &'static str {
    match value {
        RootMethod::UserSelection => "user_selection",
        RootMethod::Midpoint => "midpoint",
    }
}

fn parse_root_method(s: &str) -> Option<RootMethod> {
    match s.trim().to_ascii_lowercase().as_str() {
        "user_selection" => Some(RootMethod::UserSelection),
        "midpoint" => Some(RootMethod::Midpoint),
        _ => None,
    }
}

fn node_ordering_name(value: NodeOrdering) -> &'static str {
    match value {
        NodeOrdering::Increasing => "increasing",
        NodeOrdering::Decreasing => "decreasing",
    }
}

fn parse_node_ordering(s: &str) -> Option<NodeOrdering> {
    match s.trim().to_ascii_lowercase().as_str() {
        "increasing" => Some(NodeOrdering::Increasing),
        "decreasing" => Some(NodeOrdering::Decreasing),
        _ => None,
    }
}

fn branch_transform_name(value: BranchTransform) -> &'static str {
    match value {
        BranchTransform::Equal => "equal",
        BranchTransform::Cladogram => "cladogram",
        BranchTransform::Proportional => "proportional",
    }
}

fn parse_branch_transform(s: &str) -> Option<BranchTransform> {
    match s.trim().to_ascii_lowercase().as_str() {
        "equal" => Some(BranchTransform::Equal),
        "cladogram" => Some(BranchTransform::Cladogram),
        "proportional" => Some(BranchTransform::Proportional),
        _ => None,
    }
}

fn shape_type_name(value: ShapeType) -> &'static str {
    match value {
        ShapeType::Circle => "circle",
        ShapeType::Square => "square",
        ShapeType::Diamond => "diamond",
    }
}

fn parse_shape_type(s: &str) -> Option<ShapeType> {
    match s.trim().to_ascii_lowercase().as_str() {
        "circle" => Some(ShapeType::Circle),
        "square" => Some(ShapeType::Square),
        "diamond" => Some(ShapeType::Diamond),
        _ => None,
    }
}

fn shape_size_mode_name(value: ShapeSizeMode) -> &'static str {
    match value {
        ShapeSizeMode::Fixed => "fixed",
        ShapeSizeMode::Attribute => "attribute",
    }
}

fn parse_shape_size_mode(s: &str) -> Option<ShapeSizeMode> {
    match s.trim().to_ascii_lowercase().as_str() {
        "fixed" => Some(ShapeSizeMode::Fixed),
        "attribute" => Some(ShapeSizeMode::Attribute),
        _ => None,
    }
}

fn shape_color_mode_name(value: ShapeColorMode) -> &'static str {
    match value {
        ShapeColorMode::UserSelection => "user_selection",
        ShapeColorMode::Fixed => "fixed",
    }
}

fn parse_shape_color_mode(s: &str) -> Option<ShapeColorMode> {
    match s.trim().to_ascii_lowercase().as_str() {
        "user_selection" => Some(ShapeColorMode::UserSelection),
        "fixed" => Some(ShapeColorMode::Fixed),
        _ => None,
    }
}

fn tip_label_display_name(value: TipLabelDisplay) -> &'static str {
    match value {
        TipLabelDisplay::Names => "names",
        TipLabelDisplay::Labels => "labels",
        TipLabelDisplay::BranchLength => "branch_length",
    }
}

fn node_label_display_name(value: NodeLabelDisplay) -> &'static str {
    match value {
        NodeLabelDisplay::Names => "names",
        NodeLabelDisplay::Labels => "labels",
        NodeLabelDisplay::BranchLength => "branch_length",
        NodeLabelDisplay::NodeHeight => "node_height",
        NodeLabelDisplay::Attribute => "attribute",
    }
}

fn parse_node_label_display(s: &str) -> Option<NodeLabelDisplay> {
    match s.trim().to_ascii_lowercase().as_str() {
        "names" => Some(NodeLabelDisplay::Names),
        "labels" => Some(NodeLabelDisplay::Labels),
        "branch_length" => Some(NodeLabelDisplay::BranchLength),
        "node_height" => Some(NodeLabelDisplay::NodeHeight),
        "attribute" => Some(NodeLabelDisplay::Attribute),
        _ => None,
    }
}

fn parse_tip_label_display(s: &str) -> Option<TipLabelDisplay> {
    match s.trim().to_ascii_lowercase().as_str() {
        "names" => Some(TipLabelDisplay::Names),
        "labels" => Some(TipLabelDisplay::Labels),
        "branch_length" => Some(TipLabelDisplay::BranchLength),
        _ => None,
    }
}

fn branch_label_display_name(value: BranchLabelDisplay) -> &'static str {
    match value {
        BranchLabelDisplay::Names => "names",
        BranchLabelDisplay::BranchLength => "branch_length",
    }
}

fn parse_branch_label_display(s: &str) -> Option<BranchLabelDisplay> {
    match s.trim().to_ascii_lowercase().as_str() {
        "names" => Some(BranchLabelDisplay::Names),
        "branch_length" => Some(BranchLabelDisplay::BranchLength),
        _ => None,
    }
}

fn tip_label_font_name(value: TipLabelFontFamily) -> &'static str {
    match value {
        TipLabelFontFamily::Proportional => "proportional",
        TipLabelFontFamily::Monospace => "monospace",
    }
}

fn parse_tip_label_font_family(s: &str) -> Option<TipLabelFontFamily> {
    match s.trim().to_ascii_lowercase().as_str() {
        "proportional" => Some(TipLabelFontFamily::Proportional),
        "monospace" => Some(TipLabelFontFamily::Monospace),
        _ => None,
    }
}

fn tip_label_number_format_name(value: TipLabelNumberFormat) -> &'static str {
    match value {
        TipLabelNumberFormat::Decimal => "decimal",
        TipLabelNumberFormat::Scientific => "scientific",
        TipLabelNumberFormat::Percentage => "percentage",
    }
}

fn parse_tip_label_number_format(s: &str) -> Option<TipLabelNumberFormat> {
    match s.trim().to_ascii_lowercase().as_str() {
        "decimal" => Some(TipLabelNumberFormat::Decimal),
        "scientific" => Some(TipLabelNumberFormat::Scientific),
        "percentage" => Some(TipLabelNumberFormat::Percentage),
        _ => None,
    }
}

impl FigTreeGui {
    const MAX_RENDER_TEXTURE_EDGE: f32 = 8192.0;
    const MAX_RENDER_PIXELS: f32 = 16_000_000.0;
    const MAX_UNDO_STEPS: usize = 5;

    pub fn new(_cc: &eframe::CreationContext<'_>, config: AppConfig) -> Self {
        let mut app = Self {
            config,
            bundle: None,
            tree_viewer: TreeViewer::new(),
            status: String::from("Load a tree file to begin."),
            last_error: None,
            load_warning_dialog: None,
            export_feedback: None,
            current_layout: crate::tree::layout::TreeLayoutType::Rectangular,
            tree_painter: crate::tree::painter::TreePainter::default(),
            panel_states: PanelStates {
                layout_expanded: true,
                appearance_expanded: false,
                tip_labels_expanded: false,
                node_labels_expanded: false,
                branch_labels_expanded: false,
                scale_bar_expanded: false,
                tip_shapes_expanded: false,
                node_shapes_expanded: false,
                node_bars_expanded: false,
                trees_expanded: false,
            },
            color_picker_open: false,
            color_picker_hex_input: String::new(),
            color_picker_mode: ColorValueMode::Hex,
            color_picker_panel: ColorPanelMode::Custom,
            color_picker_target: ColorPickerTarget::Selection,
            color_picker_color: Color32::WHITE,
            color_picker_origin: None,
            color_picker_popup_open: false,
            color_picker_popup_rect: None,
            highlight_picker_open: false,
            highlight_picker_hex_input: String::new(),
            highlight_picker_mode: ColorValueMode::Hex,
            highlight_picker_color: Color32::from_rgb(255, 255, 180),
            highlight_picker_origin: None,
            highlight_picker_popup_open: false,
            highlight_picker_popup_rect: None,
            node_bar_picker_open: false,
            node_bar_picker_hex_input: String::new(),
            node_bar_picker_mode: ColorValueMode::Hex,
            node_bar_picker_color: Color32::from_rgba_unmultiplied(96, 186, 255, 100),
            node_bar_picker_origin: None,
            node_bar_picker_popup_open: false,
            node_bar_picker_popup_rect: None,
            filter_text: String::new(),
            filter_mode: FilterMode::Contains,
            pending_filter_auto_scroll: false,
            root_tree_enabled: false,
            root_method: RootMethod::UserSelection,
            user_root_snapshot: None,
            order_nodes_enabled: false,
            node_ordering: NodeOrdering::Increasing,
            transform_branches_enabled: false,
            branch_transform: BranchTransform::Proportional,
            pending_export_path: None,
            rtr_save_path: None,
            tree_canvas_rect: None,
            base_tree_texture: None,
            decor_tree_texture: None,
            interaction_tree_texture: None,
            base_render_signature: None,
            decor_render_signature: None,
            interaction_render_signature: None,
            cached_tip_label_hits_local: Vec::new(),
            render_backend: RenderBackend::Vello,
            skia_renderer: SkiaTreeRenderer::new(),
            vello_renderer: VelloTreeRenderer::new(),
            pixels_per_point: 1.0,
            highlighted_clade: None,
            annotate_dialog_open: false,
            annotate_node_id: None,
            annotate_text: String::new(),
            annotate_field: AnnotateField::Labels,
            last_saved_annotate_field: AnnotateField::Labels,
            recent_colors: Vec::new(),
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
        };

        if let Some(path) = app.config.tree_path.clone() {
            if let Err(err) = app.load_from_path(path.clone()) {
                error!("Failed to load {}: {}", path.display(), err);
            }
        }
        #[cfg(target_os = "macos")]
        if app.config.tree_path.is_none() {
            let pending_paths = crate::macos_file_open::take_pending_paths();
            app.open_external_paths(pending_paths);
        }

        app.color_picker_color = app.tree_painter.highlight_color;
        app.node_bar_picker_color = app.tree_painter.node_bar_color();

        app
    }

    fn open_external_paths<I>(&mut self, paths: I)
    where
        I: IntoIterator<Item = PathBuf>,
    {
        for path in paths {
            if let Err(err) = self.load_from_path(path.clone()) {
                error!("Failed to load {}: {}", path.display(), err);
                self.last_error = Some(err);
                continue;
            }
            break;
        }
    }

    fn consume_system_open_requests(&mut self, ctx: &egui::Context) {
        let dropped_paths: Vec<PathBuf> = ctx.input(|i| {
            i.raw
                .dropped_files
                .iter()
                .filter_map(|file| file.path.clone())
                .collect()
        });
        if !dropped_paths.is_empty() {
            self.open_external_paths(dropped_paths);
        }

        #[cfg(target_os = "macos")]
        {
            let pending_paths = crate::macos_file_open::take_pending_paths();
            if !pending_paths.is_empty() {
                self.open_external_paths(pending_paths);
            }
        }
    }

    fn capture_edit_state(&self) -> EditState {
        EditState {
            tree_viewer: self.tree_viewer.clone(),
            tree_painter: self.tree_painter.clone(),
            current_layout: self.current_layout,
            root_tree_enabled: self.root_tree_enabled,
            root_method: self.root_method,
            user_root_snapshot: self.user_root_snapshot.clone(),
            order_nodes_enabled: self.order_nodes_enabled,
            node_ordering: self.node_ordering,
            transform_branches_enabled: self.transform_branches_enabled,
            branch_transform: self.branch_transform,
        }
    }

    fn restore_edit_state(&mut self, state: EditState) {
        self.tree_viewer = state.tree_viewer;
        self.tree_painter = state.tree_painter;
        self.current_layout = state.current_layout;
        self.root_tree_enabled = state.root_tree_enabled;
        self.root_method = state.root_method;
        self.user_root_snapshot = state.user_root_snapshot;
        self.order_nodes_enabled = state.order_nodes_enabled;
        self.node_ordering = state.node_ordering;
        self.transform_branches_enabled = state.transform_branches_enabled;
        self.branch_transform = state.branch_transform;

        self.invalidate_layer_caches();
    }

    fn invalidate_layer_caches(&mut self) {
        self.base_render_signature = None;
        self.decor_render_signature = None;
        self.interaction_render_signature = None;
        self.base_tree_texture = None;
        self.decor_tree_texture = None;
        self.interaction_tree_texture = None;
        self.cached_tip_label_hits_local.clear();
    }

    fn record_undo_step(&mut self) {
        self.undo_stack.push(self.capture_edit_state());
        if self.undo_stack.len() > Self::MAX_UNDO_STEPS {
            self.undo_stack.remove(0);
        }
        self.redo_stack.clear();
    }

    fn undo_last_edit(&mut self) {
        if let Some(previous) = self.undo_stack.pop() {
            self.redo_stack.push(self.capture_edit_state());
            self.restore_edit_state(previous);
            self.status = "Undo applied.".to_string();
        }
    }

    fn redo_last_edit(&mut self) {
        if let Some(next) = self.redo_stack.pop() {
            self.undo_stack.push(self.capture_edit_state());
            if self.undo_stack.len() > Self::MAX_UNDO_STEPS {
                self.undo_stack.remove(0);
            }
            self.restore_edit_state(next);
            self.status = "Redo applied.".to_string();
        }
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
            self.pending_filter_auto_scroll = false;
            return;
        }

        let mode = self.filter_mode.to_text_search();
        self.tree_viewer.set_selection_mode(SelectionMode::Taxa);
        self.tree_viewer
            .select_taxa(Some("!name"), mode, &self.filter_text, false);
        self.pending_filter_auto_scroll = true;
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
            self.record_undo_step();
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

        if branch_nodes.is_empty() {
            return;
        }
        self.record_undo_step();
        for node_id in branch_nodes {
            self.tree_painter.set_branch_color(node_id, color);
        }
    }

    fn record_recent_color(&mut self, color: Color32) {
        self.recent_colors.retain(|c| *c != color);
        self.recent_colors.insert(0, color);
        if self.recent_colors.len() > 12 {
            self.recent_colors.truncate(12);
        }
    }

    fn tree_used_colors(&self) -> Vec<Color32> {
        let mut colors = Vec::new();
        colors.push(self.tree_painter.branch_stroke.color);
        colors.push(self.tree_painter.branch_highlight_stroke.color);
        colors.push(self.tree_painter.leaf_color);
        colors.push(self.tree_painter.internal_node_color);
        colors.push(self.tree_painter.selected_color);
        colors.push(self.tree_painter.label_color);
        colors.push(self.tree_painter.branch_label_color);
        colors.push(self.tree_painter.highlight_color);
        colors.push(self.tree_painter.tip_selection_color);
        colors.push(self.tree_painter.tip_shape_fixed_color);
        colors.push(self.tree_painter.node_shape_fixed_color);
        colors.push(self.tree_painter.node_bar_color());
        colors.extend(self.tree_painter.branch_color_overrides().values().copied());
        colors.extend(
            self.tree_painter
                .tip_label_color_overrides()
                .values()
                .copied(),
        );
        colors.extend(
            self.tree_painter
                .tip_shape_color_overrides()
                .values()
                .copied(),
        );
        colors.extend(
            self.tree_painter
                .node_shape_color_overrides()
                .values()
                .copied(),
        );
        colors.extend(self.tree_painter.highlighted_clades().values().copied());

        let mut seen = HashSet::new();
        let mut deduped = Vec::new();
        for color in colors {
            let key = (color.r(), color.g(), color.b(), color.a());
            if seen.insert(key) {
                deduped.push(color);
            }
        }
        deduped
    }

    fn picker_palette_colors(&self) -> Vec<Color32> {
        let mut combined = self.recent_colors.clone();
        combined.extend(self.tree_used_colors());

        let mut seen = HashSet::new();
        let mut deduped = Vec::new();
        for color in combined {
            let key = (color.r(), color.g(), color.b(), color.a());
            if seen.insert(key) {
                deduped.push(color);
            }
        }
        deduped.truncate(16);
        deduped
    }

    fn open_color_picker(
        &mut self,
        origin: egui::Pos2,
        initial: Color32,
        target: ColorPickerTarget,
    ) {
        self.color_picker_color = initial;
        self.color_picker_hex_input =
            format!("#{:02X}{:02X}{:02X}", initial.r(), initial.g(), initial.b());
        self.color_picker_mode = ColorValueMode::Hex;
        self.color_picker_panel = ColorPanelMode::Custom;
        self.color_picker_target = target;
        self.color_picker_open = true;
        self.color_picker_origin = Some(origin);
        self.color_picker_popup_open = false;
        self.color_picker_popup_rect = None;
    }

    fn apply_color_picker_target(&mut self, color: Color32) {
        match self.color_picker_target {
            ColorPickerTarget::Selection => self.apply_color_to_selection(color),
            ColorPickerTarget::TipShapeFixed => {
                self.tree_painter.tip_shape_fixed_color = color;
            }
            ColorPickerTarget::NodeShapeFixed => {
                self.tree_painter.node_shape_fixed_color = color;
            }
            ColorPickerTarget::TipShapeSelection => self.apply_tip_shape_color_to_selection(color),
            ColorPickerTarget::NodeShapeSelection => {
                self.apply_node_shape_color_to_selection(color)
            }
        }
    }

    fn tip_shape_selection_color(&self) -> Color32 {
        if let Some(&tip_id) = self.selected_tip_nodes_for_shape_color().first() {
            return self
                .tree_painter
                .tip_shape_color_override(tip_id)
                .unwrap_or(self.tree_painter.leaf_color);
        }
        self.tree_painter.leaf_color
    }

    fn selected_tip_nodes_for_shape_color(&self) -> Vec<NodeId> {
        let mut ids: HashSet<NodeId> = self.tree_viewer.selected_tips().iter().copied().collect();
        if let Some(tree) = self.tree_viewer.current_tree() {
            ids.extend(
                self.tree_viewer
                    .selected_nodes()
                    .iter()
                    .copied()
                    .filter(|id| tree.nodes[*id].is_leaf()),
            );
        }
        let mut tips: Vec<_> = ids.into_iter().collect();
        tips.sort_unstable();
        tips
    }

    fn apply_tip_shape_color_to_selection(&mut self, color: Color32) {
        let selected_tips = self.selected_tip_nodes_for_shape_color();
        if selected_tips.is_empty() {
            return;
        }
        self.record_undo_step();
        for tip_id in selected_tips {
            self.tree_painter.set_tip_shape_color(tip_id, color);
        }
    }

    fn node_shape_selection_color(&self) -> Color32 {
        if let Some(tree) = self.tree_viewer.current_tree() {
            if let Some(&node_id) = self
                .tree_viewer
                .selected_nodes()
                .iter()
                .find(|id| !tree.nodes[**id].is_leaf())
            {
                return self
                    .tree_painter
                    .node_shape_color_override(node_id)
                    .unwrap_or(self.tree_painter.internal_node_color);
            }
        }
        self.tree_painter.internal_node_color
    }

    fn apply_node_shape_color_to_selection(&mut self, color: Color32) {
        let Some(tree) = self.tree_viewer.current_tree() else {
            return;
        };

        let selected_internal_nodes: Vec<NodeId> = self
            .tree_viewer
            .selected_nodes()
            .iter()
            .copied()
            .filter(|id| !tree.nodes[*id].is_leaf())
            .collect();

        if selected_internal_nodes.is_empty() {
            return;
        }
        self.record_undo_step();
        for node_id in selected_internal_nodes {
            self.tree_painter.set_node_shape_color(node_id, color);
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
                self.load_warning_dialog = None;
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
                self.undo_stack.clear();
                self.redo_stack.clear();
                self.config.tree_path = Some(path);
                let rtr_metadata = if bundle.format == TreeFileFormat::Rtr {
                    Some(bundle.metadata.clone())
                } else {
                    None
                };
                self.rtr_save_path = if bundle.format == TreeFileFormat::Rtr {
                    self.config.tree_path.clone()
                } else {
                    None
                };
                self.bundle = Some(bundle);
                if let Some(metadata) = rtr_metadata {
                    self.apply_rtr_settings(&metadata);
                }
                if !self.filter_text.trim().is_empty() {
                    self.apply_filter();
                }
                Ok(())
            }
            Err(err) => {
                let message = err.to_string();
                self.status = String::from("Failed to load tree file.");
                self.last_error = Some(message.clone());
                self.load_warning_dialog = Some(format!(
                    "Failed to parse tree file:\n{}\n\n{}",
                    path.display(),
                    message
                ));
                Err(message)
            }
        }
    }

    fn open_file_dialog(&mut self) {
        if let Some(path) = FileDialog::new()
            .add_filter(
                "Tree files",
                &[
                    "tree", "tre", "trees", "nexus", "nex", "newick", "nwk", "rtr",
                ],
            )
            .pick_file()
        {
            if let Err(err) = self.load_from_path(path) {
                self.last_error = Some(err);
            }
        }
    }

    fn default_export_file_name(&self, extension: &str) -> String {
        let base = self
            .rtr_save_path
            .as_ref()
            .or(self.config.tree_path.as_ref())
            .and_then(|p| p.file_stem().and_then(|s| s.to_str()))
            .filter(|s| !s.is_empty())
            .unwrap_or("tree");
        format!("{base}.{extension}")
    }

    fn export_png_dialog(&mut self, _ctx: &egui::Context) {
        if let Some(path) = FileDialog::new()
            .add_filter("PNG Image", &["png"])
            .set_file_name(&self.default_export_file_name("png"))
            .save_file()
        {
            self.export_raster_format(&path, ExportFormat::Png);
        }
    }

    fn export_jpeg_dialog(&mut self, _ctx: &egui::Context) {
        if let Some(path) = FileDialog::new()
            .add_filter("JPEG Image", &["jpg", "jpeg"])
            .set_file_name(&self.default_export_file_name("jpg"))
            .save_file()
        {
            self.export_raster_format(&path, ExportFormat::Jpeg);
        }
    }

    fn export_svg_dialog(&mut self) {
        if let Some(path) = FileDialog::new()
            .add_filter("SVG Image", &["svg"])
            .set_file_name(&self.default_export_file_name("svg"))
            .save_file()
        {
            // For SVG, we export directly without screenshot
            self.export_vector_format(&path, ExportFormat::Svg);
        }
    }

    fn export_pdf_dialog(&mut self) {
        if let Some(path) = FileDialog::new()
            .add_filter("PDF Document", &["pdf"])
            .set_file_name(&self.default_export_file_name("pdf"))
            .save_file()
        {
            // For PDF, we export directly without screenshot
            self.export_vector_format(&path, ExportFormat::Pdf);
        }
    }

    fn save_rtr_as_dialog(&mut self) {
        let default_name = self
            .config
            .tree_path
            .as_ref()
            .and_then(|p| p.file_stem().and_then(|s| s.to_str()))
            .map(|stem| format!("{stem}.rtr"))
            .unwrap_or_else(|| "tree.rtr".to_string());
        if let Some(path) = FileDialog::new()
            .add_filter("RTR Tree Session", &["rtr"])
            .set_file_name(&default_name)
            .save_file()
        {
            self.save_rtr_to_path(&path);
            self.rtr_save_path = Some(path.clone());
        }
    }

    fn save_rtr(&mut self) {
        if let Some(path) = self.rtr_save_path.clone() {
            self.save_rtr_to_path(&path);
        } else {
            self.save_rtr_as_dialog();
        }
    }

    fn save_rtr_to_path(&mut self, path: &std::path::Path) {
        let trees = self.tree_viewer.trees().to_vec();
        if trees.is_empty() {
            self.last_error = Some("No tree loaded".to_string());
            return;
        }

        let settings = self.collect_rtr_settings();
        match io::save_rtr(path, &trees, &settings) {
            Ok(_) => {
                self.status = format!("Successfully saved RTR to {}", path.display());
                self.last_error = None;
                self.rtr_save_path = Some(path.to_path_buf());
            }
            Err(e) => {
                self.last_error = Some(format!("Failed to save RTR: {}", e));
            }
        }
    }

    fn collect_rtr_settings(&self) -> BTreeMap<String, String> {
        let mut settings = BTreeMap::new();

        settings.insert(
            "layout.type".to_string(),
            layout_type_name(self.current_layout).to_string(),
        );
        settings.insert(
            "viewer.zoom".to_string(),
            format!("{:.6}", self.tree_viewer.zoom()),
        );
        settings.insert(
            "viewer.verticalExpansion".to_string(),
            format!("{:.6}", self.tree_viewer.vertical_expansion()),
        );
        settings.insert(
            "viewer.selectionMode".to_string(),
            selection_mode_name(self.tree_viewer.selection_mode()).to_string(),
        );
        settings.insert(
            "render.backend".to_string(),
            render_backend_name(self.render_backend).to_string(),
        );

        settings.insert(
            "trees.rooting".to_string(),
            self.root_tree_enabled.to_string(),
        );
        settings.insert(
            "trees.rootingType".to_string(),
            root_method_name(self.root_method).to_string(),
        );
        settings.insert(
            "trees.order".to_string(),
            self.order_nodes_enabled.to_string(),
        );
        settings.insert(
            "trees.orderType".to_string(),
            node_ordering_name(self.node_ordering).to_string(),
        );
        settings.insert(
            "trees.transform".to_string(),
            self.transform_branches_enabled.to_string(),
        );
        settings.insert(
            "trees.transformType".to_string(),
            branch_transform_name(self.branch_transform).to_string(),
        );

        settings.insert(
            "painter.branchLineWidth".to_string(),
            format!("{:.6}", self.tree_painter.branch_stroke.width),
        );
        settings.insert(
            "painter.branchColor".to_string(),
            color_to_hex_rgba(self.tree_painter.branch_stroke.color),
        );
        settings.insert(
            "painter.branchHighlightWidth".to_string(),
            format!("{:.6}", self.tree_painter.branch_highlight_stroke.width),
        );
        settings.insert(
            "painter.branchHighlightColor".to_string(),
            color_to_hex_rgba(self.tree_painter.branch_highlight_stroke.color),
        );
        settings.insert(
            "painter.nodeRadius".to_string(),
            format!("{:.6}", self.tree_painter.node_radius),
        );
        settings.insert(
            "painter.leafColor".to_string(),
            color_to_hex_rgba(self.tree_painter.leaf_color),
        );
        settings.insert(
            "painter.internalNodeColor".to_string(),
            color_to_hex_rgba(self.tree_painter.internal_node_color),
        );
        settings.insert(
            "painter.selectedColor".to_string(),
            color_to_hex_rgba(self.tree_painter.selected_color),
        );
        settings.insert(
            "painter.labelColor".to_string(),
            color_to_hex_rgba(self.tree_painter.label_color),
        );
        settings.insert(
            "painter.branchLabelColor".to_string(),
            color_to_hex_rgba(self.tree_painter.branch_label_color),
        );
        settings.insert(
            "painter.backgroundColor".to_string(),
            color_to_hex_rgba(self.tree_painter.background_color),
        );
        settings.insert(
            "painter.canvasColor".to_string(),
            color_to_hex_rgba(self.tree_painter.canvas_color),
        );
        settings.insert(
            "painter.highlightColor".to_string(),
            color_to_hex_rgba(self.tree_painter.highlight_color),
        );
        settings.insert(
            "painter.tipSelectionColor".to_string(),
            color_to_hex_rgba(self.tree_painter.tip_selection_color),
        );

        settings.insert(
            "painter.showTipLabels".to_string(),
            self.tree_painter.show_tip_labels.to_string(),
        );
        settings.insert(
            "painter.showNodeLabels".to_string(),
            self.tree_painter.show_node_labels.to_string(),
        );
        settings.insert(
            "painter.showBranchLabels".to_string(),
            self.tree_painter.show_branch_labels.to_string(),
        );
        settings.insert(
            "painter.showScaleBar".to_string(),
            self.tree_painter.show_scale_bar.to_string(),
        );
        settings.insert(
            "painter.scaleBarRange".to_string(),
            format!("{:.6}", self.tree_painter.scale_bar_range),
        );
        settings.insert(
            "painter.scaleBarFontSize".to_string(),
            format!("{:.6}", self.tree_painter.scale_bar_font_size),
        );
        settings.insert(
            "painter.scaleBarLineWidth".to_string(),
            format!("{:.6}", self.tree_painter.scale_bar_line_width),
        );
        settings.insert(
            "painter.showTipShapes".to_string(),
            self.tree_painter.show_tip_shapes.to_string(),
        );
        settings.insert(
            "painter.showNodeShapes".to_string(),
            self.tree_painter.show_node_shapes.to_string(),
        );
        settings.insert(
            "painter.tipShape.type".to_string(),
            shape_type_name(self.tree_painter.tip_shape).to_string(),
        );
        settings.insert(
            "painter.tipShape.maxSize".to_string(),
            format!("{:.6}", self.tree_painter.tip_shape_max_size),
        );
        settings.insert(
            "painter.tipShape.sizeBy".to_string(),
            shape_size_mode_name(self.tree_painter.tip_shape_size_mode).to_string(),
        );
        settings.insert(
            "painter.tipShape.sizeAttribute".to_string(),
            self.tree_painter
                .tip_shape_size_attribute
                .clone()
                .unwrap_or_else(|| "null".to_string()),
        );
        settings.insert(
            "painter.tipShape.minSize".to_string(),
            format!("{:.6}", self.tree_painter.tip_shape_min_size),
        );
        settings.insert(
            "painter.tipShape.colorBy".to_string(),
            shape_color_mode_name(self.tree_painter.tip_shape_color_mode).to_string(),
        );
        settings.insert(
            "painter.tipShape.fixedColor".to_string(),
            color_to_hex_rgba(self.tree_painter.tip_shape_fixed_color),
        );
        settings.insert(
            "painter.nodeShape.type".to_string(),
            shape_type_name(self.tree_painter.node_shape).to_string(),
        );
        settings.insert(
            "painter.nodeShape.maxSize".to_string(),
            format!("{:.6}", self.tree_painter.node_shape_max_size),
        );
        settings.insert(
            "painter.nodeShape.sizeBy".to_string(),
            shape_size_mode_name(self.tree_painter.node_shape_size_mode).to_string(),
        );
        settings.insert(
            "painter.nodeShape.sizeAttribute".to_string(),
            self.tree_painter
                .node_shape_size_attribute
                .clone()
                .unwrap_or_else(|| "null".to_string()),
        );
        settings.insert(
            "painter.nodeShape.minSize".to_string(),
            format!("{:.6}", self.tree_painter.node_shape_min_size),
        );
        settings.insert(
            "painter.nodeShape.colorBy".to_string(),
            shape_color_mode_name(self.tree_painter.node_shape_color_mode).to_string(),
        );
        settings.insert(
            "painter.nodeShape.fixedColor".to_string(),
            color_to_hex_rgba(self.tree_painter.node_shape_fixed_color),
        );
        settings.insert(
            "painter.showNodeBars".to_string(),
            self.tree_painter.show_node_bars.to_string(),
        );
        settings.insert(
            "painter.nodeBarField".to_string(),
            self.tree_painter
                .node_bar_field()
                .map(|v| v.to_string())
                .unwrap_or_else(|| "null".to_string()),
        );
        settings.insert(
            "painter.nodeBarColor".to_string(),
            color_to_hex_rgba(self.tree_painter.node_bar_color()),
        );
        settings.insert(
            "painter.nodeBarThickness".to_string(),
            format!("{:.6}", self.tree_painter.node_bar_thickness()),
        );
        settings.insert(
            "painter.alignTipLabels".to_string(),
            self.tree_painter.align_tip_labels.to_string(),
        );
        settings.insert(
            "painter.tipLabelDisplay".to_string(),
            tip_label_display_name(self.tree_painter.tip_label_display).to_string(),
        );
        settings.insert(
            "painter.nodeLabelDisplay".to_string(),
            node_label_display_name(self.tree_painter.node_label_display).to_string(),
        );
        settings.insert(
            "painter.branchLabelDisplay".to_string(),
            branch_label_display_name(self.tree_painter.branch_label_display).to_string(),
        );
        settings.insert(
            "painter.nodeLabelAttribute".to_string(),
            self.tree_painter
                .node_label_attribute
                .clone()
                .unwrap_or_else(|| "null".to_string()),
        );
        settings.insert(
            "painter.nodeLabelFontSize".to_string(),
            format!("{:.6}", self.tree_painter.node_label_font_size),
        );
        settings.insert(
            "painter.nodeLabelNumberFormat".to_string(),
            tip_label_number_format_name(self.tree_painter.node_label_format).to_string(),
        );
        settings.insert(
            "painter.nodeLabelPrecision".to_string(),
            self.tree_painter.node_label_precision.to_string(),
        );
        settings.insert(
            "painter.branchLabelFontSize".to_string(),
            format!("{:.6}", self.tree_painter.branch_label_font_size),
        );
        settings.insert(
            "painter.branchLabelNumberFormat".to_string(),
            tip_label_number_format_name(self.tree_painter.branch_label_format).to_string(),
        );
        settings.insert(
            "painter.branchLabelPrecision".to_string(),
            self.tree_painter.branch_label_precision.to_string(),
        );
        settings.insert(
            "painter.tipLabelFontFamily".to_string(),
            tip_label_font_name(self.tree_painter.tip_label_font_family).to_string(),
        );
        settings.insert(
            "painter.tipLabelFontSize".to_string(),
            format!("{:.6}", self.tree_painter.tip_label_font_size),
        );
        settings.insert(
            "painter.tipLabelNumberFormat".to_string(),
            tip_label_number_format_name(self.tree_painter.tip_label_format).to_string(),
        );
        settings.insert(
            "painter.tipLabelPrecision".to_string(),
            self.tree_painter.tip_label_precision.to_string(),
        );

        let mut branch_overrides: Vec<_> =
            self.tree_painter.branch_color_overrides().iter().collect();
        branch_overrides.sort_by_key(|(node_id, _)| **node_id);
        for (node_id, color) in branch_overrides {
            settings.insert(
                format!("override.branch.{}", node_id),
                color_to_hex_rgba(*color),
            );
        }

        let mut tip_overrides: Vec<_> = self
            .tree_painter
            .tip_label_color_overrides()
            .iter()
            .collect();
        tip_overrides.sort_by_key(|(node_id, _)| **node_id);
        for (node_id, color) in tip_overrides {
            settings.insert(
                format!("override.tipLabel.{}", node_id),
                color_to_hex_rgba(*color),
            );
        }

        let mut tip_shape_overrides: Vec<_> = self
            .tree_painter
            .tip_shape_color_overrides()
            .iter()
            .collect();
        tip_shape_overrides.sort_by_key(|(node_id, _)| **node_id);
        for (node_id, color) in tip_shape_overrides {
            settings.insert(
                format!("override.tipShape.{}", node_id),
                color_to_hex_rgba(*color),
            );
        }

        let mut node_shape_overrides: Vec<_> = self
            .tree_painter
            .node_shape_color_overrides()
            .iter()
            .collect();
        node_shape_overrides.sort_by_key(|(node_id, _)| **node_id);
        for (node_id, color) in node_shape_overrides {
            settings.insert(
                format!("override.nodeShape.{}", node_id),
                color_to_hex_rgba(*color),
            );
        }

        let mut highlights: Vec<_> = self.tree_painter.highlighted_clades().iter().collect();
        highlights.sort_by_key(|(node_id, _)| **node_id);
        for (node_id, color) in highlights {
            settings.insert(
                format!("highlight.clade.{}", node_id),
                color_to_hex_rgba(*color),
            );
        }

        settings
    }

    fn apply_rtr_settings(&mut self, settings: &BTreeMap<String, String>) {
        if let Some(v) = settings
            .get("layout.type")
            .and_then(|s| parse_layout_type(s))
        {
            self.current_layout = v;
        }
        if let Some(v) = settings
            .get("viewer.zoom")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_viewer.set_zoom(v.clamp(1.0, 5.0));
        }
        if let Some(v) = settings
            .get("viewer.verticalExpansion")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_viewer.set_vertical_expansion(v.max(0.1));
        }
        if let Some(v) = settings
            .get("viewer.selectionMode")
            .and_then(|s| parse_selection_mode(s))
        {
            self.tree_viewer.set_selection_mode(v);
        }
        if let Some(v) = settings
            .get("render.backend")
            .and_then(|s| parse_render_backend(s))
        {
            self.render_backend = v;
        }

        if let Some(v) = settings.get("trees.rooting").and_then(|s| parse_bool(s)) {
            self.root_tree_enabled = v;
        }
        if let Some(v) = settings
            .get("trees.rootingType")
            .and_then(|s| parse_root_method(s))
        {
            self.root_method = v;
        }
        if let Some(v) = settings.get("trees.order").and_then(|s| parse_bool(s)) {
            self.order_nodes_enabled = v;
        }
        if let Some(v) = settings
            .get("trees.orderType")
            .and_then(|s| parse_node_ordering(s))
        {
            self.node_ordering = v;
        }
        if let Some(v) = settings.get("trees.transform").and_then(|s| parse_bool(s)) {
            self.transform_branches_enabled = v;
        }
        if let Some(v) = settings
            .get("trees.transformType")
            .and_then(|s| parse_branch_transform(s))
        {
            self.branch_transform = v;
        }

        if let Some(v) = settings
            .get("painter.branchLineWidth")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.branch_stroke.width = v.clamp(1.0, 5.0);
        }
        if let Some(v) = settings
            .get("painter.branchColor")
            .and_then(|s| parse_hex_color_alpha(s))
        {
            self.tree_painter.branch_stroke.color = v;
        }
        if let Some(v) = settings
            .get("painter.branchHighlightWidth")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.branch_highlight_stroke.width = v.max(0.1);
        }
        if let Some(v) = settings
            .get("painter.branchHighlightColor")
            .and_then(|s| parse_hex_color_alpha(s))
        {
            self.tree_painter.branch_highlight_stroke.color = v;
        }
        if let Some(v) = settings
            .get("painter.nodeRadius")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.node_radius = v.max(0.5);
        }
        apply_color_setting(
            settings,
            "painter.leafColor",
            &mut self.tree_painter.leaf_color,
        );
        apply_color_setting(
            settings,
            "painter.internalNodeColor",
            &mut self.tree_painter.internal_node_color,
        );
        apply_color_setting(
            settings,
            "painter.selectedColor",
            &mut self.tree_painter.selected_color,
        );
        apply_color_setting(
            settings,
            "painter.labelColor",
            &mut self.tree_painter.label_color,
        );
        apply_color_setting(
            settings,
            "painter.branchLabelColor",
            &mut self.tree_painter.branch_label_color,
        );
        apply_color_setting(
            settings,
            "painter.backgroundColor",
            &mut self.tree_painter.background_color,
        );
        apply_color_setting(
            settings,
            "painter.canvasColor",
            &mut self.tree_painter.canvas_color,
        );
        apply_color_setting(
            settings,
            "painter.highlightColor",
            &mut self.tree_painter.highlight_color,
        );
        apply_color_setting(
            settings,
            "painter.tipSelectionColor",
            &mut self.tree_painter.tip_selection_color,
        );

        apply_bool_setting(
            settings,
            "painter.showTipLabels",
            &mut self.tree_painter.show_tip_labels,
        );
        apply_bool_setting(
            settings,
            "painter.showNodeLabels",
            &mut self.tree_painter.show_node_labels,
        );
        apply_bool_setting(
            settings,
            "painter.showBranchLabels",
            &mut self.tree_painter.show_branch_labels,
        );
        apply_bool_setting(
            settings,
            "painter.showScaleBar",
            &mut self.tree_painter.show_scale_bar,
        );
        if let Some(v) = settings
            .get("painter.scaleBarRange")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.scale_bar_range = v.max(1e-6);
        }
        if let Some(v) = settings
            .get("painter.scaleBarFontSize")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.scale_bar_font_size = v.max(6.0);
        }
        if let Some(v) = settings
            .get("painter.scaleBarLineWidth")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.scale_bar_line_width = v.max(0.5);
        }
        apply_bool_setting(
            settings,
            "painter.showTipShapes",
            &mut self.tree_painter.show_tip_shapes,
        );
        apply_bool_setting(
            settings,
            "painter.showNodeShapes",
            &mut self.tree_painter.show_node_shapes,
        );
        if let Some(v) = settings
            .get("painter.tipShape.type")
            .and_then(|s| parse_shape_type(s))
        {
            self.tree_painter.tip_shape = v;
        }
        if let Some(v) = settings
            .get("painter.tipShape.maxSize")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.tip_shape_max_size = v.max(0.5);
        }
        if let Some(v) = settings
            .get("painter.tipShape.sizeBy")
            .and_then(|s| parse_shape_size_mode(s))
        {
            self.tree_painter.tip_shape_size_mode = v;
        }
        if let Some(v) = settings.get("painter.tipShape.sizeAttribute") {
            self.tree_painter.tip_shape_size_attribute =
                (!v.eq_ignore_ascii_case("null")).then(|| v.to_string());
        }
        if let Some(v) = settings
            .get("painter.tipShape.minSize")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.tip_shape_min_size = v.max(0.5);
        }
        if let Some(v) = settings
            .get("painter.tipShape.colorBy")
            .and_then(|s| parse_shape_color_mode(s))
        {
            self.tree_painter.tip_shape_color_mode = v;
        }
        if let Some(v) = settings
            .get("painter.tipShape.fixedColor")
            .and_then(|s| parse_hex_color_alpha(s))
        {
            self.tree_painter.tip_shape_fixed_color = v;
        }
        if let Some(v) = settings
            .get("painter.nodeShape.type")
            .and_then(|s| parse_shape_type(s))
        {
            self.tree_painter.node_shape = v;
        }
        if let Some(v) = settings
            .get("painter.nodeShape.maxSize")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.node_shape_max_size = v.max(0.5);
        }
        if let Some(v) = settings
            .get("painter.nodeShape.sizeBy")
            .and_then(|s| parse_shape_size_mode(s))
        {
            self.tree_painter.node_shape_size_mode = v;
        }
        if let Some(v) = settings.get("painter.nodeShape.sizeAttribute") {
            self.tree_painter.node_shape_size_attribute =
                (!v.eq_ignore_ascii_case("null")).then(|| v.to_string());
        }
        if let Some(v) = settings
            .get("painter.nodeShape.minSize")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.node_shape_min_size = v.max(0.5);
        }
        if let Some(v) = settings
            .get("painter.nodeShape.colorBy")
            .and_then(|s| parse_shape_color_mode(s))
        {
            self.tree_painter.node_shape_color_mode = v;
        }
        if let Some(v) = settings
            .get("painter.nodeShape.fixedColor")
            .and_then(|s| parse_hex_color_alpha(s))
        {
            self.tree_painter.node_shape_fixed_color = v;
        }
        apply_bool_setting(
            settings,
            "painter.showNodeBars",
            &mut self.tree_painter.show_node_bars,
        );
        apply_bool_setting(
            settings,
            "painter.alignTipLabels",
            &mut self.tree_painter.align_tip_labels,
        );

        if let Some(v) = settings.get("painter.nodeBarField") {
            self.tree_painter
                .set_node_bar_field((!v.eq_ignore_ascii_case("null")).then(|| v.to_string()));
        }
        if let Some(v) = settings
            .get("painter.nodeBarColor")
            .and_then(|s| parse_hex_color_alpha(s))
        {
            self.tree_painter.set_node_bar_color(v);
        }
        if let Some(v) = settings
            .get("painter.nodeBarThickness")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.set_node_bar_thickness(v);
        }
        if let Some(v) = settings
            .get("painter.tipLabelDisplay")
            .and_then(|s| parse_tip_label_display(s))
        {
            self.tree_painter.tip_label_display = v;
        }
        if let Some(v) = settings
            .get("painter.nodeLabelDisplay")
            .and_then(|s| parse_node_label_display(s))
        {
            self.tree_painter.node_label_display = v;
        }
        if let Some(v) = settings
            .get("painter.branchLabelDisplay")
            .and_then(|s| parse_branch_label_display(s))
        {
            self.tree_painter.branch_label_display = v;
        }
        if let Some(v) = settings.get("painter.nodeLabelAttribute") {
            self.tree_painter.node_label_attribute =
                (!v.eq_ignore_ascii_case("null")).then(|| v.to_string());
        }
        if let Some(v) = settings
            .get("painter.nodeLabelFontSize")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.node_label_font_size = v.max(6.0);
        }
        if let Some(v) = settings
            .get("painter.nodeLabelNumberFormat")
            .and_then(|s| parse_tip_label_number_format(s))
        {
            self.tree_painter.node_label_format = v;
        }
        if let Some(v) = settings
            .get("painter.nodeLabelPrecision")
            .and_then(|s| s.parse::<usize>().ok())
        {
            self.tree_painter.node_label_precision = v.min(12);
        }
        if let Some(v) = settings
            .get("painter.branchLabelFontSize")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.branch_label_font_size = v.max(6.0);
        }
        if let Some(v) = settings
            .get("painter.branchLabelNumberFormat")
            .and_then(|s| parse_tip_label_number_format(s))
        {
            self.tree_painter.branch_label_format = v;
        }
        if let Some(v) = settings
            .get("painter.branchLabelPrecision")
            .and_then(|s| s.parse::<usize>().ok())
        {
            self.tree_painter.branch_label_precision = v.min(12);
        }
        if let Some(v) = settings
            .get("painter.tipLabelFontFamily")
            .and_then(|s| parse_tip_label_font_family(s))
        {
            self.tree_painter.tip_label_font_family = v;
        }
        if let Some(v) = settings
            .get("painter.tipLabelFontSize")
            .and_then(|s| s.parse::<f32>().ok())
        {
            self.tree_painter.tip_label_font_size = v.max(6.0);
        }
        if let Some(v) = settings
            .get("painter.tipLabelNumberFormat")
            .and_then(|s| parse_tip_label_number_format(s))
        {
            self.tree_painter.tip_label_format = v;
        }
        if let Some(v) = settings
            .get("painter.tipLabelPrecision")
            .and_then(|s| s.parse::<usize>().ok())
        {
            self.tree_painter.tip_label_precision = v.min(12);
        }

        self.tree_painter.clear_color_overrides();
        self.tree_painter.clear_highlighted_clades();
        for (k, v) in settings {
            if let Some(id) = k
                .strip_prefix("override.branch.")
                .and_then(|s| s.parse::<NodeId>().ok())
            {
                if let Some(color) = parse_hex_color_alpha(v) {
                    self.tree_painter.set_branch_color(id, color);
                }
            } else if let Some(id) = k
                .strip_prefix("override.tipLabel.")
                .and_then(|s| s.parse::<NodeId>().ok())
            {
                if let Some(color) = parse_hex_color_alpha(v) {
                    self.tree_painter.set_tip_label_color(id, color);
                }
            } else if let Some(id) = k
                .strip_prefix("override.tipShape.")
                .and_then(|s| s.parse::<NodeId>().ok())
            {
                if let Some(color) = parse_hex_color_alpha(v) {
                    self.tree_painter.set_tip_shape_color(id, color);
                }
            } else if let Some(id) = k
                .strip_prefix("override.nodeShape.")
                .and_then(|s| s.parse::<NodeId>().ok())
            {
                if let Some(color) = parse_hex_color_alpha(v) {
                    self.tree_painter.set_node_shape_color(id, color);
                }
            } else if let Some(id) = k
                .strip_prefix("highlight.clade.")
                .and_then(|s| s.parse::<NodeId>().ok())
            {
                if let Some(color) = parse_hex_color_alpha(v) {
                    self.tree_painter.add_highlighted_clade(id, color);
                }
            }
        }

        self.invalidate_layer_caches();
    }

    fn export_vector_format(&mut self, path: &std::path::Path, format: ExportFormat) {
        // Get current tree and layout
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
                crate::tree::layout::TreeLayout::from_tree(&tree, self.current_layout).map(
                    |layout| {
                        layout.with_tip_labels(
                            &tree,
                            self.tree_painter.show_tip_labels,
                            self.tree_painter.tip_label_font_size,
                            self.tree_painter.tip_label_font_family.into_font_family(),
                        )
                    },
                )
            {
                // Apply zoom and expansion effects (only for Rectangular and Slanted layouts)
                let expansion = if matches!(
                    self.current_layout,
                    crate::tree::layout::TreeLayoutType::Rectangular
                        | crate::tree::layout::TreeLayoutType::Slanted
                ) {
                    self.tree_viewer.vertical_expansion()
                } else {
                    1.0
                };

                let zoom = if matches!(
                    self.current_layout,
                    crate::tree::layout::TreeLayoutType::Rectangular
                        | crate::tree::layout::TreeLayoutType::Slanted
                ) {
                    self.tree_viewer.zoom.max(1.0)
                } else {
                    1.0
                };

                // Base dimensions for export
                let base_width = 1200.0;
                let base_height = 900.0;

                // Apply zoom and expansion to dimensions
                let width = base_width * zoom;
                let height = base_height * expansion * zoom;

                let result = match format {
                    ExportFormat::Svg => crate::export::export_svg(
                        &tree,
                        &layout,
                        &self.tree_painter,
                        path,
                        width,
                        height,
                    ),
                    ExportFormat::Pdf => crate::export::export_pdf(
                        &tree,
                        &layout,
                        &self.tree_painter,
                        path,
                        width,
                        height,
                    ),
                    _ => Err("Unsupported export format".to_string()),
                };

                match result {
                    Ok(_) => {
                        self.status = format!("Successfully exported to {}", path.display());
                        self.last_error = None;
                    }
                    Err(e) => {
                        self.last_error = Some(format!("Failed to export: {}", e));
                    }
                }
            } else {
                self.last_error = Some("Failed to create tree layout".to_string());
            }
        } else {
            self.last_error = Some("No tree loaded".to_string());
        }
    }

    fn export_raster_format(&mut self, path: &std::path::Path, format: ExportFormat) {
        // Render offscreen at higher resolution to avoid low-quality screenshot exports.
        if let Some(mut tree) = self.tree_viewer.current_tree().cloned() {
            if self.transform_branches_enabled {
                match self.branch_transform {
                    BranchTransform::Equal => tree.apply_equal_transform(),
                    BranchTransform::Cladogram => tree.apply_cladogram_transform(),
                    BranchTransform::Proportional => tree.apply_proportional_transform(),
                }
            }

            if let Some(layout) =
                crate::tree::layout::TreeLayout::from_tree(&tree, self.current_layout).map(
                    |layout| {
                        layout.with_tip_labels(
                            &tree,
                            self.tree_painter.show_tip_labels,
                            self.tree_painter.tip_label_font_size,
                            self.tree_painter.tip_label_font_family.into_font_family(),
                        )
                    },
                )
            {
                let expansion = if matches!(
                    self.current_layout,
                    crate::tree::layout::TreeLayoutType::Rectangular
                        | crate::tree::layout::TreeLayoutType::Slanted
                ) {
                    self.tree_viewer.vertical_expansion()
                } else {
                    1.0
                };

                let zoom = if matches!(
                    self.current_layout,
                    crate::tree::layout::TreeLayoutType::Rectangular
                        | crate::tree::layout::TreeLayoutType::Slanted
                ) {
                    self.tree_viewer.zoom.max(1.0)
                } else {
                    1.0
                };

                let base_width = 1200.0;
                let base_height = 900.0;
                let export_scale = 2.0;
                let width = base_width * zoom * export_scale;
                let height = base_height * expansion * zoom * export_scale;

                // Scale geometry-sensitive painter parameters for high-resolution raster export.
                // This keeps node bars and tip/node shape sizes visually consistent with line/text scaling.
                let mut export_painter = self.tree_painter.clone();
                export_painter.node_bar_thickness *= export_scale;
                export_painter.tip_shape_max_size *= export_scale;
                export_painter.tip_shape_min_size *= export_scale;
                export_painter.node_shape_max_size *= export_scale;
                export_painter.node_shape_min_size *= export_scale;

                let mut scene = crate::export::svg::build_export_scene(
                    &tree,
                    &layout,
                    &export_painter,
                    width,
                    height,
                );
                for primitive in &mut scene.primitives {
                    match primitive {
                        crate::tree::scene_graph::ScenePrimitive::Text { size, .. } => {
                            *size *= export_scale;
                        }
                        crate::tree::scene_graph::ScenePrimitive::StrokeLine { style, .. }
                        | crate::tree::scene_graph::ScenePrimitive::StrokePolyline {
                            style, ..
                        }
                        | crate::tree::scene_graph::ScenePrimitive::StrokeCircularBranch {
                            style,
                            ..
                        } => {
                            style.width *= export_scale;
                            if let Some((dash, gap)) = &mut style.dash {
                                *dash *= export_scale;
                                *gap *= export_scale;
                            }
                        }
                        _ => {}
                    }
                }

                let image = match self.render_backend {
                    RenderBackend::Vello => self
                        .vello_renderer
                        .render_scene_to_image(&scene, 1.0, 1.0)
                        .or_else(|_| self.skia_renderer.render_scene_to_image(&scene, 1.0, 1.0))
                        .map(|out| out.image),
                    RenderBackend::Skia => self
                        .skia_renderer
                        .render_scene_to_image(&scene, 1.0, 1.0)
                        .or_else(|_| self.vello_renderer.render_scene_to_image(&scene, 1.0, 1.0))
                        .map(|out| out.image),
                };

                match image.and_then(|img| self.save_color_image_to_file(&img, path, format)) {
                    Ok(_) => {
                        self.status = format!("Successfully exported to {}", path.display());
                        self.last_error = None;
                    }
                    Err(e) => {
                        self.last_error = Some(format!("Failed to export: {}", e));
                    }
                }
            } else {
                self.last_error = Some("Failed to create tree layout".to_string());
            }
        } else {
            self.last_error = Some("No tree loaded".to_string());
        }
    }

    fn save_color_image_to_file(
        &self,
        image: &egui::ColorImage,
        path: &std::path::Path,
        format: ExportFormat,
    ) -> Result<(), String> {
        use image::{ImageBuffer, RgbaImage};
        use std::fs::File;
        use std::io::BufWriter;

        let rgba_image: RgbaImage = ImageBuffer::from_raw(
            image.width() as u32,
            image.height() as u32,
            image.pixels.iter().flat_map(|c| c.to_array()).collect(),
        )
        .ok_or_else(|| "Failed to create image buffer".to_string())?;

        match format {
            ExportFormat::Png => {
                rgba_image
                    .save_with_format(path, image::ImageFormat::Png)
                    .map_err(|e| format!("Failed to save PNG: {}", e))?;
            }
            ExportFormat::Jpeg => {
                let rgb_image = image::DynamicImage::ImageRgba8(rgba_image).to_rgb8();
                let file = File::create(path).map_err(|e| format!("Failed to create JPEG: {e}"))?;
                let mut writer = BufWriter::new(file);
                let mut encoder =
                    image::codecs::jpeg::JpegEncoder::new_with_quality(&mut writer, 95);
                encoder
                    .encode_image(&image::DynamicImage::ImageRgb8(rgb_image))
                    .map_err(|e| format!("Failed to encode JPEG: {}", e))?;
            }
            _ => return Err(format!("Unsupported export format: {:?}", format)),
        }

        Ok(())
    }

    fn save_screenshot_to_file(
        &self,
        screenshot: &egui::ColorImage,
        path: &std::path::Path,
        format: ExportFormat,
    ) -> Result<(), String> {
        use image::{ImageBuffer, RgbaImage};

        // Convert egui ColorImage to image crate's RgbaImage
        let full_image: RgbaImage = ImageBuffer::from_raw(
            screenshot.width() as u32,
            screenshot.height() as u32,
            screenshot
                .pixels
                .iter()
                .flat_map(|c| c.to_array())
                .collect(),
        )
        .ok_or_else(|| "Failed to create image buffer".to_string())?;

        // Crop to tree canvas area if available
        let rgba_image = if let Some(canvas_rect) = self.tree_canvas_rect {
            // Convert rect coordinates to pixel coordinates using pixels_per_point
            let ppp = self.pixels_per_point;
            let x = ((canvas_rect.min.x * ppp).max(0.0) as u32).min(screenshot.width() as u32);
            let y = ((canvas_rect.min.y * ppp).max(0.0) as u32).min(screenshot.height() as u32);
            let width = ((canvas_rect.width() * ppp) as u32).min(screenshot.width() as u32 - x);
            let height = ((canvas_rect.height() * ppp) as u32).min(screenshot.height() as u32 - y);

            // Crop the image
            image::DynamicImage::ImageRgba8(full_image)
                .crop_imm(x, y, width, height)
                .to_rgba8()
        } else {
            // If no canvas rect, use full screenshot
            full_image
        };

        self.save_color_image_to_file(
            &egui::ColorImage::from_rgba_unmultiplied(
                [rgba_image.width() as usize, rgba_image.height() as usize],
                rgba_image.as_raw(),
            ),
            path,
            format,
        )
    }

    fn build_display_tree_layout(
        &self,
    ) -> Option<(crate::tree::Tree, crate::tree::layout::TreeLayout)> {
        let mut tree = self.tree_viewer.current_tree()?.clone();

        if self.transform_branches_enabled {
            match self.branch_transform {
                BranchTransform::Equal => tree.apply_equal_transform(),
                BranchTransform::Cladogram => tree.apply_cladogram_transform(),
                BranchTransform::Proportional => tree.apply_proportional_transform(),
            }
        }

        let layout = crate::tree::layout::TreeLayout::from_tree(&tree, self.current_layout)?
            .with_tip_labels(
                &tree,
                self.tree_painter.show_tip_labels,
                self.tree_painter.tip_label_font_size,
                self.tree_painter.tip_label_font_family.into_font_family(),
            );

        Some((tree, layout))
    }

    fn hash_color<H: Hasher>(h: &mut H, c: Color32) {
        c.r().hash(h);
        c.g().hash(h);
        c.b().hash(h);
        c.a().hash(h);
    }

    fn hash_node_set<H: Hasher>(h: &mut H, set: &std::collections::HashSet<NodeId>) {
        let mut ids: Vec<_> = set.iter().copied().collect();
        ids.sort_unstable();
        ids.hash(h);
    }

    fn hash_tree<H: Hasher>(h: &mut H, tree: &crate::tree::Tree) {
        tree.id.hash(h);
        tree.root.hash(h);
        tree.nodes.len().hash(h);
        for n in &tree.nodes {
            n.id.hash(h);
            n.parent.hash(h);
            n.length.map(f64::to_bits).hash(h);
            n.name.hash(h);
            n.label.hash(h);
            n.children.hash(h);
        }
    }

    fn hash_branch_overrides<H: Hasher>(
        h: &mut H,
        tree: &crate::tree::Tree,
        painter: &crate::tree::painter::TreePainter,
    ) {
        for node in &tree.nodes {
            node.id.hash(h);
            if let Some(color) = painter.branch_color_override(node.id) {
                1u8.hash(h);
                Self::hash_color(h, color);
            } else {
                0u8.hash(h);
            }
        }
    }

    fn hash_tip_label_overrides<H: Hasher>(
        h: &mut H,
        tree: &crate::tree::Tree,
        painter: &crate::tree::painter::TreePainter,
    ) {
        for node in &tree.nodes {
            node.id.hash(h);
            if let Some(color) = painter.tip_label_color_override(node.id) {
                1u8.hash(h);
                Self::hash_color(h, color);
            } else {
                0u8.hash(h);
            }
        }
    }

    fn hash_tip_shape_overrides<H: Hasher>(
        h: &mut H,
        tree: &crate::tree::Tree,
        painter: &crate::tree::painter::TreePainter,
    ) {
        for node in &tree.nodes {
            node.id.hash(h);
            if let Some(color) = painter.tip_shape_color_override(node.id) {
                1u8.hash(h);
                Self::hash_color(h, color);
            } else {
                0u8.hash(h);
            }
        }
    }

    fn hash_node_shape_overrides<H: Hasher>(
        h: &mut H,
        tree: &crate::tree::Tree,
        painter: &crate::tree::painter::TreePainter,
    ) {
        for node in &tree.nodes {
            node.id.hash(h);
            if let Some(color) = painter.node_shape_color_override(node.id) {
                1u8.hash(h);
                Self::hash_color(h, color);
            } else {
                0u8.hash(h);
            }
        }
    }

    fn hash_clade_highlights<H: Hasher>(h: &mut H, painter: &crate::tree::painter::TreePainter) {
        let mut highlighted: Vec<_> = painter.highlighted_clades().iter().collect();
        highlighted.sort_by_key(|(node_id, _)| **node_id);
        for (node_id, color) in highlighted {
            node_id.hash(h);
            Self::hash_color(h, *color);
        }
    }

    fn hash_shared_render_state<H: Hasher>(
        &self,
        h: &mut H,
        tree: &crate::tree::Tree,
        canvas_width: f32,
        canvas_height: f32,
    ) {
        Self::hash_tree(h, tree);
        self.current_layout.hash(h);
        self.transform_branches_enabled.hash(h);
        self.branch_transform.hash(h);
        self.tree_viewer.current_tree_index().hash(h);
        self.tree_viewer.zoom.to_bits().hash(h);
        self.tree_viewer.vertical_expansion().to_bits().hash(h);
        canvas_width.to_bits().hash(h);
        canvas_height.to_bits().hash(h);
        self.pixels_per_point.to_bits().hash(h);
        self.render_backend.hash(h);
    }

    fn base_render_signature(
        &self,
        tree: &crate::tree::Tree,
        canvas_width: f32,
        canvas_height: f32,
    ) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash_shared_render_state(&mut hasher, tree, canvas_width, canvas_height);
        self.tree_painter
            .branch_stroke
            .width
            .to_bits()
            .hash(&mut hasher);
        Self::hash_color(&mut hasher, self.tree_painter.branch_stroke.color);
        Self::hash_color(&mut hasher, self.tree_painter.background_color);
        Self::hash_color(&mut hasher, self.tree_painter.canvas_color);
        Self::hash_branch_overrides(&mut hasher, tree, &self.tree_painter);
        Self::hash_clade_highlights(&mut hasher, &self.tree_painter);
        hasher.finish()
    }

    fn decor_render_signature(
        &self,
        tree: &crate::tree::Tree,
        canvas_width: f32,
        canvas_height: f32,
    ) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash_shared_render_state(&mut hasher, tree, canvas_width, canvas_height);

        Self::hash_color(&mut hasher, self.tree_painter.label_color);
        Self::hash_color(&mut hasher, self.tree_painter.branch_label_color);
        self.tree_painter.show_tip_labels.hash(&mut hasher);
        self.tree_painter.show_node_labels.hash(&mut hasher);
        self.tree_painter.show_branch_labels.hash(&mut hasher);
        self.tree_painter.tip_label_display.hash(&mut hasher);
        self.tree_painter.node_label_display.hash(&mut hasher);
        self.tree_painter.branch_label_display.hash(&mut hasher);
        self.tree_painter.node_label_attribute.hash(&mut hasher);
        self.tree_painter
            .node_label_font_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .branch_label_font_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter.node_label_format.hash(&mut hasher);
        self.tree_painter.branch_label_format.hash(&mut hasher);
        self.tree_painter.node_label_precision.hash(&mut hasher);
        self.tree_painter.branch_label_precision.hash(&mut hasher);
        self.tree_painter.tip_label_font_family.hash(&mut hasher);
        self.tree_painter
            .tip_label_font_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter.tip_label_format.hash(&mut hasher);
        self.tree_painter.tip_label_precision.hash(&mut hasher);

        self.tree_painter.show_tip_shapes.hash(&mut hasher);
        self.tree_painter.show_node_shapes.hash(&mut hasher);
        self.tree_painter.tip_shape.hash(&mut hasher);
        self.tree_painter.node_shape.hash(&mut hasher);
        self.tree_painter.tip_shape_size_mode.hash(&mut hasher);
        self.tree_painter.node_shape_size_mode.hash(&mut hasher);
        self.tree_painter.tip_shape_size_attribute.hash(&mut hasher);
        self.tree_painter
            .node_shape_size_attribute
            .hash(&mut hasher);
        self.tree_painter
            .tip_shape_max_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .node_shape_max_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .tip_shape_min_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .node_shape_min_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter.tip_shape_color_mode.hash(&mut hasher);
        self.tree_painter.node_shape_color_mode.hash(&mut hasher);
        Self::hash_color(&mut hasher, self.tree_painter.tip_shape_fixed_color);
        Self::hash_color(&mut hasher, self.tree_painter.node_shape_fixed_color);
        Self::hash_color(&mut hasher, self.tree_painter.leaf_color);
        Self::hash_color(&mut hasher, self.tree_painter.internal_node_color);

        self.tree_painter.show_node_bars.hash(&mut hasher);
        self.tree_painter.node_bar_field().hash(&mut hasher);
        Self::hash_color(&mut hasher, self.tree_painter.node_bar_color());
        self.tree_painter
            .node_bar_thickness()
            .to_bits()
            .hash(&mut hasher);

        self.tree_painter.show_scale_bar.hash(&mut hasher);
        self.tree_painter
            .scale_bar_range
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .scale_bar_font_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .scale_bar_line_width
            .to_bits()
            .hash(&mut hasher);

        self.tree_painter.align_tip_labels.hash(&mut hasher);
        Self::hash_tip_label_overrides(&mut hasher, tree, &self.tree_painter);
        Self::hash_tip_shape_overrides(&mut hasher, tree, &self.tree_painter);
        Self::hash_node_shape_overrides(&mut hasher, tree, &self.tree_painter);
        hasher.finish()
    }

    fn interaction_render_signature(
        &self,
        tree: &crate::tree::Tree,
        canvas_width: f32,
        canvas_height: f32,
    ) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash_shared_render_state(&mut hasher, tree, canvas_width, canvas_height);

        self.tree_viewer.selection_mode().hash(&mut hasher);
        Self::hash_node_set(&mut hasher, self.tree_viewer.selected_nodes());
        Self::hash_node_set(&mut hasher, self.tree_viewer.selected_tips());

        self.tree_painter.show_tip_labels.hash(&mut hasher);
        self.tree_painter.tip_label_display.hash(&mut hasher);
        self.tree_painter
            .tip_label_font_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter.tip_label_format.hash(&mut hasher);
        self.tree_painter.tip_label_precision.hash(&mut hasher);
        self.tree_painter.align_tip_labels.hash(&mut hasher);

        self.tree_painter.show_tip_shapes.hash(&mut hasher);
        self.tree_painter.show_node_shapes.hash(&mut hasher);
        self.tree_painter.tip_shape.hash(&mut hasher);
        self.tree_painter.node_shape.hash(&mut hasher);
        self.tree_painter.tip_shape_size_mode.hash(&mut hasher);
        self.tree_painter.node_shape_size_mode.hash(&mut hasher);
        self.tree_painter.tip_shape_size_attribute.hash(&mut hasher);
        self.tree_painter
            .node_shape_size_attribute
            .hash(&mut hasher);
        self.tree_painter
            .tip_shape_max_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .node_shape_max_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .tip_shape_min_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .node_shape_min_size
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter.tip_shape_color_mode.hash(&mut hasher);
        self.tree_painter.node_shape_color_mode.hash(&mut hasher);
        self.tree_painter.node_radius.to_bits().hash(&mut hasher);

        self.tree_painter
            .branch_stroke
            .width
            .to_bits()
            .hash(&mut hasher);
        self.tree_painter
            .branch_highlight_stroke
            .width
            .to_bits()
            .hash(&mut hasher);
        Self::hash_color(&mut hasher, self.tree_painter.tip_selection_color);
        hasher.finish()
    }

    fn render_with_current_backend(
        &mut self,
        tree: &Tree,
        layout: &crate::tree::layout::TreeLayout,
        rect: egui::Rect,
        inner: egui::Rect,
        selected_nodes: &std::collections::HashSet<NodeId>,
        selected_tips: &std::collections::HashSet<NodeId>,
        canvas_width: f32,
        canvas_height: f32,
        scene_layer: SceneLayer,
    ) -> Result<crate::tree::skia_renderer::SkiaRenderOutput, String> {
        match self.render_backend {
            RenderBackend::Skia => self.skia_renderer.render(
                tree,
                layout,
                &self.tree_painter,
                selected_nodes,
                selected_tips,
                rect,
                inner,
                inner,
                1.0,
                Some(self.tree_viewer.selection_mode()),
                scene_layer,
                self.pixels_per_point,
                self.render_resolution_scale(canvas_width, canvas_height),
            ),
            RenderBackend::Vello => self.vello_renderer.render(
                tree,
                layout,
                &self.tree_painter,
                selected_nodes,
                selected_tips,
                rect,
                inner,
                inner,
                1.0,
                Some(self.tree_viewer.selection_mode()),
                scene_layer,
                self.pixels_per_point,
                self.render_resolution_scale(canvas_width, canvas_height),
            ),
        }
    }

    fn render_resolution_scale(&self, canvas_width: f32, canvas_height: f32) -> f32 {
        let ppp = self.pixels_per_point.max(1.0);
        let width_px = (canvas_width.max(1.0) * ppp).max(1.0);
        let height_px = (canvas_height.max(1.0) * ppp).max(1.0);

        let edge_scale = (Self::MAX_RENDER_TEXTURE_EDGE / width_px)
            .min(Self::MAX_RENDER_TEXTURE_EDGE / height_px)
            .min(1.0);

        let area = width_px * height_px;
        let area_scale = if area > Self::MAX_RENDER_PIXELS {
            (Self::MAX_RENDER_PIXELS / area).sqrt()
        } else {
            1.0
        };

        edge_scale.min(area_scale).clamp(0.1, 1.0)
    }

    fn draw_tree_canvas(&mut self, ui: &mut egui::Ui) {
        if let Some(tree_ref) = self.tree_viewer.current_tree() {
            let raw_height = ui.available_height();
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

            let raw_width = ui.available_width();
            let available_width = if raw_width.is_finite() {
                (raw_width - 20.0).max(400.0)
            } else {
                800.0
            };

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

            let base_signature = self.base_render_signature(tree_ref, canvas_width, canvas_height);
            let decor_signature =
                self.decor_render_signature(tree_ref, canvas_width, canvas_height);
            let interaction_signature =
                self.interaction_render_signature(tree_ref, canvas_width, canvas_height);

            let needs_base = self.base_tree_texture.is_none()
                || self.base_render_signature != Some(base_signature);
            let needs_decor = self.decor_tree_texture.is_none()
                || self.decor_render_signature != Some(decor_signature);
            let needs_interaction = self.interaction_tree_texture.is_none()
                || self.interaction_render_signature != Some(interaction_signature);

            if needs_base || needs_decor || needs_interaction {
                if let Some((tree, layout)) = self.build_display_tree_layout() {
                    let selected_nodes = self.tree_viewer.selected_nodes().clone();
                    let selected_tips = self.tree_viewer.selected_tips().clone();

                    if needs_base {
                        match self.render_with_current_backend(
                            &tree,
                            &layout,
                            rect,
                            inner,
                            &selected_nodes,
                            &selected_tips,
                            canvas_width,
                            canvas_height,
                            SceneLayer::Base,
                        ) {
                            Ok(output) => {
                                if let Some(texture) = self.base_tree_texture.as_mut() {
                                    texture.set(output.image, egui::TextureOptions::LINEAR);
                                } else {
                                    self.base_tree_texture = Some(ui.ctx().load_texture(
                                        "tree_canvas_base",
                                        output.image,
                                        egui::TextureOptions::LINEAR,
                                    ));
                                }
                                self.base_render_signature = Some(base_signature);
                            }
                            Err(err) => {
                                self.last_error = Some(format!("Base layer render failed: {err}"));
                                self.base_render_signature = None;
                            }
                        }
                    }

                    if needs_decor {
                        match self.render_with_current_backend(
                            &tree,
                            &layout,
                            rect,
                            inner,
                            &selected_nodes,
                            &selected_tips,
                            canvas_width,
                            canvas_height,
                            SceneLayer::Decor,
                        ) {
                            Ok(output) => {
                                if let Some(texture) = self.decor_tree_texture.as_mut() {
                                    texture.set(output.image, egui::TextureOptions::LINEAR);
                                } else {
                                    self.decor_tree_texture = Some(ui.ctx().load_texture(
                                        "tree_canvas_decor",
                                        output.image,
                                        egui::TextureOptions::LINEAR,
                                    ));
                                }
                                self.cached_tip_label_hits_local = output.tip_label_hits;
                                self.decor_render_signature = Some(decor_signature);
                            }
                            Err(err) => {
                                self.last_error = Some(format!("Decor layer render failed: {err}"));
                                self.decor_render_signature = None;
                                self.cached_tip_label_hits_local.clear();
                            }
                        }
                    }

                    if needs_interaction {
                        match self.render_with_current_backend(
                            &tree,
                            &layout,
                            rect,
                            inner,
                            &selected_nodes,
                            &selected_tips,
                            canvas_width,
                            canvas_height,
                            SceneLayer::Interaction,
                        ) {
                            Ok(output) => {
                                if let Some(texture) = self.interaction_tree_texture.as_mut() {
                                    texture.set(output.image, egui::TextureOptions::LINEAR);
                                } else {
                                    self.interaction_tree_texture = Some(ui.ctx().load_texture(
                                        "tree_canvas_interaction",
                                        output.image,
                                        egui::TextureOptions::LINEAR,
                                    ));
                                }
                                self.interaction_render_signature = Some(interaction_signature);
                            }
                            Err(err) => {
                                self.last_error =
                                    Some(format!("Interaction layer render failed: {err}"));
                                self.interaction_render_signature = None;
                            }
                        }
                    }
                } else {
                    ui.label("Tree structure unavailable for rendering.");
                    return;
                }
            }

            let uv = egui::Rect::from_min_max(egui::pos2(0.0, 0.0), egui::pos2(1.0, 1.0));
            for texture in [
                self.base_tree_texture.as_ref(),
                self.decor_tree_texture.as_ref(),
                self.interaction_tree_texture.as_ref(),
            ]
            .into_iter()
            .flatten()
            {
                painter.image(texture.id(), rect, uv, egui::Color32::WHITE);
            }

            let tip_label_hits: Vec<_> = self
                .cached_tip_label_hits_local
                .iter()
                .cloned()
                .map(|mut hit| {
                    hit.rect = hit.rect.translate(rect.min.to_vec2());
                    hit.anchor_pos += rect.min.to_vec2();
                    hit
                })
                .collect();

            self.tree_canvas_rect = Some(rect);

            if self.pending_filter_auto_scroll {
                let selected_tips = self.tree_viewer.selected_tips();
                let first_hit = tip_label_hits
                    .iter()
                    .find(|hit| selected_tips.contains(&hit.node_id));
                if let Some(hit) = first_hit {
                    let visible_rect = ui.clip_rect();
                    if !visible_rect.intersects(hit.rect) {
                        ui.scroll_to_rect(hit.rect, Some(egui::Align::Center));
                    }
                }
                self.pending_filter_auto_scroll = false;
            }

            if response.clicked() {
                if let Some(pointer_pos) = response.interact_pointer_pos() {
                    if let Some((tree, layout)) = self.build_display_tree_layout() {
                        let mut handled = false;
                        let mode = self.tree_viewer.selection_mode();
                        let to_screen = self
                            .tree_painter
                            .create_to_screen_transform(&tree, &layout, inner);

                        fn distance_to_segment(
                            point: egui::Pos2,
                            seg: (egui::Pos2, egui::Pos2),
                        ) -> f32 {
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

                        if matches!(mode, SelectionMode::Taxa | SelectionMode::Tips) {
                            if let Some(hit) = tip_label_hits
                                .iter()
                                .find(|hit| hit.contains(pointer_pos, 2.0))
                            {
                                self.tree_viewer.select_tip_label(hit.node_id);
                                handled = true;
                            }
                        }

                        if !handled && matches!(mode, SelectionMode::Taxa | SelectionMode::Tips) {
                            let tip_hit_radius = self.tree_painter.node_radius + 6.0;
                            if let Some(closest_tip) = tree
                                .external_nodes()
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

                        if !handled {
                            let branch_hit_radius = 8.0_f32;
                            let mut best_branch = None;
                            let mut best_distance = f32::INFINITY;

                            if !layout.continuous_branches.is_empty() {
                                for branch in &layout.continuous_branches {
                                    if branch.points.len() >= 2 {
                                        for i in 0..branch.points.len() - 1 {
                                            let p1 = to_screen(branch.points[i]);
                                            let p2 = to_screen(branch.points[i + 1]);
                                            let distance =
                                                distance_to_segment(pointer_pos, (p1, p2));
                                            if distance <= branch_hit_radius
                                                && distance < best_distance
                                            {
                                                best_branch = Some(branch.child);
                                                best_distance = distance;
                                            }
                                        }
                                    }
                                }
                            } else {
                                for (parent, child) in &layout.edges {
                                    let parent_pos = to_screen(layout.positions[*parent]);
                                    let child_pos = to_screen(layout.positions[*child]);
                                    let distance =
                                        distance_to_segment(pointer_pos, (parent_pos, child_pos));
                                    if distance <= branch_hit_radius && distance < best_distance {
                                        best_branch = Some(*child);
                                        best_distance = distance;
                                    }
                                }
                            }

                            if let Some(branch_child) = best_branch {
                                match mode {
                                    SelectionMode::Nodes => {
                                        self.tree_viewer.select_branch_node(branch_child)
                                    }
                                    SelectionMode::Clade => {
                                        self.tree_viewer.select_clade_nodes(branch_child)
                                    }
                                    SelectionMode::Taxa | SelectionMode::Tips => {
                                        self.tree_viewer.select_clade_tips(branch_child)
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
            }
        }
    }
}

impl eframe::App for FigTreeGui {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.consume_system_open_requests(ctx);

        // Store pixels_per_point for export
        self.pixels_per_point = ctx.pixels_per_point();

        // Handle screenshot results for export
        ctx.input(|i| {
            for event in &i.events {
                if let egui::Event::Screenshot { image, .. } = event {
                    if let Some((path, format)) = self.pending_export_path.take() {
                        match self.save_screenshot_to_file(image, &path, format) {
                            Ok(_) => {
                                self.status =
                                    format!("Successfully exported to {}", path.display());
                            }
                            Err(e) => {
                                self.last_error = Some(format!("Failed to export: {}", e));
                            }
                        }
                    }
                }
            }
        });

        let (undo_pressed, redo_pressed) = ctx.input(|i| {
            let undo = i.modifiers.command && i.key_pressed(egui::Key::Z) && !i.modifiers.shift;
            let redo = (i.modifiers.command && i.modifiers.shift && i.key_pressed(egui::Key::Z))
                || (i.modifiers.command && i.key_pressed(egui::Key::Y));
            (undo, redo)
        });
        if undo_pressed {
            self.undo_last_edit();
            ctx.request_repaint();
        } else if redo_pressed {
            self.redo_last_edit();
            ctx.request_repaint();
        }

        // Menu bar
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        // Spawn a new instance of the application
                        if let Ok(exe_path) = std::env::current_exe() {
                            match Command::new(exe_path).spawn() {
                                Ok(_) => {
                                    info!("Successfully spawned new application instance");
                                    self.status = "New window opened.".to_string();
                                }
                                Err(e) => {
                                    error!("Failed to spawn new instance: {}", e);
                                    self.last_error =
                                        Some(format!("Failed to open new window: {}", e));
                                }
                            }
                        } else {
                            error!("Failed to get current executable path");
                            self.last_error = Some("Failed to get executable path".to_string());
                        }
                        ui.close();
                    }

                    if ui.button("Open...").clicked() {
                        self.open_file_dialog();
                        ui.close();
                    }

                    let has_tree = self.config.tree_path.is_some();
                    if ui
                        .add_enabled(has_tree, egui::Button::new("Reload"))
                        .clicked()
                    {
                        if let Some(path) = self.config.tree_path.clone() {
                            if let Err(err) = self.load_from_path(path) {
                                self.last_error = Some(err);
                            }
                        }
                        ui.close();
                    }

                    ui.separator();

                    if ui.button("Close").clicked() {
                        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                    }

                    if ui
                        .add_enabled(
                            self.tree_viewer.current_tree().is_some(),
                            egui::Button::new("Save"),
                        )
                        .clicked()
                    {
                        self.save_rtr();
                        ui.close();
                    }

                    if ui
                        .add_enabled(
                            self.tree_viewer.current_tree().is_some(),
                            egui::Button::new("Save As..."),
                        )
                        .clicked()
                    {
                        self.save_rtr_as_dialog();
                        ui.close();
                    }

                    if ui
                        .add_enabled(false, egui::Button::new("Import Annotations..."))
                        .clicked()
                    {
                        // TODO: Import annotations
                        ui.close();
                    }

                    if ui
                        .add_enabled(false, egui::Button::new("Import Colour Scheme..."))
                        .clicked()
                    {
                        // TODO: Import colour scheme
                        ui.close();
                    }

                    ui.separator();

                    if ui
                        .add_enabled(false, egui::Button::new("Export Trees..."))
                        .clicked()
                    {
                        // TODO: Export trees
                        ui.close();
                    }

                    if ui.button("Export PDF").clicked() {
                        self.export_pdf_dialog();
                        ui.close();
                    }

                    if ui.button("Export SVG").clicked() {
                        self.export_svg_dialog();
                        ui.close();
                    }

                    if ui.button("Export PNG").clicked() {
                        self.export_png_dialog(ctx);
                        ui.close();
                    }

                    if ui.button("Export JPEG").clicked() {
                        self.export_jpeg_dialog(ctx);
                        ui.close();
                    }
                });

                ui.menu_button("Edit", |ui| {
                    if ui
                        .add_enabled(!self.undo_stack.is_empty(), egui::Button::new("Undo"))
                        .clicked()
                    {
                        self.undo_last_edit();
                        ui.close();
                        ctx.request_repaint();
                    }
                    if ui
                        .add_enabled(!self.redo_stack.is_empty(), egui::Button::new("Redo"))
                        .clicked()
                    {
                        self.redo_last_edit();
                        ui.close();
                        ctx.request_repaint();
                    }
                    ui.separator();
                    if ui.button("Clear Selection").clicked() {
                        self.tree_viewer.clear_selection();
                        ui.close();
                        ctx.request_repaint();
                    }
                    let has_highlights = !self.tree_painter.highlighted_clades().is_empty();
                    if ui
                        .add_enabled(has_highlights, egui::Button::new("Clear Highlight"))
                        .clicked()
                    {
                        self.record_undo_step();
                        self.tree_painter.clear_highlighted_clades();
                        self.status = "Cleared all highlights".to_string();
                        ui.close();
                        ctx.request_repaint();
                    }
                });

                ui.menu_button("Tree", |ui| {
                    let branch_target = self.selected_branch_node();
                    if ui
                        .add_enabled(branch_target.is_some(), egui::Button::new("Reroot..."))
                        .clicked()
                    {
                        if let Some(target) = branch_target {
                            self.record_undo_step();
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
                            ui.close();
                            ctx.request_repaint();
                        }
                    }

                    ui.separator();

                    if ui.button("Order Increasing").clicked() {
                        let can_order = self.tree_viewer.current_tree().is_some();
                        if can_order {
                            self.record_undo_step();
                        }
                        if let Some(tree) = self.tree_viewer.current_tree_mut() {
                            tree.order_nodes(true);
                            self.status = "Ordered nodes by increasing clade size.".to_string();
                            ui.close();
                            ctx.request_repaint();
                        }
                    }

                    if ui.button("Order Decreasing").clicked() {
                        let can_order = self.tree_viewer.current_tree().is_some();
                        if can_order {
                            self.record_undo_step();
                        }
                        if let Some(tree) = self.tree_viewer.current_tree_mut() {
                            tree.order_nodes(false);
                            self.status = "Ordered nodes by decreasing clade size.".to_string();
                            ui.close();
                            ctx.request_repaint();
                        }
                    }

                    ui.separator();

                    let has_highlights = !self.tree_painter.highlighted_clades().is_empty();
                    if ui
                        .add_enabled(has_highlights, egui::Button::new("Clear Highlights"))
                        .clicked()
                    {
                        self.record_undo_step();
                        self.tree_painter.clear_highlighted_clades();
                        self.status = "Cleared all highlights".to_string();
                        ui.close();
                        ctx.request_repaint();
                    }
                });
            });
        });

        // Minimal top menu with only file operations
        egui::TopBottomPanel::top("rhaetree_menu")
            .min_height(28.0)
            .max_height(32.0)
            .show(ctx, |ui| {
                ui.columns(3, |columns| {
                    columns[0].with_layout(
                        egui::Layout::left_to_right(egui::Align::Center),
                        |ui| {
                            let branch_target = self.selected_branch_node();
                            let reroot_response = ui.add_enabled(
                                branch_target.is_some(),
                                egui::Button::new("⟳ Reroot"),
                            );
                            if reroot_response.clicked() {
                                if let Some(target) = branch_target {
                                    self.record_undo_step();
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

                            // Rotate button
                            let has_node_selection = !self.tree_viewer.selected_nodes().is_empty();
                            let rotate_response =
                                ui.add_enabled(has_node_selection, egui::Button::new("↻ Rotate"));
                            if rotate_response.clicked() {
                                // Rotate the selected node's subtree
                                if let Some(&node_id) =
                                    self.tree_viewer.selected_nodes().iter().next()
                                {
                                    self.record_undo_step();
                                    if let Some(tree) = self.tree_viewer.current_tree_mut() {
                                        tree.rotate_node(node_id);
                                        self.status = format!("Rotated node {}", node_id);
                                        ctx.request_repaint();
                                    }
                                }
                            }

                            let has_selection = !self.tree_viewer.selected_tips().is_empty()
                                || !self.tree_viewer.selected_nodes().is_empty();
                            let color_response =
                                ui.add_enabled(has_selection, egui::Button::new("🎨 Color"));
                            if color_response.clicked() {
                                let initial = self.selection_current_color();
                                self.open_color_picker(
                                    color_response.rect.right_top(),
                                    initial,
                                    ColorPickerTarget::Selection,
                                );
                            }

                            // Highlight button
                            let has_node_selection_for_highlight =
                                !self.tree_viewer.selected_nodes().is_empty();
                            let highlight_response = ui.add_enabled(
                                has_node_selection_for_highlight,
                                egui::Button::new("✨ Highlight"),
                            );
                            if highlight_response.clicked() {
                                // Open highlight color picker
                                let highlighted_root = self.selected_branch_node().or_else(|| {
                                    self.tree_viewer.selected_nodes().iter().next().copied()
                                });
                                if let Some(node_id) = highlighted_root {
                                    self.highlighted_clade = Some(node_id);
                                    let initial = self.highlight_picker_color;
                                    self.highlight_picker_hex_input = format!(
                                        "#{:02X}{:02X}{:02X}",
                                        initial.r(),
                                        initial.g(),
                                        initial.b()
                                    );
                                    self.highlight_picker_mode = ColorValueMode::Hex;
                                    self.highlight_picker_open = true;
                                    self.highlight_picker_origin =
                                        Some(highlight_response.rect.right_top());
                                    self.highlight_picker_popup_open = false;
                                    self.highlight_picker_popup_rect = None;
                                }
                            }

                            // Annotate button
                            let has_single_tip_selected =
                                self.tree_viewer.selected_tips().len() == 1;
                            let annotate_response = ui.add_enabled(
                                has_single_tip_selected,
                                egui::Button::new("📝 Annotate"),
                            );
                            if annotate_response.clicked() {
                                // Open annotation dialog
                                if let Some(&tip_id) =
                                    self.tree_viewer.selected_tips().iter().next()
                                {
                                    if let Some(tree) = self.tree_viewer.current_tree() {
                                        if let Some(node) = tree.nodes.get(tip_id) {
                                            self.annotate_node_id = Some(tip_id);
                                            // Use last saved field choice
                                            self.annotate_field = self.last_saved_annotate_field;
                                            // Initialize with current value from the selected field
                                            self.annotate_text = match self.annotate_field {
                                                AnnotateField::Names => {
                                                    node.name.clone().unwrap_or_default()
                                                }
                                                AnnotateField::Labels => {
                                                    node.label.clone().unwrap_or_default()
                                                }
                                            };
                                            self.annotate_dialog_open = true;
                                        }
                                    }
                                }
                            }
                        },
                    );

                    // Empty middle column
                    columns[1]
                        .with_layout(egui::Layout::left_to_right(egui::Align::Center), |_ui| {});

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

                            ui.add_space(8.0);

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

                            ui.add_space(6.0);

                            // Selection Mode buttons
                            let mode = self.tree_viewer.selection_mode();
                            let options = [
                                (SelectionMode::Taxa, "Taxa"),
                                (SelectionMode::Clade, "Clade"),
                                (SelectionMode::Nodes, "Node"),
                            ];
                            for (idx, (selection, label)) in options.iter().enumerate() {
                                if idx > 0 {
                                    ui.add_space(2.0);
                                }
                                let button = egui::Button::new(*label).selected(mode == *selection);
                                if ui.add(button).clicked() {
                                    self.tree_viewer.set_selection_mode(*selection);
                                    self.tree_viewer.clear_selection();
                                }
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

            window.open(&mut open).show(ctx, |ui| {
                ui.horizontal(|ui| {
                    let custom_btn = egui::Button::new("Custom")
                        .selected(matches!(self.color_picker_panel, ColorPanelMode::Custom));
                    if ui.add(custom_btn).clicked() {
                        self.color_picker_panel = ColorPanelMode::Custom;
                    }
                    let library_btn = egui::Button::new("Library")
                        .selected(matches!(self.color_picker_panel, ColorPanelMode::Library));
                    if ui.add(library_btn).clicked() {
                        self.color_picker_panel = ColorPanelMode::Library;
                    }
                });

                ui.separator();

                let mut picker_changed = false;
                match self.color_picker_panel {
                    ColorPanelMode::Custom => {
                        let rgb_input_width = 30.0;
                        let rgb_inputs_total_width =
                            rgb_input_width * 3.0 + ui.spacing().item_spacing.x * 2.0;
                        ui.horizontal(|ui| {
                            let toggle_label = match self.color_picker_mode {
                                ColorValueMode::Hex => "HEX",
                                ColorValueMode::Rgb => "RGB",
                            };
                            if ui.button(toggle_label).clicked() {
                                self.color_picker_mode = match self.color_picker_mode {
                                    ColorValueMode::Hex => ColorValueMode::Rgb,
                                    ColorValueMode::Rgb => ColorValueMode::Hex,
                                };
                                if matches!(self.color_picker_mode, ColorValueMode::Hex) {
                                    self.color_picker_hex_input = format!(
                                        "#{:02X}{:02X}{:02X}",
                                        color.r(),
                                        color.g(),
                                        color.b()
                                    );
                                }
                            }

                            match self.color_picker_mode {
                                ColorValueMode::Hex => {
                                    let response = ui
                                        .scope(|ui| {
                                            let mut style = (**ui.style()).clone();
                                            let input_bg = Color32::from_gray(235);
                                            style.visuals.widgets.inactive.bg_fill = input_bg;
                                            style.visuals.widgets.hovered.bg_fill = input_bg;
                                            style.visuals.widgets.active.bg_fill = input_bg;
                                            ui.set_style(style);
                                            ui.add_sized(
                                                [rgb_inputs_total_width, 20.0],
                                                egui::TextEdit::singleline(
                                                    &mut self.color_picker_hex_input,
                                                ),
                                            )
                                        })
                                        .inner;
                                    if response.changed() {
                                        if let Some(parsed) =
                                            parse_hex_color(&self.color_picker_hex_input)
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
                                    let mut r = color.r() as i32;
                                    let mut g = color.g() as i32;
                                    let mut b = color.b() as i32;
                                    let mut rgb_changed = false;

                                    ui.scope(|ui| {
                                        let mut style = (**ui.style()).clone();
                                        let input_bg = Color32::from_gray(235);
                                        style.visuals.widgets.inactive.bg_fill = input_bg;
                                        style.visuals.widgets.hovered.bg_fill = input_bg;
                                        style.visuals.widgets.active.bg_fill = input_bg;
                                        ui.set_style(style);

                                        rgb_changed |= ui
                                            .add_sized(
                                                [rgb_input_width, 20.0],
                                                egui::DragValue::new(&mut r)
                                                    .range(0..=255)
                                                    .speed(1.0),
                                            )
                                            .changed();
                                        rgb_changed |= ui
                                            .add_sized(
                                                [rgb_input_width, 20.0],
                                                egui::DragValue::new(&mut g)
                                                    .range(0..=255)
                                                    .speed(1.0),
                                            )
                                            .changed();
                                        rgb_changed |= ui
                                            .add_sized(
                                                [rgb_input_width, 20.0],
                                                egui::DragValue::new(&mut b)
                                                    .range(0..=255)
                                                    .speed(1.0),
                                            )
                                            .changed();
                                    });

                                    if rgb_changed {
                                        color = Color32::from_rgb(r as u8, g as u8, b as u8);
                                        self.color_picker_hex_input = format!(
                                            "#{:02X}{:02X}{:02X}",
                                            color.r(),
                                            color.g(),
                                            color.b()
                                        );
                                    }
                                }
                            }
                        });

                        ui.separator();

                        let palette_colors = self.picker_palette_colors();
                        ui.horizontal(|ui| {
                            // Visual gap = item_spacing + shrink(1.0) on both adjacent cells.
                            // Target 6 px actual spacing across DPI scales.
                            let target_gap_points = 6.0 / ctx.pixels_per_point();
                            ui.spacing_mut().item_spacing.x = (target_gap_points - 2.0).max(0.0);
                            let shown = palette_colors.len().min(10);
                            if shown > 0 {
                                let spacing = ui.spacing().item_spacing.x;
                                let row_width = ui.available_width().max(80.0);
                                let cell = ((row_width - spacing * (shown as f32 - 1.0))
                                    / shown as f32)
                                    .clamp(11.0, 20.0);
                                for palette_color in palette_colors.into_iter().take(shown) {
                                    let (rect, response) = ui.allocate_exact_size(
                                        egui::vec2(cell, cell),
                                        egui::Sense::click(),
                                    );
                                    ui.painter()
                                        .rect_filled(rect.shrink(1.0), 3.0, palette_color);
                                    if response.clicked() {
                                        color = palette_color;
                                        picker_changed = true;
                                    }
                                }
                            }
                        });

                        // Keep the built-in picker body, but clip away its first control row (U8/copy/RGB).
                        let clip_top = ui.spacing().interact_size.y + ui.spacing().item_spacing.y;
                        let side_padding = 0.0;
                        let picker_height = 250.0;
                        let row_width = ui.available_width().max(96.0);
                        let (row_rect, _) = ui.allocate_exact_size(
                            egui::vec2(row_width, picker_height),
                            egui::Sense::hover(),
                        );
                        let picker_rect = row_rect.shrink2(egui::vec2(side_padding, 0.0));
                        let picker_width = picker_rect.width().max(80.0);
                        let shifted_rect = picker_rect.translate(egui::vec2(0.0, -clip_top));
                        ui.scope_builder(egui::UiBuilder::new().max_rect(shifted_rect), |ui| {
                            ui.set_clip_rect(picker_rect);
                            let mut style = (**ui.style()).clone();
                            style.spacing.slider_width = picker_width;
                            style.spacing.item_spacing *= 0.8;
                            ui.set_style(style);
                            if color_picker::color_picker_color32(
                                ui,
                                &mut color,
                                color_picker::Alpha::Opaque,
                            ) {
                                picker_changed = true;
                            }
                        });
                    }
                    ColorPanelMode::Library => {
                        let library_colors: [[u8; 3]; 12] = [
                            [255, 53, 64],
                            [255, 142, 40],
                            [247, 202, 0],
                            [55, 192, 86],
                            [26, 184, 168],
                            [27, 179, 192],
                            [24, 167, 206],
                            [28, 131, 229],
                            [93, 83, 221],
                            [184, 50, 205],
                            [255, 40, 94],
                            [173, 130, 95],
                        ];
                        let cols = 6;
                        let spacing_x = 10.0;
                        let spacing_y = 10.0;
                        let row_width = ui.available_width().max(120.0);
                        let cell = ((row_width - spacing_x * (cols as f32 - 1.0)) / cols as f32)
                            .clamp(18.0, 40.0);

                        for row in 0..2 {
                            ui.horizontal(|ui| {
                                ui.spacing_mut().item_spacing.x = spacing_x;
                                for col in 0..cols {
                                    let idx = row * cols + col;
                                    let rgb = library_colors[idx];
                                    let swatch = Color32::from_rgb(rgb[0], rgb[1], rgb[2]);
                                    let is_selected = swatch.r() == color.r()
                                        && swatch.g() == color.g()
                                        && swatch.b() == color.b();
                                    let stroke = if is_selected {
                                        egui::Stroke::new(2.0, Color32::from_gray(40))
                                    } else {
                                        egui::Stroke::NONE
                                    };
                                    let button = egui::Button::new("")
                                        .fill(swatch)
                                        .stroke(stroke)
                                        .corner_radius(8.0)
                                        .min_size(egui::vec2(cell, cell * 0.86));
                                    if ui.add(button).clicked() {
                                        color = swatch;
                                        picker_changed = true;
                                    }
                                }
                            });
                            if row == 0 {
                                ui.add_space(spacing_y);
                            }
                        }
                    }
                }

                if picker_changed {
                    self.color_picker_hex_input =
                        format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
                }

                ui.separator();
                ui.horizontal(|ui| {
                    if ui.button("Apply").clicked() {
                        self.apply_color_picker_target(color);
                        self.record_recent_color(color);
                        self.color_picker_open = false;
                        ctx.request_repaint();
                    }
                    if ui.button("Cancel").clicked() {
                        self.color_picker_open = false;
                    }
                });
            });

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

        // Highlight color picker window
        if self.highlight_picker_open {
            let mut open = self.highlight_picker_open;
            let mut color = Color32::from_rgb(
                self.highlight_picker_color.r(),
                self.highlight_picker_color.g(),
                self.highlight_picker_color.b(),
            );
            let mut window = egui::Window::new("")
                .resizable(false)
                .collapsible(false)
                .title_bar(false)
                .default_width(110.0);

            if let Some(anchor) = self.highlight_picker_origin {
                window = window.default_pos(anchor + egui::vec2(12.0, 0.0));
            }

            let mut request_popup_open = false;
            let mut window_rect: Option<egui::Rect> = None;

            let window_output = window.open(&mut open).show(ctx, |ui| {
                ui.horizontal(|ui| {
                    let toggle_label = match self.highlight_picker_mode {
                        ColorValueMode::Hex => "Show RGB",
                        ColorValueMode::Rgb => "Show HEX",
                    };
                    if ui.button(toggle_label).clicked() {
                        self.highlight_picker_mode = match self.highlight_picker_mode {
                            ColorValueMode::Hex => ColorValueMode::Rgb,
                            ColorValueMode::Rgb => ColorValueMode::Hex,
                        };
                        if matches!(self.highlight_picker_mode, ColorValueMode::Hex) {
                            self.highlight_picker_hex_input =
                                format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
                        }
                    }

                    match self.highlight_picker_mode {
                        ColorValueMode::Hex => {
                            let response = ui.add(
                                egui::TextEdit::singleline(&mut self.highlight_picker_hex_input)
                                    .desired_width(60.0),
                            );
                            if response.changed() {
                                if let Some(parsed) =
                                    parse_hex_color(&self.highlight_picker_hex_input)
                                {
                                    color = parsed;
                                    self.highlight_picker_hex_input = format!(
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
                        let opaque = Color32::from_rgb(color.r(), color.g(), color.b());
                        self.highlight_picker_color = opaque;
                        if let Some(node_id) = self.highlighted_clade {
                            self.record_undo_step();
                            self.tree_painter.add_highlighted_clade(node_id, opaque);
                            self.status = format!("Added highlight to clade at node {}", node_id);
                        }
                        self.highlight_picker_open = false;
                        ctx.request_repaint();
                        self.highlight_picker_popup_open = false;
                    }
                    if ui.button("Cancel").clicked() {
                        self.highlighted_clade = None;
                        self.highlight_picker_open = false;
                        self.highlight_picker_popup_open = false;
                    }
                });
            });

            if let Some(output) = window_output {
                window_rect = Some(output.response.rect);
            }

            if open {
                self.highlight_picker_popup_rect = window_rect;
                if request_popup_open {
                    self.highlight_picker_popup_open = true;
                }
            } else {
                self.highlight_picker_popup_open = false;
                self.highlight_picker_popup_rect = None;
            }

            if self.highlight_picker_popup_open {
                if let Some(parent_rect) = self.highlight_picker_popup_rect {
                    let mut popup_color = color;
                    let popup_pos = parent_rect.right_top() + egui::vec2(12.0, 0.0);
                    egui::Area::new("highlight_picker_popup".into())
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
                                    self.highlight_picker_popup_open = false;
                                }

                                if popup_changed {
                                    self.highlight_picker_hex_input = format!(
                                        "#{:02X}{:02X}{:02X}",
                                        popup_color.r(),
                                        popup_color.g(),
                                        popup_color.b()
                                    );
                                }
                            });
                        });

                    if self.highlight_picker_popup_open
                        && (popup_color.r() != color.r()
                            || popup_color.g() != color.g()
                            || popup_color.b() != color.b())
                    {
                        color = popup_color;
                        self.highlight_picker_hex_input =
                            format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
                    }
                } else {
                    self.highlight_picker_popup_open = false;
                }
            }

            self.highlight_picker_color = Color32::from_rgb(color.r(), color.g(), color.b());
            self.highlight_picker_open = open && self.highlight_picker_open;
            if self.highlight_picker_open {
                self.highlight_picker_hex_input =
                    format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
            } else {
                self.highlight_picker_origin = None;
                self.highlight_picker_popup_open = false;
                self.highlight_picker_popup_rect = None;
            }
        } else {
            self.highlight_picker_popup_open = false;
            self.highlight_picker_popup_rect = None;
        }

        // Node bar color picker window
        if self.node_bar_picker_open {
            let mut open = self.node_bar_picker_open;
            let mut color = self.node_bar_picker_color;
            let mut window = egui::Window::new("")
                .resizable(false)
                .collapsible(false)
                .title_bar(false)
                .default_width(110.0);

            if let Some(anchor) = self.node_bar_picker_origin {
                window = window.default_pos(anchor + egui::vec2(12.0, 0.0));
            }

            let mut request_popup_open = false;
            let mut window_rect: Option<egui::Rect> = None;

            let window_output = window.open(&mut open).show(ctx, |ui| {
                ui.horizontal(|ui| {
                    let toggle_label = match self.node_bar_picker_mode {
                        ColorValueMode::Hex => "Show RGB",
                        ColorValueMode::Rgb => "Show HEX",
                    };
                    if ui.button(toggle_label).clicked() {
                        self.node_bar_picker_mode = match self.node_bar_picker_mode {
                            ColorValueMode::Hex => ColorValueMode::Rgb,
                            ColorValueMode::Rgb => ColorValueMode::Hex,
                        };
                        if matches!(self.node_bar_picker_mode, ColorValueMode::Hex) {
                            self.node_bar_picker_hex_input =
                                format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
                        }
                    }

                    match self.node_bar_picker_mode {
                        ColorValueMode::Hex => {
                            let response = ui.add(
                                egui::TextEdit::singleline(&mut self.node_bar_picker_hex_input)
                                    .desired_width(60.0),
                            );
                            if response.changed() {
                                if let Some(parsed) =
                                    parse_hex_color(&self.node_bar_picker_hex_input)
                                {
                                    color = parsed;
                                    self.node_bar_picker_hex_input = format!(
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
                    ui.label(format!("Alpha: {:.0}%", color.a() as f32 / 255.0 * 100.0));
                    let mut alpha = color.a() as f32;
                    if ui
                        .add(egui::Slider::new(&mut alpha, 0.0..=255.0).show_value(false))
                        .changed()
                    {
                        let alpha_u8 = alpha.clamp(0.0, 255.0).round() as u8;
                        color = Color32::from_rgba_unmultiplied(
                            color.r(),
                            color.g(),
                            color.b(),
                            alpha_u8,
                        );
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
                if ui.button("Close").clicked() {
                    self.node_bar_picker_open = false;
                    self.node_bar_picker_popup_open = false;
                }
            });

            if let Some(output) = window_output {
                window_rect = Some(output.response.rect);
            }

            if open {
                self.node_bar_picker_popup_rect = window_rect;
                if request_popup_open {
                    self.node_bar_picker_popup_open = true;
                }
            } else {
                self.node_bar_picker_popup_open = false;
                self.node_bar_picker_popup_rect = None;
            }

            if self.node_bar_picker_popup_open {
                if let Some(parent_rect) = self.node_bar_picker_popup_rect {
                    let mut popup_color = color;
                    let popup_pos = parent_rect.right_top() + egui::vec2(12.0, 0.0);
                    egui::Area::new("node_bar_picker_popup".into())
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
                                        color_picker::Alpha::OnlyBlend,
                                    ) {
                                        popup_changed = true;
                                    }
                                });

                                ui.separator();
                                if ui.button("Close").clicked() {
                                    self.node_bar_picker_popup_open = false;
                                }

                                if popup_changed {
                                    self.node_bar_picker_hex_input = format!(
                                        "#{:02X}{:02X}{:02X}",
                                        popup_color.r(),
                                        popup_color.g(),
                                        popup_color.b()
                                    );
                                }
                            });
                        });

                    if self.node_bar_picker_popup_open
                        && (popup_color.r() != color.r()
                            || popup_color.g() != color.g()
                            || popup_color.b() != color.b()
                            || popup_color.a() != color.a())
                    {
                        color = popup_color;
                        self.node_bar_picker_hex_input =
                            format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
                    }
                } else {
                    self.node_bar_picker_popup_open = false;
                }
            }

            self.node_bar_picker_color = color;
            self.tree_painter.set_node_bar_color(color);
            ctx.request_repaint();
            self.node_bar_picker_open = open && self.node_bar_picker_open;
            if self.node_bar_picker_open {
                self.node_bar_picker_hex_input =
                    format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b());
            } else {
                self.node_bar_picker_origin = None;
                self.node_bar_picker_popup_open = false;
                self.node_bar_picker_popup_rect = None;
            }
        } else {
            self.node_bar_picker_popup_open = false;
            self.node_bar_picker_popup_rect = None;
        }

        // Annotate dialog
        if self.annotate_dialog_open {
            let mut open = self.annotate_dialog_open;
            egui::Window::new("Annotate Tip Label")
                .resizable(false)
                .collapsible(false)
                .show(ctx, |ui| {
                    ui.vertical(|ui| {
                        ui.horizontal(|ui| {
                            ui.label("Save to field:");
                            let old_field = self.annotate_field;
                            egui::ComboBox::from_id_salt("annotate_field_combo")
                                .selected_text(self.annotate_field.label())
                                .show_ui(ui, |ui| {
                                    ui.selectable_value(
                                        &mut self.annotate_field,
                                        AnnotateField::Names,
                                        "Names",
                                    );
                                    ui.selectable_value(
                                        &mut self.annotate_field,
                                        AnnotateField::Labels,
                                        "Labels",
                                    );
                                });

                            // Update text field when field selection changes
                            if old_field != self.annotate_field {
                                if let Some(node_id) = self.annotate_node_id {
                                    if let Some(tree) = self.tree_viewer.current_tree() {
                                        if let Some(node) = tree.nodes.get(node_id) {
                                            self.annotate_text = match self.annotate_field {
                                                AnnotateField::Names => {
                                                    node.name.clone().unwrap_or_default()
                                                }
                                                AnnotateField::Labels => {
                                                    node.label.clone().unwrap_or_default()
                                                }
                                            };
                                        }
                                    }
                                }
                            }
                        });

                        ui.add_space(8.0);

                        ui.label("Edit label text:");
                        ui.text_edit_singleline(&mut self.annotate_text);

                        ui.add_space(8.0);

                        ui.horizontal(|ui| {
                            if ui.button("Apply").clicked() {
                                // Apply the annotation
                                if let Some(node_id) = self.annotate_node_id {
                                    self.record_undo_step();
                                    if let Some(tree) = self.tree_viewer.current_tree_mut() {
                                        if let Some(node) = tree.node_mut(node_id) {
                                            match self.annotate_field {
                                                AnnotateField::Names => {
                                                    node.name = Some(self.annotate_text.clone());
                                                }
                                                AnnotateField::Labels => {
                                                    node.label = Some(self.annotate_text.clone());
                                                }
                                            }
                                            // Remember the last saved field choice
                                            self.last_saved_annotate_field = self.annotate_field;
                                            self.status = format!(
                                                "Updated {} for tip {}",
                                                match self.annotate_field {
                                                    AnnotateField::Names => "name",
                                                    AnnotateField::Labels => "label",
                                                },
                                                node_id
                                            );
                                        }
                                    }
                                }
                                open = false;
                                ctx.request_repaint();
                            }

                            if ui.button("Cancel").clicked() {
                                open = false;
                            }
                        });
                    });
                });

            self.annotate_dialog_open = open;
            if !open {
                self.annotate_node_id = None;
                self.annotate_text.clear();
            }
        }

        // Left sidebar for controls (narrow)
        egui::SidePanel::left("controls_panel")
            .resizable(true)
            .default_width(220.0)
            .min_width(180.0)
            .max_width(280.0)
            .show(ctx, |ui| {
                ui.visuals_mut().collapsing_header_frame = true;

                // Add scroll area for the control panels
                egui::ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .show(ui, |ui| {
                let prev_panel_states = self.panel_states;

                // Layout Panel (expanded by default)
                let layout_response =
                    egui::CollapsingHeader::new(egui::RichText::new("Layout").strong())
                    .id_salt("controls_layout")
                    .open(Some(self.panel_states.layout_expanded))
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
                                egui::Slider::new(&mut self.tree_viewer.zoom, 1.0..=5.0)
                                    .show_value(false),
                            );

                            ui.label("Expansion:");
                            ui.add(
                                egui::Slider::new(&mut self.tree_viewer.vertical_expansion, 1.0..=5.0)
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

                // Trees Panel
                let trees_response =
                    egui::CollapsingHeader::new(egui::RichText::new("Trees").strong())
                    .id_salt("controls_trees")
                    .open(Some(self.panel_states.trees_expanded))
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

                        let mut root_enabled = self.root_tree_enabled;
                        if ui.checkbox(&mut root_enabled, "Root tree").changed() {
                            self.record_undo_step();
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
                            self.record_undo_step();
                            let previous_method = self.root_method;
                            self.root_method = method;
                            self.apply_root_configuration(Some(previous_method));
                        }

                        ui.separator();

                        // Order nodes checkbox and combobox
                        let mut order_enabled = self.order_nodes_enabled;
                        if ui.checkbox(&mut order_enabled, "Order nodes").changed() {
                            self.record_undo_step();
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
                                self.record_undo_step();
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

                // Appearance Panel
                let appearance_response =
                    egui::CollapsingHeader::new(egui::RichText::new("Appearance").strong())
                    .id_salt("controls_appearance")
                    .open(Some(self.panel_states.appearance_expanded))
                    .default_open(self.panel_states.appearance_expanded)
                    .show(ui, |ui| {
                        ui.label("Renderer:");
                        let old_backend = self.render_backend;
                        egui::ComboBox::from_id_salt("renderer_backend")
                            .selected_text(self.render_backend.label())
                            .show_ui(ui, |ui| {
                                ui.selectable_value(
                                    &mut self.render_backend,
                                    RenderBackend::Skia,
                                    RenderBackend::Skia.label(),
                                );
                                ui.selectable_value(
                                    &mut self.render_backend,
                                    RenderBackend::Vello,
                                    RenderBackend::Vello.label(),
                                );
                            });
                        if self.render_backend != old_backend {
                            self.invalidate_layer_caches();
                            self.last_error = None;
                        }

                        ui.separator();
                        ui.label("Line Width:");
                        ui.add(
                            egui::Slider::new(
                                &mut self.tree_painter.branch_stroke.width,
                                1.0..=5.0,
                            )
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
                tip_state.set_open(self.panel_states.tip_labels_expanded);

                let tip_header_response = ui.horizontal(|ui| {
                    let toggle = tip_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_tip_labels = self.tree_painter.show_tip_labels;
                    if ui.checkbox(&mut show_tip_labels, "").changed() {
                        self.tree_painter.show_tip_labels = show_tip_labels;
                    }

                    let label_response = ui.add(
                        egui::Label::new(egui::RichText::new("Labels: Tip").strong())
                            .sense(egui::Sense::click()),
                    );
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
                                        TipLabelDisplay::Labels,
                                        "Labels",
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
                node_state.set_open(self.panel_states.node_labels_expanded);

                let header_response = ui.horizontal(|ui| {
                    let toggle = node_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_labels = self.tree_painter.show_node_labels;
                    if ui.checkbox(&mut show_labels, "").changed() {
                        self.tree_painter.show_node_labels = show_labels;
                    }

                    let label_response = ui.add(
                        egui::Label::new(egui::RichText::new("Labels: Node").strong())
                            .sense(egui::Sense::click()),
                    );
                    if label_response.clicked() {
                        node_state.toggle(ui);
                    }

                    toggle
                });

                node_state.show_body_indented(&header_response.response, ui, |ui| {
                    ui.vertical(|ui| {
                        let mut has_node_names = false;
                        let mut has_node_labels = false;
                        let mut attribute_fields: Vec<String> = self
                            .tree_viewer
                            .current_tree()
                            .map(|t| {
                                for n in &t.nodes {
                                    if n.name.as_deref().is_some_and(|v| !v.trim().is_empty()) {
                                        has_node_names = true;
                                    }
                                    if n.label.as_deref().is_some_and(|v| !v.trim().is_empty()) {
                                        has_node_labels = true;
                                    }
                                }
                                let mut keys = t.node_attribute_keys();
                                if keys.is_empty() {
                                    keys = t.node_numeric_attribute_keys();
                                }
                                keys
                            })
                            .unwrap_or_default();
                        attribute_fields.sort();
                        attribute_fields.dedup();

                        if matches!(self.tree_painter.node_label_display, NodeLabelDisplay::Names)
                            && !has_node_names
                        {
                            self.tree_painter.node_label_display = if has_node_labels {
                                NodeLabelDisplay::Labels
                            } else {
                                NodeLabelDisplay::BranchLength
                            };
                        }
                        if matches!(self.tree_painter.node_label_display, NodeLabelDisplay::Labels)
                            && !has_node_labels
                        {
                            self.tree_painter.node_label_display = if has_node_names {
                                NodeLabelDisplay::Names
                            } else {
                                NodeLabelDisplay::BranchLength
                            };
                        }

                        ui.horizontal(|ui| {
                            ui.label("Display:");
                            let selected_text = match self.tree_painter.node_label_display {
                                NodeLabelDisplay::Attribute => self
                                    .tree_painter
                                    .node_label_attribute
                                    .as_deref()
                                    .map(|k| format!("Attribute: {k}"))
                                    .unwrap_or_else(|| "Attribute".to_string()),
                                _ => self.tree_painter.node_label_display.label().to_string(),
                            };
                            egui::ComboBox::from_id_salt("node_labels_display")
                                .selected_text(selected_text)
                                .show_ui(ui, |ui| {
                                    if has_node_names {
                                        ui.selectable_value(
                                            &mut self.tree_painter.node_label_display,
                                            NodeLabelDisplay::Names,
                                            "Names",
                                        );
                                    }
                                    if has_node_labels {
                                        ui.selectable_value(
                                            &mut self.tree_painter.node_label_display,
                                            NodeLabelDisplay::Labels,
                                            "Labels",
                                        );
                                    }
                                    ui.selectable_value(
                                        &mut self.tree_painter.node_label_display,
                                        NodeLabelDisplay::BranchLength,
                                        "Branch Length",
                                    );
                                    ui.selectable_value(
                                        &mut self.tree_painter.node_label_display,
                                        NodeLabelDisplay::NodeHeight,
                                        "Node Height",
                                    );
                                    if !attribute_fields.is_empty() {
                                        ui.separator();
                                        for key in &attribute_fields {
                                            let selected =
                                                matches!(
                                                    self.tree_painter.node_label_display,
                                                    NodeLabelDisplay::Attribute
                                                ) && self.tree_painter.node_label_attribute.as_deref()
                                                    == Some(key.as_str());
                                            if ui
                                                .selectable_label(
                                                    selected,
                                                    format!("Attribute: {key}"),
                                                )
                                                .clicked()
                                            {
                                                self.tree_painter.node_label_display =
                                                    NodeLabelDisplay::Attribute;
                                                self.tree_painter.node_label_attribute =
                                                    Some(key.clone());
                                            }
                                        }
                                    }
                                });
                        });

                        ui.horizontal(|ui| {
                            ui.label("Font Size:");
                            ui.add(
                                egui::DragValue::new(&mut self.tree_painter.node_label_font_size)
                                    .speed(0.25)
                                    .range(6.0..=48.0),
                            );
                        });

                        let is_numeric = self.tree_painter.node_label_display.is_numeric();

                        ui.horizontal(|ui| {
                            ui.label("Format:");
                            ui.add_enabled_ui(is_numeric, |ui| {
                                egui::ComboBox::from_id_salt("node_labels_format")
                                    .selected_text(self.tree_painter.node_label_format.label())
                                    .show_ui(ui, |ui| {
                                        ui.selectable_value(
                                            &mut self.tree_painter.node_label_format,
                                            TipLabelNumberFormat::Decimal,
                                            "Decimal",
                                        );
                                        ui.selectable_value(
                                            &mut self.tree_painter.node_label_format,
                                            TipLabelNumberFormat::Scientific,
                                            "Scientific",
                                        );
                                        ui.selectable_value(
                                            &mut self.tree_painter.node_label_format,
                                            TipLabelNumberFormat::Percentage,
                                            "Percentage",
                                        );
                                    });
                            });
                        });

                        ui.horizontal(|ui| {
                            ui.label("Sig Digits:");
                            ui.add_enabled_ui(is_numeric, |ui| {
                                ui.add(
                                    egui::DragValue::new(
                                        &mut self.tree_painter.node_label_precision,
                                    )
                                    .speed(0.25)
                                    .range(0.0..=10.0),
                                );
                            });
                        });
                    });
                });
                self.panel_states.node_labels_expanded = node_state.is_open();

                let branch_header_id = ui.make_persistent_id("controls_branch_labels");
                let mut branch_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    branch_header_id,
                    self.panel_states.branch_labels_expanded,
                );
                branch_state.set_open(self.panel_states.branch_labels_expanded);

                let branch_header_response = ui.horizontal(|ui| {
                    let toggle = branch_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_branch = self.tree_painter.show_branch_labels;
                    if ui.checkbox(&mut show_branch, "").changed() {
                        self.tree_painter.show_branch_labels = show_branch;
                    }

                    let label_response = ui.add(
                        egui::Label::new(egui::RichText::new("Labels: Branch").strong())
                            .sense(egui::Sense::click()),
                    );
                    if label_response.clicked() {
                        branch_state.toggle(ui);
                    }

                    toggle
                });

                branch_state.show_body_indented(&branch_header_response.response, ui, |ui| {
                    if matches!(self.tree_painter.branch_label_display, BranchLabelDisplay::Names) {
                        self.tree_painter.branch_label_display = BranchLabelDisplay::BranchLength;
                    }

                    ui.horizontal(|ui| {
                        ui.label("Display:");
                        egui::ComboBox::from_id_salt("branch_labels_display")
                            .selected_text(self.tree_painter.branch_label_display.label())
                            .show_ui(ui, |ui| {
                                ui.selectable_value(
                                    &mut self.tree_painter.branch_label_display,
                                    BranchLabelDisplay::BranchLength,
                                    BranchLabelDisplay::BranchLength.label(),
                                );
                            });
                    });

                    ui.horizontal(|ui| {
                        ui.label("Font Size:");
                        ui.add(
                            egui::DragValue::new(&mut self.tree_painter.branch_label_font_size)
                                .speed(0.25)
                                .range(6.0..=48.0),
                        );
                    });

                    let is_numeric = self.tree_painter.branch_label_display.is_numeric();
                    ui.horizontal(|ui| {
                        ui.label("Format:");
                        ui.add_enabled_ui(is_numeric, |ui| {
                            egui::ComboBox::from_id_salt("branch_labels_format")
                                .selected_text(self.tree_painter.branch_label_format.label())
                                .show_ui(ui, |ui| {
                                    ui.selectable_value(
                                        &mut self.tree_painter.branch_label_format,
                                        TipLabelNumberFormat::Decimal,
                                        "Decimal",
                                    );
                                    ui.selectable_value(
                                        &mut self.tree_painter.branch_label_format,
                                        TipLabelNumberFormat::Scientific,
                                        "Scientific",
                                    );
                                    ui.selectable_value(
                                        &mut self.tree_painter.branch_label_format,
                                        TipLabelNumberFormat::Percentage,
                                        "Percentage",
                                    );
                                });
                        });
                    });

                    ui.horizontal(|ui| {
                        ui.label("Sig Digits:");
                        ui.add_enabled_ui(is_numeric, |ui| {
                            ui.add(
                                egui::DragValue::new(&mut self.tree_painter.branch_label_precision)
                                    .speed(0.25)
                                    .range(0.0..=10.0),
                            );
                        });
                    });
                });
                self.panel_states.branch_labels_expanded = branch_state.is_open();

                // Tip Shapes
                let tip_shapes_header_id = ui.make_persistent_id("controls_tip_shapes");
                let mut tip_shapes_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    tip_shapes_header_id,
                    self.panel_states.tip_shapes_expanded,
                );
                tip_shapes_state.set_open(self.panel_states.tip_shapes_expanded);

                let tip_shapes_header_response = ui.horizontal(|ui| {
                    let toggle = tip_shapes_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_tip_shapes = self.tree_painter.show_tip_shapes;
                    if ui.checkbox(&mut show_tip_shapes, "").changed() {
                        self.tree_painter.show_tip_shapes = show_tip_shapes;
                    }

                    let label_response = ui.add(
                        egui::Label::new(egui::RichText::new("Shapes: Tip").strong())
                            .sense(egui::Sense::click()),
                    );
                    if label_response.clicked() {
                        tip_shapes_state.toggle(ui);
                    }

                    toggle
                });

                tip_shapes_state.show_body_indented(&tip_shapes_header_response.response, ui, |ui| {
                    let mut numeric_fields: Vec<String> = self
                        .tree_viewer
                        .current_tree()
                        .map(|t| t.node_numeric_attribute_keys())
                        .unwrap_or_default();
                    numeric_fields.sort();
                    numeric_fields.dedup();

                    ui.horizontal(|ui| {
                        ui.label("Shape:");
                        egui::ComboBox::from_id_salt("tip_shape_type")
                            .selected_text(self.tree_painter.tip_shape.label())
                            .show_ui(ui, |ui| {
                                ui.selectable_value(
                                    &mut self.tree_painter.tip_shape,
                                    ShapeType::Circle,
                                    ShapeType::Circle.label(),
                                );
                                ui.selectable_value(
                                    &mut self.tree_painter.tip_shape,
                                    ShapeType::Square,
                                    ShapeType::Square.label(),
                                );
                                ui.selectable_value(
                                    &mut self.tree_painter.tip_shape,
                                    ShapeType::Diamond,
                                    ShapeType::Diamond.label(),
                                );
                            });
                    });

                    ui.horizontal(|ui| {
                        ui.label("Max size:");
                        ui.add(
                            egui::DragValue::new(&mut self.tree_painter.tip_shape_max_size)
                                .range(1.0..=64.0)
                                .speed(0.2),
                        );
                    });

                    ui.horizontal(|ui| {
                        ui.label("Size by:");
                        let selected = if matches!(
                            self.tree_painter.tip_shape_size_mode,
                            ShapeSizeMode::Fixed
                        ) {
                            "Fixed".to_string()
                        } else {
                            self.tree_painter
                                .tip_shape_size_attribute
                                .clone()
                                .unwrap_or_else(|| "Attribute".to_string())
                        };
                        egui::ComboBox::from_id_salt("tip_shape_size_by")
                            .selected_text(selected)
                            .show_ui(ui, |ui| {
                                if ui
                                    .selectable_label(
                                        matches!(
                                            self.tree_painter.tip_shape_size_mode,
                                            ShapeSizeMode::Fixed
                                        ),
                                        "Fixed",
                                    )
                                    .clicked()
                                {
                                    self.tree_painter.tip_shape_size_mode = ShapeSizeMode::Fixed;
                                    self.tree_painter.tip_shape_size_attribute = None;
                                }
                                for field in &numeric_fields {
                                    let selected = matches!(
                                        self.tree_painter.tip_shape_size_mode,
                                        ShapeSizeMode::Attribute
                                    ) && self.tree_painter.tip_shape_size_attribute.as_deref()
                                        == Some(field.as_str());
                                    if ui.selectable_label(selected, field).clicked() {
                                        self.tree_painter.tip_shape_size_mode =
                                            ShapeSizeMode::Attribute;
                                        self.tree_painter.tip_shape_size_attribute =
                                            Some(field.clone());
                                    }
                                }
                            });
                    });

                    if matches!(
                        self.tree_painter.tip_shape_size_mode,
                        ShapeSizeMode::Attribute
                    ) {
                        ui.horizontal(|ui| {
                            ui.label("Min size:");
                            ui.add(
                                egui::DragValue::new(&mut self.tree_painter.tip_shape_min_size)
                                    .range(0.5..=self.tree_painter.tip_shape_max_size.max(0.5))
                                    .speed(0.2),
                            );
                        });
                    }

                    ui.horizontal(|ui| {
                        ui.label("Color by:");
                        egui::ComboBox::from_id_salt("tip_shape_color_by")
                            .selected_text(self.tree_painter.tip_shape_color_mode.label())
                            .show_ui(ui, |ui| {
                                ui.selectable_value(
                                    &mut self.tree_painter.tip_shape_color_mode,
                                    ShapeColorMode::Fixed,
                                    ShapeColorMode::Fixed.label(),
                                );
                                ui.selectable_value(
                                    &mut self.tree_painter.tip_shape_color_mode,
                                    ShapeColorMode::UserSelection,
                                    ShapeColorMode::UserSelection.label(),
                                );
                            });
                    });
                    match self.tree_painter.tip_shape_color_mode {
                        ShapeColorMode::Fixed => {
                            ui.horizontal(|ui| {
                                ui.label("Color:");
                                let resp = ui.add(
                                    egui::Button::new(" ")
                                        .fill(self.tree_painter.tip_shape_fixed_color)
                                        .min_size(egui::vec2(24.0, 20.0)),
                                );
                                if resp.clicked() {
                                    self.open_color_picker(
                                        resp.rect.right_top(),
                                        self.tree_painter.tip_shape_fixed_color,
                                        ColorPickerTarget::TipShapeFixed,
                                    );
                                }
                            });
                        }
                        ShapeColorMode::UserSelection => {
                            ui.horizontal(|ui| {
                                ui.label("Color:");
                                let has_tip_selection =
                                    !self.selected_tip_nodes_for_shape_color().is_empty();
                                let current_color = self.tip_shape_selection_color();
                                let resp = ui.add_enabled(
                                    has_tip_selection,
                                    egui::Button::new(" ")
                                        .fill(current_color)
                                        .min_size(egui::vec2(24.0, 20.0)),
                                );
                                if resp.clicked() {
                                    self.open_color_picker(
                                        resp.rect.right_top(),
                                        current_color,
                                        ColorPickerTarget::TipShapeSelection,
                                    );
                                }
                            });
                        }
                    }
                });
                self.panel_states.tip_shapes_expanded = tip_shapes_state.is_open();

                // Node Shapes
                let node_shapes_header_id = ui.make_persistent_id("controls_node_shapes");
                let mut node_shapes_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    node_shapes_header_id,
                    self.panel_states.node_shapes_expanded,
                );
                node_shapes_state.set_open(self.panel_states.node_shapes_expanded);

                let node_shapes_header_response = ui.horizontal(|ui| {
                    let toggle = node_shapes_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_node_shapes = self.tree_painter.show_node_shapes;
                    if ui.checkbox(&mut show_node_shapes, "").changed() {
                        self.tree_painter.show_node_shapes = show_node_shapes;
                    }

                    let label_response = ui.add(
                        egui::Label::new(egui::RichText::new("Shapes: Node").strong())
                            .sense(egui::Sense::click()),
                    );
                    if label_response.clicked() {
                        node_shapes_state.toggle(ui);
                    }

                    toggle
                });

                node_shapes_state.show_body_indented(
                    &node_shapes_header_response.response,
                    ui,
                    |ui| {
                        let mut numeric_fields: Vec<String> = self
                            .tree_viewer
                            .current_tree()
                            .map(|t| t.node_numeric_attribute_keys())
                            .unwrap_or_default();
                        numeric_fields.sort();
                        numeric_fields.dedup();

                        ui.horizontal(|ui| {
                            ui.label("Shape:");
                            egui::ComboBox::from_id_salt("node_shape_type")
                                .selected_text(self.tree_painter.node_shape.label())
                                .show_ui(ui, |ui| {
                                    ui.selectable_value(
                                        &mut self.tree_painter.node_shape,
                                        ShapeType::Circle,
                                        ShapeType::Circle.label(),
                                    );
                                    ui.selectable_value(
                                        &mut self.tree_painter.node_shape,
                                        ShapeType::Square,
                                        ShapeType::Square.label(),
                                    );
                                    ui.selectable_value(
                                        &mut self.tree_painter.node_shape,
                                        ShapeType::Diamond,
                                        ShapeType::Diamond.label(),
                                    );
                                });
                        });

                        ui.horizontal(|ui| {
                            ui.label("Max size:");
                            ui.add(
                                egui::DragValue::new(&mut self.tree_painter.node_shape_max_size)
                                    .range(1.0..=64.0)
                                    .speed(0.2),
                            );
                        });

                        ui.horizontal(|ui| {
                            ui.label("Size by:");
                            let selected = if matches!(
                                self.tree_painter.node_shape_size_mode,
                                ShapeSizeMode::Fixed
                            ) {
                                "Fixed".to_string()
                            } else {
                                self.tree_painter
                                    .node_shape_size_attribute
                                    .clone()
                                    .unwrap_or_else(|| "Attribute".to_string())
                            };
                            egui::ComboBox::from_id_salt("node_shape_size_by")
                                .selected_text(selected)
                                .show_ui(ui, |ui| {
                                    if ui
                                        .selectable_label(
                                            matches!(
                                                self.tree_painter.node_shape_size_mode,
                                                ShapeSizeMode::Fixed
                                            ),
                                            "Fixed",
                                        )
                                        .clicked()
                                    {
                                        self.tree_painter.node_shape_size_mode =
                                            ShapeSizeMode::Fixed;
                                        self.tree_painter.node_shape_size_attribute = None;
                                    }
                                    for field in &numeric_fields {
                                        let selected = matches!(
                                            self.tree_painter.node_shape_size_mode,
                                            ShapeSizeMode::Attribute
                                        ) && self.tree_painter.node_shape_size_attribute.as_deref()
                                            == Some(field.as_str());
                                        if ui.selectable_label(selected, field).clicked() {
                                            self.tree_painter.node_shape_size_mode =
                                                ShapeSizeMode::Attribute;
                                            self.tree_painter.node_shape_size_attribute =
                                                Some(field.clone());
                                        }
                                    }
                                });
                        });

                        if matches!(
                            self.tree_painter.node_shape_size_mode,
                            ShapeSizeMode::Attribute
                        ) {
                            ui.horizontal(|ui| {
                                ui.label("Min size:");
                                ui.add(
                                    egui::DragValue::new(&mut self.tree_painter.node_shape_min_size)
                                        .range(0.5..=self.tree_painter.node_shape_max_size.max(0.5))
                                        .speed(0.2),
                                );
                            });
                        }

                        ui.horizontal(|ui| {
                            ui.label("Color by:");
                            egui::ComboBox::from_id_salt("node_shape_color_by")
                                .selected_text(self.tree_painter.node_shape_color_mode.label())
                                .show_ui(ui, |ui| {
                                    ui.selectable_value(
                                        &mut self.tree_painter.node_shape_color_mode,
                                        ShapeColorMode::Fixed,
                                        ShapeColorMode::Fixed.label(),
                                    );
                                    ui.selectable_value(
                                        &mut self.tree_painter.node_shape_color_mode,
                                        ShapeColorMode::UserSelection,
                                        ShapeColorMode::UserSelection.label(),
                                    );
                                });
                        });
                        match self.tree_painter.node_shape_color_mode {
                            ShapeColorMode::Fixed => {
                                ui.horizontal(|ui| {
                                    ui.label("Color:");
                                    let resp = ui.add(
                                        egui::Button::new(" ")
                                            .fill(self.tree_painter.node_shape_fixed_color)
                                            .min_size(egui::vec2(24.0, 20.0)),
                                    );
                                    if resp.clicked() {
                                        self.open_color_picker(
                                            resp.rect.right_top(),
                                            self.tree_painter.node_shape_fixed_color,
                                            ColorPickerTarget::NodeShapeFixed,
                                        );
                                    }
                                });
                            }
                            ShapeColorMode::UserSelection => {
                                ui.horizontal(|ui| {
                                    ui.label("Color:");
                                    let has_node_selection = self
                                        .tree_viewer
                                        .current_tree()
                                        .map(|tree| {
                                            self.tree_viewer
                                        .selected_nodes()
                                                .iter()
                                                .any(|id| !tree.nodes[*id].is_leaf())
                                        })
                                        .unwrap_or(false);
                                    let current_color = self.node_shape_selection_color();
                                    let resp = ui.add_enabled(
                                        has_node_selection,
                                        egui::Button::new(" ")
                                            .fill(current_color)
                                            .min_size(egui::vec2(24.0, 20.0)),
                                    );
                                    if resp.clicked() {
                                        self.open_color_picker(
                                            resp.rect.right_top(),
                                            current_color,
                                            ColorPickerTarget::NodeShapeSelection,
                                        );
                                    }
                                });
                            }
                        }
                    },
                );
                self.panel_states.node_shapes_expanded = node_shapes_state.is_open();

                // Node Bars
                let node_bars_header_id = ui.make_persistent_id("controls_node_bars");
                let mut node_bars_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    node_bars_header_id,
                    self.panel_states.node_bars_expanded,
                );
                node_bars_state.set_open(self.panel_states.node_bars_expanded);

                let node_bars_header_response = ui.horizontal(|ui| {
                    let toggle = node_bars_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_node_bars = self.tree_painter.show_node_bars;
                    if ui.checkbox(&mut show_node_bars, "").changed() {
                        self.tree_painter.show_node_bars = show_node_bars;
                    }

                    let label_response = ui.add(
                        egui::Label::new(egui::RichText::new("Node Bars").strong())
                            .sense(egui::Sense::click()),
                    );
                    if label_response.clicked() {
                        node_bars_state.toggle(ui);
                    }

                    toggle
                });

                node_bars_state.show_body_indented(&node_bars_header_response.response, ui, |ui| {
                    let maybe_tree = self.tree_viewer.current_tree();

                    let mut available_fields: Vec<String> = maybe_tree
                        .map(|tree| tree.node_numeric_range_keys())
                        .unwrap_or_default();
                    available_fields.sort();
                    available_fields.dedup();

                    if maybe_tree.is_none() {
                        ui.label("Load a tree to configure node bars.");
                        return;
                    }

                    if available_fields.is_empty() {
                        ui.label("No numeric range annotations found in this tree.");
                    } else {
                        let mut selection = self
                            .tree_painter
                            .node_bar_field()
                            .map(|field| field.to_string());
                        let previous = selection.clone();
                        let selected_label = selection
                            .as_deref()
                            .unwrap_or("Select field");

                        egui::ComboBox::from_id_salt("node_bar_field_combo")
                            .selected_text(selected_label)
                            .width(180.0)
                            .show_ui(ui, |ui| {
                                ui.selectable_value(&mut selection, None, "None");
                                for field in &available_fields {
                                    ui.selectable_value(
                                        &mut selection,
                                        Some(field.clone()),
                                        field,
                                    );
                                }
                            });

                        if selection != previous {
                            self.tree_painter.set_node_bar_field(selection);
                        }

                        ui.horizontal(|ui| {
                            ui.label("Bar thickness");
                            let mut thickness = self.tree_painter.node_bar_thickness();
                            if ui
                                .add(
                                    egui::DragValue::new(&mut thickness)
                                        .range(1.0..=60.0)
                                        .speed(0.25)
                                        .suffix(" px"),
                                )
                                .changed()
                            {
                                self.tree_painter.set_node_bar_thickness(thickness);
                            }
                        });

                        ui.horizontal(|ui| {
                            ui.label("Bar color");
                            let color = self.tree_painter.node_bar_color();
                            let (rect, response) = ui.allocate_exact_size(
                                egui::vec2(40.0, 18.0),
                                egui::Sense::click(),
                            );

                            if ui.is_rect_visible(rect) {
                                ui.painter().rect_filled(rect, 4.0, color);
                                let stroke = ui.visuals().widgets.noninteractive.bg_stroke;
                                ui.painter().rect_stroke(
                                    rect,
                                    4.0,
                                    stroke,
                                    egui::StrokeKind::Middle,
                                );
                            }

                            if response.clicked() {
                                let initial = self.tree_painter.node_bar_color();
                                self.node_bar_picker_color = initial;
                                self.node_bar_picker_hex_input = format!(
                                    "#{:02X}{:02X}{:02X}",
                                    initial.r(),
                                    initial.g(),
                                    initial.b()
                                );
                                self.node_bar_picker_mode = ColorValueMode::Hex;
                                self.node_bar_picker_open = true;
                                self.node_bar_picker_origin = Some(response.rect.right_top());
                                self.node_bar_picker_popup_open = false;
                                self.node_bar_picker_popup_rect = None;
                            }
                        });

                        if !self.tree_painter.show_node_bars {
                            ui.label("Enable \"Show Node Bars\" to display bars on the tree.");
                        }
                    }
                });
                self.panel_states.node_bars_expanded = node_bars_state.is_open();

                let scale_header_id = ui.make_persistent_id("controls_scale_bar");
                let mut scale_state = CollapsingState::load_with_default_open(
                    ui.ctx(),
                    scale_header_id,
                    self.panel_states.scale_bar_expanded,
                );
                scale_state.set_open(self.panel_states.scale_bar_expanded);

                let scale_header_response = ui.horizontal(|ui| {
                    let toggle = scale_state.show_toggle_button(ui, paint_default_icon);

                    let mut show_scale = self.tree_painter.show_scale_bar;
                    if ui.checkbox(&mut show_scale, "").changed() {
                        self.tree_painter.show_scale_bar = show_scale;
                    }

                    let label_response = ui.add(
                        egui::Label::new(egui::RichText::new("Scale Bar").strong())
                            .sense(egui::Sense::click()),
                    );
                    if label_response.clicked() {
                        scale_state.toggle(ui);
                    }

                    toggle
                });

                scale_state.show_body_indented(&scale_header_response.response, ui, |ui| {
                    ui.horizontal(|ui| {
                        ui.label("Scale Range:");
                        ui.add(
                            egui::DragValue::new(&mut self.tree_painter.scale_bar_range)
                                .speed(0.01)
                                .range(0.000001..=1_000_000.0),
                        );
                    });
                    ui.horizontal(|ui| {
                        ui.label("Font Size:");
                        ui.add(
                            egui::DragValue::new(&mut self.tree_painter.scale_bar_font_size)
                                .speed(0.25)
                                .range(6.0..=64.0),
                        );
                    });
                    ui.horizontal(|ui| {
                        ui.label("Line Width:");
                        ui.add(
                            egui::DragValue::new(&mut self.tree_painter.scale_bar_line_width)
                                .speed(0.1)
                                .range(0.5..=20.0),
                        );
                    });
                });
                self.panel_states.scale_bar_expanded = scale_state.is_open();

                let newly_opened = if !prev_panel_states.layout_expanded && self.panel_states.layout_expanded {
                    Some(ControlSection::Layout)
                } else if !prev_panel_states.trees_expanded && self.panel_states.trees_expanded {
                    Some(ControlSection::Trees)
                } else if !prev_panel_states.appearance_expanded
                    && self.panel_states.appearance_expanded
                {
                    Some(ControlSection::Appearance)
                } else if !prev_panel_states.tip_labels_expanded
                    && self.panel_states.tip_labels_expanded
                {
                    Some(ControlSection::LabelsTip)
                } else if !prev_panel_states.node_labels_expanded
                    && self.panel_states.node_labels_expanded
                {
                    Some(ControlSection::LabelsNode)
                } else if !prev_panel_states.branch_labels_expanded
                    && self.panel_states.branch_labels_expanded
                {
                    Some(ControlSection::LabelsBranch)
                } else if !prev_panel_states.tip_shapes_expanded
                    && self.panel_states.tip_shapes_expanded
                {
                    Some(ControlSection::ShapesTip)
                } else if !prev_panel_states.node_shapes_expanded
                    && self.panel_states.node_shapes_expanded
                {
                    Some(ControlSection::ShapesNode)
                } else if !prev_panel_states.node_bars_expanded
                    && self.panel_states.node_bars_expanded
                {
                    Some(ControlSection::NodeBars)
                } else if !prev_panel_states.scale_bar_expanded
                    && self.panel_states.scale_bar_expanded
                {
                    Some(ControlSection::ScaleBar)
                } else {
                    None
                };

                if let Some(section) = newly_opened {
                    self.panel_states.expand_only(section);
                }

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

        egui::TopBottomPanel::bottom("rhaetree_status").show(ctx, |ui| {
            ui.horizontal(|ui| {
                if let Some(path) = &self.config.tree_path {
                    ui.label(format!("Current file: {}", path.display()));
                }
            });
        });

        if self.load_warning_dialog.is_some() {
            let mut open = true;
            let mut close_clicked = false;
            let message = self
                .load_warning_dialog
                .as_deref()
                .unwrap_or("Failed to parse tree file.");
            egui::Window::new("Warning")
                .collapsible(false)
                .resizable(false)
                .open(&mut open)
                .anchor(egui::Align2::CENTER_CENTER, egui::vec2(0.0, 0.0))
                .show(ctx, |ui| {
                    ui.label(message);
                    ui.add_space(8.0);
                    if ui.button("OK").clicked() {
                        close_clicked = true;
                    }
                });
            if !open || close_clicked {
                self.load_warning_dialog = None;
            }
        }
    }
}
