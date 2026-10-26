use super::*;
use crate::tree::attributes::InternalLabelMode;
use clap::Parser;

fn app() -> (egui::Context, FigTreeGui) {
    let context = egui::Context::default();
    let cc = eframe::CreationContext::_new_kittest(context.clone());
    let path =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/annotated-sources.nex");
    let config = AppConfig::parse_from(["rhaeTree", "--headless", path.to_str().unwrap()]);
    (context, FigTreeGui::new(&cc, config))
}

#[test]
fn attribute_edit_undo_redo_and_gui_session_settings_round_trip() {
    let (_, mut app) = app();
    let node_id = app.tree_viewer.current_tree().unwrap().external_nodes()[0].id;
    app.record_undo_step();
    app.tree_viewer
        .set_node_attribute(node_id, "host", "edited");
    app.tree_painter.branch_colour_mapping.attribute = Some("host".into());
    app.tree_painter.tip_label_display = TipLabelDisplay::Attribute;
    app.tree_painter.tip_label_attribute = Some("host".into());
    app.tree_painter.branch_label_display = BranchLabelDisplay::Attribute;
    app.tree_painter.branch_label_attribute = Some("!length".into());
    let settings = app.collect_rtr_settings();
    app.undo_last_edit();
    assert!(app.tree_painter.branch_colour_mapping.attribute.is_none());
    assert_ne!(
        app.tree_viewer.current_tree().unwrap().nodes[node_id]
            .get_attribute("host")
            .map(String::as_str),
        Some("edited")
    );
    app.redo_last_edit();
    assert_eq!(
        app.tree_viewer.current_tree().unwrap().nodes[node_id]
            .get_attribute("host")
            .map(String::as_str),
        Some("edited")
    );
    let (_, mut restored) = self::app();
    restored.apply_rtr_settings(&settings);
    assert_eq!(
        restored.tree_painter.branch_colour_mapping,
        app.tree_painter.branch_colour_mapping
    );
    assert_eq!(
        restored.tree_painter.tip_label_attribute.as_deref(),
        Some("host")
    );
    assert_eq!(
        restored.tree_painter.branch_label_attribute.as_deref(),
        Some("!length")
    );
    assert_eq!(
        restored.tree_painter.tip_label_display,
        TipLabelDisplay::Attribute
    );
    assert_eq!(
        restored.tree_painter.branch_label_display,
        BranchLabelDisplay::Attribute
    );
}

#[test]
fn attribute_panels_render_with_missing_fields_and_cache_tracks_edits() {
    let (context, mut app) = app();
    app.tree_painter.branch_colour_mapping.attribute = Some("posterior".into());
    app.tree_viewer.show_tree(1);
    app.tree_viewer
        .set_internal_label_mode(InternalLabelMode::IqTreeAlrtUfboot);
    let before =
        app.base_render_signature(app.tree_viewer.current_tree().unwrap(), 640.0, 480.0, None);
    let id = app.tree_viewer.current_tree().unwrap().external_nodes()[0].id;
    app.tree_viewer.select_tip_label(id);
    let output = context.run(egui::RawInput::default(), |ctx| {
        egui::CentralPanel::default().show(ctx, |ui| app.attribute_controls(ui));
    });
    assert!(!output.shapes.is_empty());
    assert_eq!(
        app.tree_painter.branch_colour_mapping.attribute.as_deref(),
        Some("posterior")
    );
    app.tree_viewer.set_node_attribute(id, "posterior", "0.9");
    let after =
        app.base_render_signature(app.tree_viewer.current_tree().unwrap(), 640.0, 480.0, None);
    assert_ne!(before, after);
    app.tree_painter.branch_colour_mapping.high = Color32::GREEN;
    let palette =
        app.base_render_signature(app.tree_viewer.current_tree().unwrap(), 640.0, 480.0, None);
    assert_ne!(after, palette);
    app.panel_states.appearance_expanded = true;
    app.panel_states.node_labels_expanded = true;
    let mut frame = eframe::Frame::_new_kittest();
    let output = context.run(
        egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::Pos2::ZERO,
                egui::vec2(1200.0, 900.0),
            )),
            ..Default::default()
        },
        |ctx| eframe::App::update(&mut app, ctx, &mut frame),
    );
    assert!(!output.shapes.is_empty());
}
