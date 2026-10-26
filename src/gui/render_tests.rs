use super::*;
use clap::Parser;

fn app() -> (egui::Context, FigTreeGui) {
    let context = egui::Context::default();
    let cc = eframe::CreationContext::_new_kittest(context.clone());
    let path =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/annotated-sources.nex");
    let config = AppConfig::parse_from(["rhaeTree", "--headless", path.to_str().unwrap()]);
    let mut app = FigTreeGui::new(&cc, config);
    app.tree_viewer.zoom = 5.0;
    app.tree_viewer.vertical_expansion = 5.0;
    (context, app)
}

fn draw(context: &egui::Context, app: &mut FigTreeGui, offset: f32) -> egui::FullOutput {
    context.run(
        egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::Pos2::ZERO,
                egui::vec2(480.0, 360.0),
            )),
            ..Default::default()
        },
        |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                egui::ScrollArea::both()
                    .auto_shrink([false, false])
                    .vertical_scroll_offset(offset)
                    .show(ui, |ui| app.draw_tree_canvas(ui));
            });
        },
    )
}

fn wait_for_tile(context: &egui::Context, app: &mut FigTreeGui) {
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        // The test controls the idle deadline independently of frame timing.
        app.view_settle_at = None;
        draw(context, app, 0.0);
        if app.texture_viewport_origin.is_some() && !app.render_worker.is_busy() {
            return;
        }
        assert!(Instant::now() < deadline, "viewport rendering timed out");
        std::thread::sleep(Duration::from_millis(5));
    }
}

#[test]
fn expanded_tree_stays_visible_when_scrolling_beyond_a_tile() {
    let (context, mut app) = app();
    wait_for_tile(&context, &mut app);
    let preview = app.full_canvas_preview.as_ref().unwrap();
    let preview_id = preview.base.id();
    let preview_size = preview.base.size();
    let preview_hits = preview.tip_label_hits.len();
    let tile_id = app.base_tree_texture.as_ref().unwrap().id();
    assert_ne!(preview_id, tile_id);
    assert!(preview.interaction.is_some());

    for offset in [800.0, 1600.0, 2400.0, 3200.0, 1600.0, 0.0] {
        let output = draw(&context, &mut app, offset);
        assert!(app.base_tree_texture.is_some(), "blank frame at {offset}");
        assert!(app.decor_tree_texture.is_some());
        assert!(app.interaction_tree_texture.is_some());
        assert_eq!(app.texture_viewport_origin, None);
        assert_eq!(app.base_tree_texture.as_ref().unwrap().id(), preview_id);
        assert_eq!(app.cached_tip_label_hits_local.len(), preview_hits);
        assert!(output.shapes.iter().any(|shape| {
            matches!(&shape.shape, egui::Shape::Mesh(mesh) if mesh.texture_id == preview_id)
        }));
        assert_eq!(
            context.tex_manager().read().meta(preview_id).unwrap().size,
            preview_size,
            "uploading a tile must not overwrite the full preview"
        );
    }

    wait_for_tile(&context, &mut app);
    assert!(app.texture_viewport_origin.is_some());
    assert_eq!(
        app.full_canvas_preview.as_ref().unwrap().base.id(),
        preview_id
    );
}

#[test]
fn live_expansion_keeps_the_full_preview_visible() {
    let (context, mut app) = app();
    wait_for_tile(&context, &mut app);
    let preview_id = app.full_canvas_preview.as_ref().unwrap().base.id();

    app.is_live_adjusting = true;
    app.tree_viewer.vertical_expansion = 3.0;
    draw(&context, &mut app, 0.0);

    assert_eq!(app.texture_viewport_origin, None);
    assert_eq!(app.base_tree_texture.as_ref().unwrap().id(), preview_id);
    assert!(app.decor_tree_texture.is_some());
    assert!(app.interaction_tree_texture.is_some());
}

#[test]
fn expanded_full_canvas_does_not_rerender_while_scrolling() {
    let (context, mut app) = app();
    app.tree_viewer.zoom = 1.0;
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        draw(&context, &mut app, 0.0);
        if app.base_tree_texture.is_some()
            && app.interaction_tree_texture.is_some()
            && !app.render_worker.is_busy()
        {
            break;
        }
        assert!(Instant::now() < deadline, "full canvas rendering timed out");
        std::thread::sleep(Duration::from_millis(5));
    }
    let texture_id = app.base_tree_texture.as_ref().unwrap().id();
    let next_job_id = app.next_job_id;
    for offset in [400.0, 800.0, 1200.0, 800.0, 0.0] {
        draw(&context, &mut app, offset);
        assert_eq!(app.base_tree_texture.as_ref().unwrap().id(), texture_id);
        assert_eq!(app.next_job_id, next_job_id);
        assert_eq!(app.texture_viewport_origin, None);
    }
}

fn submit_tile(app: &mut FigTreeGui) {
    let empty_scene = || crate::tree::scene_graph::TreeSceneGraph {
        size: egui::vec2(2.0, 2.0),
        primitives: Vec::new(),
        tip_label_hits: Vec::new(),
    };
    let job_id = app.next_job_id;
    app.next_job_id += 1;
    assert_eq!(
        app.render_worker.try_send(RenderJob {
            base: empty_scene(),
            decor: empty_scene(),
            ppp: 1.0,
            scale: 1.0,
            job_id,
        }),
        SubmitStatus::Submitted
    );
    app.pending_job_id = Some(job_id);
    app.pending_base_sig = Some(123);
    app.pending_decor_sig = Some(456);
    app.pending_raster_size = Some(egui::vec2(2.0, 2.0));
    app.pending_viewport_origin = Some(egui::vec2(0.0, 800.0));
}

#[test]
fn preview_fallback_preserves_in_flight_tile_coordinates() {
    let (context, mut app) = app();
    wait_for_tile(&context, &mut app);
    submit_tile(&mut app);
    let job_id = app.pending_job_id;

    app.restore_full_canvas_preview();
    assert_eq!(app.pending_job_id, job_id);
    assert_eq!(app.pending_raster_size, Some(egui::vec2(2.0, 2.0)));
    assert_eq!(app.pending_viewport_origin, Some(egui::vec2(0.0, 800.0)));

    let deadline = Instant::now() + Duration::from_secs(2);
    while app.render_worker.is_busy() {
        app.receive_render_result(&context);
        assert!(Instant::now() < deadline, "render result timed out");
        std::thread::yield_now();
    }
    assert_eq!(app.texture_raster_size, Some(egui::vec2(2.0, 2.0)));
    assert_eq!(app.texture_viewport_origin, Some(egui::vec2(0.0, 800.0)));
}

#[test]
fn invalidation_discards_results_for_the_previous_tree_state() {
    let (context, mut app) = app();
    submit_tile(&mut app);
    app.invalidate_layer_caches();
    let deadline = Instant::now() + Duration::from_secs(2);
    while app.render_worker.is_busy() {
        app.receive_render_result(&context);
        assert!(Instant::now() < deadline, "render result timed out");
        std::thread::yield_now();
    }
    assert!(app.base_tree_texture.is_none());
    assert!(app.texture_raster_size.is_none());
    assert!(app.texture_viewport_origin.is_none());
    assert!(app.full_canvas_preview.is_none());
}
