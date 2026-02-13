use std::collections::HashSet;

use eframe::egui::{self, Align2, Color32, Pos2, Rect};
use font_kit::family_name::FamilyName;
use font_kit::properties::Properties;
use font_kit::source::SystemSource;
use fontdue::layout::{CoordinateSystem, Layout, LayoutSettings, TextStyle};
use fontdue::{Font, FontSettings};
use tiny_skia::{
    Color, FillRule, LineCap, Paint, PathBuilder, Pixmap, Stroke, StrokeDash, Transform,
};

use crate::tree::layout::TreeLayout;
use crate::tree::painter::{TipLabelHit, TreePainter};
use crate::tree::scene_graph::{build_tree_scene, SceneLayer, ScenePrimitive, TreeSceneGraph};
use crate::tree::viewer::SelectionMode;
use crate::tree::{NodeId, Tree};

pub struct SkiaRenderOutput {
    pub image: egui::ColorImage,
    pub tip_label_hits: Vec<TipLabelHit>,
}

pub struct SkiaTreeRenderer {
    text_font: Option<Font>,
}

impl Default for SkiaTreeRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl SkiaTreeRenderer {
    pub fn new() -> Self {
        Self {
            text_font: load_system_sans_font(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn render(
        &mut self,
        tree: &Tree,
        layout: &TreeLayout,
        painter: &TreePainter,
        selected_nodes: &HashSet<NodeId>,
        selected_tips: &HashSet<NodeId>,
        rect: Rect,
        canvas_inner: Rect,
        transform_inner: Rect,
        stroke_scale: f32,
        selection_mode: Option<SelectionMode>,
        scene_layer: SceneLayer,
        pixels_per_point: f32,
        resolution_scale: f32,
    ) -> Result<SkiaRenderOutput, String> {
        let scene = build_tree_scene(
            tree,
            layout,
            painter,
            selected_nodes,
            selected_tips,
            rect,
            canvas_inner,
            transform_inner,
            stroke_scale,
            selection_mode,
            scene_layer,
        );
        self.render_scene_to_image(&scene, pixels_per_point, resolution_scale)
    }

    pub fn render_scene_to_image(
        &mut self,
        scene: &TreeSceneGraph,
        pixels_per_point: f32,
        resolution_scale: f32,
    ) -> Result<SkiaRenderOutput, String> {
        let ppp = (pixels_per_point.max(1.0) * resolution_scale.clamp(0.05, 1.0)).max(0.05);
        let width_px = ((scene.size.x * ppp).round().max(1.0)) as u32;
        let height_px = ((scene.size.y * ppp).round().max(1.0)) as u32;

        let mut pixmap = Pixmap::new(width_px, height_px)
            .ok_or_else(|| "Failed to create Skia pixmap".to_string())?;

        for primitive in &scene.primitives {
            render_primitive(
                &mut pixmap,
                primitive,
                ppp,
                self.text_font.as_ref(),
                width_px,
                height_px,
            );
        }

        let rgba = premultiplied_rgba_to_unmultiplied(pixmap.data());
        Ok(SkiaRenderOutput {
            image: egui::ColorImage::from_rgba_unmultiplied(
                [width_px as usize, height_px as usize],
                &rgba,
            ),
            tip_label_hits: scene.tip_label_hits.clone(),
        })
    }
}

fn load_system_sans_font() -> Option<Font> {
    let source = SystemSource::new();
    let handle = source
        .select_best_match(&[FamilyName::SansSerif], &Properties::new())
        .ok()?;
    let font = handle.load().ok()?;
    let font_data = font.copy_font_data()?;
    Font::from_bytes(font_data.as_ref().clone(), FontSettings::default()).ok()
}

fn render_primitive(
    pixmap: &mut Pixmap,
    primitive: &ScenePrimitive,
    ppp: f32,
    text_font: Option<&Font>,
    width_px: u32,
    _height_px: u32,
) {
    match primitive {
        ScenePrimitive::FillRect { rect, color } => {
            let Some(ts_rect) = tiny_skia::Rect::from_xywh(
                rect.min.x * ppp,
                rect.min.y * ppp,
                (rect.width() * ppp).max(0.0),
                (rect.height() * ppp).max(0.0),
            ) else {
                return;
            };
            let mut paint = Paint::default();
            paint.set_color(to_skia_color(*color));
            pixmap.fill_rect(ts_rect, &paint, Transform::identity(), None);
        }
        ScenePrimitive::FillCircle {
            center,
            radius,
            color,
        } => {
            let mut pb = PathBuilder::new();
            pb.push_circle(center.x * ppp, center.y * ppp, (radius * ppp).max(0.5));
            if let Some(path) = pb.finish() {
                let mut paint = Paint::default();
                paint.set_color(to_skia_color(*color));
                pixmap.fill_path(
                    &path,
                    &paint,
                    FillRule::Winding,
                    Transform::identity(),
                    None,
                );
            }
        }
        ScenePrimitive::FillPolygon { points, color } => {
            if points.len() < 3 {
                return;
            }
            let mut pb = PathBuilder::new();
            pb.move_to(points[0].x * ppp, points[0].y * ppp);
            for p in points.iter().skip(1) {
                pb.line_to(p.x * ppp, p.y * ppp);
            }
            pb.close();
            if let Some(path) = pb.finish() {
                let mut paint = Paint::default();
                paint.set_color(to_skia_color(*color));
                pixmap.fill_path(
                    &path,
                    &paint,
                    FillRule::Winding,
                    Transform::identity(),
                    None,
                );
            }
        }
        ScenePrimitive::FillSector {
            center,
            inner_radius,
            outer_radius,
            start_angle,
            end_angle,
            color,
        } => {
            let points = annular_sector_points(
                *center,
                *inner_radius,
                *outer_radius,
                *start_angle,
                *end_angle,
                ppp,
            );
            if points.len() < 3 {
                return;
            }
            let mut pb = PathBuilder::new();
            pb.move_to(points[0].x, points[0].y);
            for p in points.iter().skip(1) {
                pb.line_to(p.x, p.y);
            }
            pb.close();
            if let Some(path) = pb.finish() {
                let mut paint = Paint::default();
                paint.set_color(to_skia_color(*color));
                pixmap.fill_path(
                    &path,
                    &paint,
                    FillRule::Winding,
                    Transform::identity(),
                    None,
                );
            }
        }
        ScenePrimitive::StrokeLine { from, to, style } => {
            let mut pb = PathBuilder::new();
            pb.move_to(from.x * ppp, from.y * ppp);
            pb.line_to(to.x * ppp, to.y * ppp);
            if let Some(path) = pb.finish() {
                stroke_path(pixmap, &path, style, ppp);
            }
        }
        ScenePrimitive::StrokePolyline { points, style } => {
            if points.len() < 2 {
                return;
            }
            let mut pb = PathBuilder::new();
            pb.move_to(points[0].x * ppp, points[0].y * ppp);
            for p in points.iter().skip(1) {
                pb.line_to(p.x * ppp, p.y * ppp);
            }
            if let Some(path) = pb.finish() {
                stroke_path(pixmap, &path, style, ppp);
            }
        }
        ScenePrimitive::StrokeCircularBranch {
            child,
            center,
            radius,
            start_angle,
            end_angle,
            style,
        } => {
            let shoulder = Pos2::new(
                center.x + radius * start_angle.cos(),
                center.y + radius * start_angle.sin(),
            );
            let mut pb = PathBuilder::new();
            pb.move_to(child.x * ppp, child.y * ppp);
            pb.line_to(shoulder.x * ppp, shoulder.y * ppp);
            if let Some(path) = pb.finish() {
                stroke_path(pixmap, &path, style, ppp);
            }

            let arc_points = arc_points(*center, *radius, *start_angle, *end_angle, ppp);
            if arc_points.len() < 2 {
                return;
            }
            let mut pb = PathBuilder::new();
            pb.move_to(arc_points[0].x, arc_points[0].y);
            for p in arc_points.iter().skip(1) {
                pb.line_to(p.x, p.y);
            }
            if let Some(path) = pb.finish() {
                stroke_path(pixmap, &path, style, ppp);
            }
        }
        ScenePrimitive::Text {
            text,
            anchor,
            angle,
            align,
            size,
            color,
        } => {
            let Some(font) = text_font else {
                return;
            };
            render_text(
                pixmap, font, text, *anchor, *angle, *align, *size, *color, ppp, width_px,
            );
        }
    }
}

fn stroke_path(
    pixmap: &mut Pixmap,
    path: &tiny_skia::Path,
    style: &crate::tree::scene_graph::StrokeStyle,
    ppp: f32,
) {
    let mut paint = Paint::default();
    paint.set_color(to_skia_color(style.color));

    let mut stroke = Stroke::default();
    stroke.width = (style.width * ppp).max(1.0);
    stroke.line_cap = LineCap::Round;
    if let Some((dash, gap)) = style.dash {
        stroke.dash = StrokeDash::new(vec![(dash * ppp).max(1.0), (gap * ppp).max(1.0)], 0.0);
    }

    pixmap.stroke_path(path, &paint, &stroke, Transform::identity(), None);
}

fn to_skia_color(color: Color32) -> Color {
    Color::from_rgba8(color.r(), color.g(), color.b(), color.a())
}

fn arc_points(center: Pos2, radius: f32, start_angle: f32, end_angle: f32, ppp: f32) -> Vec<Pos2> {
    let radius_px = (radius * ppp).abs();
    let delta = end_angle - start_angle;
    let steps = ((delta.abs() * radius_px / 2.0).ceil() as usize).clamp(12, 320);

    let mut points = Vec::with_capacity(steps + 1);
    for i in 0..=steps {
        let t = i as f32 / steps as f32;
        let a = start_angle + delta * t;
        points.push(Pos2::new(
            (center.x + radius * a.cos()) * ppp,
            (center.y + radius * a.sin()) * ppp,
        ));
    }
    points
}

fn annular_sector_points(
    center: Pos2,
    inner_radius: f32,
    outer_radius: f32,
    start_angle: f32,
    end_angle: f32,
    ppp: f32,
) -> Vec<Pos2> {
    let outer = arc_points(center, outer_radius, start_angle, end_angle, ppp);
    let mut inner = arc_points(center, inner_radius, end_angle, start_angle, ppp);
    let mut points = Vec::with_capacity(outer.len() + inner.len());
    points.extend(outer);
    points.append(&mut inner);
    points
}

#[allow(clippy::too_many_arguments)]
fn render_text(
    pixmap: &mut Pixmap,
    font: &Font,
    text: &str,
    anchor: Pos2,
    angle: f32,
    align: Align2,
    size: f32,
    color: Color32,
    ppp: f32,
    width_px: u32,
) {
    if text.is_empty() {
        return;
    }

    let px = (size * ppp).max(6.0);
    let mut layout = Layout::new(CoordinateSystem::PositiveYDown);
    layout.reset(&LayoutSettings::default());
    layout.append(&[font], &TextStyle::new(text, px, 0));

    let glyphs = layout.glyphs();
    if glyphs.is_empty() {
        return;
    }

    let mut min_x = f32::INFINITY;
    let mut min_y = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut max_y = f32::NEG_INFINITY;
    for g in glyphs {
        if g.width == 0 || g.height == 0 {
            continue;
        }
        min_x = min_x.min(g.x);
        min_y = min_y.min(g.y);
        max_x = max_x.max(g.x + g.width as f32);
        max_y = max_y.max(g.y + g.height as f32);
    }

    if !min_x.is_finite() || !min_y.is_finite() || !max_x.is_finite() || !max_y.is_finite() {
        return;
    }

    let text_w = (max_x - min_x).max(1.0);
    let text_h = (max_y - min_y).max(1.0);
    let anchor_local = match align {
        Align2::LEFT_CENTER => (0.0_f32, text_h * 0.5),
        Align2::RIGHT_CENTER => (text_w, text_h * 0.5),
        _ => (text_w * 0.5, text_h * 0.5),
    };

    let ax = anchor.x * ppp;
    let ay = anchor.y * ppp;
    let cos_a = angle.cos();
    let sin_a = angle.sin();

    let data = pixmap.data_mut();

    for g in layout.glyphs() {
        if g.width == 0 || g.height == 0 {
            continue;
        }

        let (metrics, bitmap) = font.rasterize_config(g.key);
        if metrics.width == 0 || metrics.height == 0 {
            continue;
        }

        let gx = g.x - min_x;
        let gy = g.y - min_y;

        for y in 0..metrics.height {
            for x in 0..metrics.width {
                let cov = bitmap[y * metrics.width + x] as f32 / 255.0;
                if cov <= 0.0 {
                    continue;
                }

                // Sample at pixel center for better rotation stability.
                let local_x = gx + x as f32 + 0.5;
                let local_y = gy + y as f32 + 0.5;

                let dx = local_x - anchor_local.0;
                let dy = local_y - anchor_local.1;
                let world_x = ax + dx * cos_a - dy * sin_a;
                let world_y = ay + dx * sin_a + dy * cos_a;

                let src_a = (color.a() as f32 / 255.0) * cov;
                if src_a <= 0.0 {
                    continue;
                }

                // Bilinear splat to avoid pinholes on rotated glyph strokes.
                let x0 = world_x.floor();
                let y0 = world_y.floor();
                let fx = world_x - x0;
                let fy = world_y - y0;

                let x0i = x0 as i32;
                let y0i = y0 as i32;

                blend_pixel_premultiplied(
                    data,
                    width_px as usize,
                    x0i,
                    y0i,
                    color,
                    src_a * (1.0 - fx) * (1.0 - fy),
                );
                blend_pixel_premultiplied(
                    data,
                    width_px as usize,
                    x0i + 1,
                    y0i,
                    color,
                    src_a * fx * (1.0 - fy),
                );
                blend_pixel_premultiplied(
                    data,
                    width_px as usize,
                    x0i,
                    y0i + 1,
                    color,
                    src_a * (1.0 - fx) * fy,
                );
                blend_pixel_premultiplied(
                    data,
                    width_px as usize,
                    x0i + 1,
                    y0i + 1,
                    color,
                    src_a * fx * fy,
                );
            }
        }
    }
}

fn blend_pixel_premultiplied(
    data: &mut [u8],
    width: usize,
    x: i32,
    y: i32,
    color: Color32,
    src_a: f32,
) {
    if src_a <= 0.0 {
        return;
    }
    if x < 0 || y < 0 {
        return;
    }
    let xu = x as usize;
    let yu = y as usize;
    let height = data.len() / (width * 4);
    if xu >= width || yu >= height {
        return;
    }
    let idx = (yu * width + xu) * 4;

    let dst_r = data[idx] as f32 / 255.0;
    let dst_g = data[idx + 1] as f32 / 255.0;
    let dst_b = data[idx + 2] as f32 / 255.0;
    let dst_a = data[idx + 3] as f32 / 255.0;

    let src_r = (color.r() as f32 / 255.0) * src_a;
    let src_g = (color.g() as f32 / 255.0) * src_a;
    let src_b = (color.b() as f32 / 255.0) * src_a;

    let out_a = src_a + dst_a * (1.0 - src_a);
    let out_r = src_r + dst_r * (1.0 - src_a);
    let out_g = src_g + dst_g * (1.0 - src_a);
    let out_b = src_b + dst_b * (1.0 - src_a);

    data[idx] = (out_r.clamp(0.0, 1.0) * 255.0).round() as u8;
    data[idx + 1] = (out_g.clamp(0.0, 1.0) * 255.0).round() as u8;
    data[idx + 2] = (out_b.clamp(0.0, 1.0) * 255.0).round() as u8;
    data[idx + 3] = (out_a.clamp(0.0, 1.0) * 255.0).round() as u8;
}

fn premultiplied_rgba_to_unmultiplied(data: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(data.len());
    for rgba in data.chunks_exact(4) {
        let r = rgba[0] as u32;
        let g = rgba[1] as u32;
        let b = rgba[2] as u32;
        let a = rgba[3] as u32;

        if a == 0 {
            out.extend_from_slice(&[0, 0, 0, 0]);
            continue;
        }

        let ur = ((r * 255 + a / 2) / a).min(255) as u8;
        let ug = ((g * 255 + a / 2) / a).min(255) as u8;
        let ub = ((b * 255 + a / 2) / a).min(255) as u8;
        out.extend_from_slice(&[ur, ug, ub, a as u8]);
    }
    out
}
