use std::collections::HashSet;
use std::path::Path;

use eframe::egui::{self, Pos2, Rect};
use svg::node::element::{Line, Path as SvgPath, Polygon, Rectangle, Text};
use svg::node::Text as SvgText;
use svg::Document;

use crate::tree::layout::TreeLayout;
use crate::tree::painter::TreePainter;
use crate::tree::scene_graph::{build_tree_scene, ScenePrimitive, TreeSceneGraph};
use crate::tree::Tree;

pub fn export_svg(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    path: &Path,
    width: f32,
    height: f32,
) -> Result<(), String> {
    let scene = build_export_scene(tree, layout, painter, width, height);
    let document = scene_to_svg_document(&scene);
    svg::save(path, &document).map_err(|e| format!("Failed to save SVG: {e}"))
}

pub(crate) fn build_export_scene(
    tree: &Tree,
    layout: &TreeLayout,
    painter: &TreePainter,
    width: f32,
    height: f32,
) -> TreeSceneGraph {
    let rect = Rect::from_min_size(Pos2::ZERO, egui::vec2(width.max(1.0), height.max(1.0)));

    let margin_x = (width * 0.05).max(20.0).min(60.0);
    let margin_y = (height * 0.05).max(20.0).min(40.0);
    let margin = egui::vec2(margin_x, margin_y);
    let candidate = rect.shrink2(margin);
    let inner = if candidate.is_positive() { candidate } else { rect };

    build_tree_scene(
        tree,
        layout,
        painter,
        &HashSet::new(),
        &HashSet::new(),
        rect,
        inner,
        inner,
        1.0,
        None,
    )
}

pub(crate) fn scene_to_svg_document(scene: &TreeSceneGraph) -> Document {
    let mut document = Document::new()
        .set("viewBox", format!("0 0 {} {}", scene.size.x, scene.size.y))
        .set("width", scene.size.x)
        .set("height", scene.size.y)
        .set("xmlns", "http://www.w3.org/2000/svg");

    for primitive in &scene.primitives {
        match primitive {
            ScenePrimitive::FillRect { rect, color } => {
                let node = Rectangle::new()
                    .set("x", rect.min.x)
                    .set("y", rect.min.y)
                    .set("width", rect.width().max(0.0))
                    .set("height", rect.height().max(0.0))
                    .set("fill", color_hex(*color))
                    .set("fill-opacity", color_alpha(*color));
                document = document.add(node);
            }
            ScenePrimitive::FillCircle {
                center,
                radius,
                color,
            } => {
                let node = svg::node::element::Circle::new()
                    .set("cx", center.x)
                    .set("cy", center.y)
                    .set("r", (*radius).max(0.0))
                    .set("fill", color_hex(*color))
                    .set("fill-opacity", color_alpha(*color));
                document = document.add(node);
            }
            ScenePrimitive::FillPolygon { points, color } => {
                if points.len() < 3 {
                    continue;
                }
                let points_str = points
                    .iter()
                    .map(|p| format!("{},{}", p.x, p.y))
                    .collect::<Vec<_>>()
                    .join(" ");
                let node = Polygon::new()
                    .set("points", points_str)
                    .set("fill", color_hex(*color))
                    .set("fill-opacity", color_alpha(*color));
                document = document.add(node);
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
                );
                if points.len() < 3 {
                    continue;
                }
                let mut d = format!("M {} {}", points[0].x, points[0].y);
                for p in points.iter().skip(1) {
                    d.push_str(&format!(" L {} {}", p.x, p.y));
                }
                d.push_str(" Z");
                let node = SvgPath::new()
                    .set("d", d)
                    .set("fill", color_hex(*color))
                    .set("fill-opacity", color_alpha(*color));
                document = document.add(node);
            }
            ScenePrimitive::StrokeLine { from, to, style } => {
                let mut node = Line::new()
                    .set("x1", from.x)
                    .set("y1", from.y)
                    .set("x2", to.x)
                    .set("y2", to.y)
                    .set("stroke", color_hex(style.color))
                    .set("stroke-opacity", color_alpha(style.color))
                    .set("stroke-width", style.width)
                    .set("stroke-linecap", "round");
                if let Some((dash, gap)) = style.dash {
                    node = node.set("stroke-dasharray", format!("{} {}", dash, gap));
                }
                document = document.add(node);
            }
            ScenePrimitive::StrokePolyline { points, style } => {
                if points.len() < 2 {
                    continue;
                }
                let mut d = format!("M {} {}", points[0].x, points[0].y);
                for p in points.iter().skip(1) {
                    d.push_str(&format!(" L {} {}", p.x, p.y));
                }
                let mut node = SvgPath::new()
                    .set("d", d)
                    .set("fill", "none")
                    .set("stroke", color_hex(style.color))
                    .set("stroke-opacity", color_alpha(style.color))
                    .set("stroke-width", style.width)
                    .set("stroke-linecap", "round")
                    .set("stroke-linejoin", "round");
                if let Some((dash, gap)) = style.dash {
                    node = node.set("stroke-dasharray", format!("{} {}", dash, gap));
                }
                document = document.add(node);
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
                let mut d = format!("M {} {} L {} {}", child.x, child.y, shoulder.x, shoulder.y);
                let arc_end = Pos2::new(
                    center.x + radius * end_angle.cos(),
                    center.y + radius * end_angle.sin(),
                );
                let delta = (end_angle - start_angle).abs();
                let large_arc = if delta > std::f32::consts::PI { 1 } else { 0 };
                let sweep = if end_angle >= start_angle { 1 } else { 0 };
                d.push_str(&format!(
                    " M {} {} A {} {} 0 {} {} {} {}",
                    shoulder.x,
                    shoulder.y,
                    radius.abs(),
                    radius.abs(),
                    large_arc,
                    sweep,
                    arc_end.x,
                    arc_end.y
                ));
                let mut node = SvgPath::new()
                    .set("d", d)
                    .set("fill", "none")
                    .set("stroke", color_hex(style.color))
                    .set("stroke-opacity", color_alpha(style.color))
                    .set("stroke-width", style.width)
                    .set("stroke-linecap", "round");
                if let Some((dash, gap)) = style.dash {
                    node = node.set("stroke-dasharray", format!("{} {}", dash, gap));
                }
                document = document.add(node);
            }
            ScenePrimitive::Text {
                text,
                anchor,
                angle,
                align,
                size,
                color,
            } => {
                let anchor_mode = match *align {
                    egui::Align2::LEFT_CENTER => "start",
                    egui::Align2::RIGHT_CENTER => "end",
                    _ => "middle",
                };
                let mut node = Text::new("")
                    .set("x", anchor.x)
                    .set("y", anchor.y)
                    .set("font-size", *size)
                    .set("fill", color_hex(*color))
                    .set("fill-opacity", color_alpha(*color))
                    .set("text-anchor", anchor_mode)
                    .set("dominant-baseline", "middle");
                if angle.abs() > f32::EPSILON {
                    node = node.set(
                        "transform",
                        format!(
                            "rotate({} {} {})",
                            angle.to_degrees(),
                            anchor.x,
                            anchor.y
                        ),
                    );
                }
                node = node.add(SvgText::new(text.clone()));
                document = document.add(node);
            }
        }
    }

    document
}

fn color_hex(color: egui::Color32) -> String {
    format!("#{:02X}{:02X}{:02X}", color.r(), color.g(), color.b())
}

fn color_alpha(color: egui::Color32) -> f32 {
    color.a() as f32 / 255.0
}

fn annular_sector_points(
    center: Pos2,
    inner_radius: f32,
    outer_radius: f32,
    start_angle: f32,
    end_angle: f32,
) -> Vec<Pos2> {
    let outer = arc_points(center, outer_radius, start_angle, end_angle);
    let mut inner = arc_points(center, inner_radius, end_angle, start_angle);
    let mut points = Vec::with_capacity(outer.len() + inner.len());
    points.extend(outer);
    points.append(&mut inner);
    points
}

fn arc_points(center: Pos2, radius: f32, start_angle: f32, end_angle: f32) -> Vec<Pos2> {
    let delta = end_angle - start_angle;
    let steps = ((delta.abs() * radius.abs() / 2.0).ceil() as usize).clamp(12, 320);
    let mut points = Vec::with_capacity(steps + 1);
    for i in 0..=steps {
        let t = i as f32 / steps as f32;
        let a = start_angle + delta * t;
        points.push(Pos2::new(
            center.x + radius * a.cos(),
            center.y + radius * a.sin(),
        ));
    }
    points
}
