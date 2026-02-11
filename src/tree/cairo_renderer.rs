use cairo::{Context, FontSlant, FontWeight, LineCap};
use eframe::egui::{self, Color32, Pos2};

use crate::tree::scene_graph::{ScenePrimitive, TreeSceneGraph};

pub fn render_scene_to_context(cr: &Context, scene: &TreeSceneGraph, ppp: f32) {
    for primitive in &scene.primitives {
        match primitive {
            ScenePrimitive::FillRect { rect, color } => {
                set_source_color(cr, *color);
                cr.rectangle(
                    (rect.min.x * ppp) as f64,
                    (rect.min.y * ppp) as f64,
                    (rect.width().max(0.0) * ppp) as f64,
                    (rect.height().max(0.0) * ppp) as f64,
                );
                let _ = cr.fill();
            }
            ScenePrimitive::FillCircle {
                center,
                radius,
                color,
            } => {
                set_source_color(cr, *color);
                cr.arc(
                    (center.x * ppp) as f64,
                    (center.y * ppp) as f64,
                    (radius * ppp).max(0.5) as f64,
                    0.0,
                    std::f64::consts::TAU,
                );
                let _ = cr.fill();
            }
            ScenePrimitive::FillPolygon { points, color } => {
                if points.len() < 3 {
                    continue;
                }
                set_source_color(cr, *color);
                cr.move_to((points[0].x * ppp) as f64, (points[0].y * ppp) as f64);
                for p in points.iter().skip(1) {
                    cr.line_to((p.x * ppp) as f64, (p.y * ppp) as f64);
                }
                cr.close_path();
                let _ = cr.fill();
            }
            ScenePrimitive::FillSector {
                center,
                inner_radius,
                outer_radius,
                start_angle,
                end_angle,
                color,
            } => {
                set_source_color(cr, *color);
                let cx = center.x * ppp;
                let cy = center.y * ppp;
                cr.new_path();
                cr.arc(
                    cx as f64,
                    cy as f64,
                    (outer_radius * ppp).max(0.0) as f64,
                    *start_angle as f64,
                    *end_angle as f64,
                );
                if end_angle >= start_angle {
                    cr.arc_negative(
                        cx as f64,
                        cy as f64,
                        (inner_radius * ppp).max(0.0) as f64,
                        *end_angle as f64,
                        *start_angle as f64,
                    );
                } else {
                    cr.arc(
                        cx as f64,
                        cy as f64,
                        (inner_radius * ppp).max(0.0) as f64,
                        *end_angle as f64,
                        *start_angle as f64,
                    );
                }
                cr.close_path();
                let _ = cr.fill();
            }
            ScenePrimitive::StrokeLine { from, to, style } => {
                set_source_color(cr, style.color);
                cr.set_line_cap(LineCap::Round);
                cr.set_line_width((style.width * ppp).max(1.0) as f64);
                if let Some((dash, gap)) = style.dash {
                    cr.set_dash(&[(dash * ppp) as f64, (gap * ppp) as f64], 0.0);
                } else {
                    cr.set_dash(&[], 0.0);
                }
                cr.move_to((from.x * ppp) as f64, (from.y * ppp) as f64);
                cr.line_to((to.x * ppp) as f64, (to.y * ppp) as f64);
                let _ = cr.stroke();
                cr.set_dash(&[], 0.0);
            }
            ScenePrimitive::StrokePolyline { points, style } => {
                if points.len() < 2 {
                    continue;
                }
                set_source_color(cr, style.color);
                cr.set_line_cap(LineCap::Round);
                cr.set_line_width((style.width * ppp).max(1.0) as f64);
                if let Some((dash, gap)) = style.dash {
                    cr.set_dash(&[(dash * ppp) as f64, (gap * ppp) as f64], 0.0);
                } else {
                    cr.set_dash(&[], 0.0);
                }
                cr.move_to((points[0].x * ppp) as f64, (points[0].y * ppp) as f64);
                for p in points.iter().skip(1) {
                    cr.line_to((p.x * ppp) as f64, (p.y * ppp) as f64);
                }
                let _ = cr.stroke();
                cr.set_dash(&[], 0.0);
            }
            ScenePrimitive::StrokeCircularBranch {
                child,
                center,
                radius,
                start_angle,
                end_angle,
                style,
            } => {
                set_source_color(cr, style.color);
                cr.set_line_cap(LineCap::Round);
                cr.set_line_width((style.width * ppp).max(1.0) as f64);
                let shoulder = Pos2::new(
                    center.x + radius * start_angle.cos(),
                    center.y + radius * start_angle.sin(),
                );
                cr.move_to((child.x * ppp) as f64, (child.y * ppp) as f64);
                cr.line_to((shoulder.x * ppp) as f64, (shoulder.y * ppp) as f64);
                let _ = cr.stroke();
                cr.new_sub_path();
                if end_angle >= start_angle {
                    cr.arc(
                        (center.x * ppp) as f64,
                        (center.y * ppp) as f64,
                        (radius * ppp).max(0.5) as f64,
                        *start_angle as f64,
                        *end_angle as f64,
                    );
                } else {
                    cr.arc_negative(
                        (center.x * ppp) as f64,
                        (center.y * ppp) as f64,
                        (radius * ppp).max(0.5) as f64,
                        *start_angle as f64,
                        *end_angle as f64,
                    );
                }
                let _ = cr.stroke();
            }
            ScenePrimitive::Text {
                text,
                anchor,
                angle,
                align,
                size,
                color,
            } => {
                draw_text(cr, text, *anchor, *angle, *align, *size, *color, ppp);
            }
        }
    }
}

fn draw_text(
    cr: &Context,
    text: &str,
    anchor: Pos2,
    angle: f32,
    align: egui::Align2,
    size: f32,
    color: Color32,
    ppp: f32,
) {
    if text.is_empty() {
        return;
    }
    cr.save().ok();
    set_source_color(cr, color);
    cr.select_font_face("Sans", FontSlant::Normal, FontWeight::Normal);
    cr.set_font_size((size * ppp).max(6.0) as f64);
    cr.translate((anchor.x * ppp) as f64, (anchor.y * ppp) as f64);
    cr.rotate(angle as f64);
    let ext = match cr.text_extents(text) {
        Ok(e) => e,
        Err(_) => {
            cr.restore().ok();
            return;
        }
    };
    let y = -(ext.y_bearing() + ext.height() * 0.5);
    let x = match align {
        egui::Align2::LEFT_CENTER => -ext.x_bearing(),
        egui::Align2::RIGHT_CENTER => -(ext.width() + ext.x_bearing()),
        _ => -(ext.width() * 0.5 + ext.x_bearing()),
    };
    cr.move_to(x, y);
    let _ = cr.show_text(text);
    cr.restore().ok();
}

fn set_source_color(cr: &Context, color: Color32) {
    cr.set_source_rgba(
        color.r() as f64 / 255.0,
        color.g() as f64 / 255.0,
        color.b() as f64 / 255.0,
        color.a() as f64 / 255.0,
    );
}
