use std::collections::HashSet;
use std::sync::mpsc;

use eframe::egui::{self, Align2, Color32, Pos2, Rect};
use font_kit::family_name::FamilyName;
use font_kit::properties::Properties;
use font_kit::source::SystemSource;
use fontdue::layout::{CoordinateSystem, Layout, LayoutSettings, TextStyle};
use fontdue::{Font, FontSettings};
use vello::kurbo::{Affine, BezPath, Circle, Rect as KRect, Stroke as KStroke};
use vello::peniko::{Blob, Color as VelloColor, Fill, FontData};
use vello::{AaConfig, Glyph, RenderParams, Renderer, RendererOptions, Scene};

use crate::tree::layout::TreeLayout;
use crate::tree::painter::TreePainter;
use crate::tree::scene_graph::{build_tree_scene, SceneLayer, ScenePrimitive, TreeSceneGraph};
use crate::tree::skia_renderer::SkiaRenderOutput;
use crate::tree::viewer::SelectionMode;
use crate::tree::{NodeId, Tree};

pub struct VelloTreeRenderer {
    gpu: Option<VelloGpuState>,
    text_font: Option<Font>,
    text_font_data: Option<FontData>,
}

struct VelloGpuState {
    device: wgpu::Device,
    queue: wgpu::Queue,
    renderer: Renderer,
    target_texture: Option<wgpu::Texture>,
    target_view: Option<wgpu::TextureView>,
    readback_buffer: Option<wgpu::Buffer>,
    target_size: (u32, u32),
    padded_bytes_per_row: u32,
    readback_size: u64,
}

impl Default for VelloTreeRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl VelloTreeRenderer {
    pub fn new() -> Self {
        let (text_font, text_font_data) = load_system_sans_font_bundle()
            .map_or((None, None), |(font, data)| (Some(font), Some(data)));
        Self {
            gpu: None,
            text_font,
            text_font_data,
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
        let text_font = self.text_font.clone();
        let text_font_data = self.text_font_data.clone();

        let gpu = self.ensure_gpu()?;
        gpu.ensure_targets(width_px, height_px);

        let mut vello_scene = Scene::new();
        for primitive in &scene.primitives {
            match primitive {
                ScenePrimitive::Text {
                    text,
                    anchor,
                    angle,
                    align,
                    size,
                    color,
                } => {
                    if let (Some(font), Some(font_data)) =
                        (text_font.as_ref(), text_font_data.as_ref())
                    {
                        encode_text_primitive(
                            &mut vello_scene,
                            font,
                            font_data,
                            text,
                            *anchor,
                            *angle,
                            *align,
                            *size,
                            *color,
                            ppp,
                        );
                    }
                }
                _ => encode_primitive(&mut vello_scene, primitive, ppp),
            }
        }

        let target_view = gpu
            .target_view
            .as_ref()
            .ok_or_else(|| "Vello target view unavailable".to_string())?;

        gpu.renderer
            .render_to_texture(
                &gpu.device,
                &gpu.queue,
                &vello_scene,
                target_view,
                &RenderParams {
                    base_color: VelloColor::from_rgba8(0, 0, 0, 0),
                    width: width_px,
                    height: height_px,
                    antialiasing_method: AaConfig::Area,
                },
            )
            .map_err(|err| format!("Vello render_to_texture failed: {err}"))?;

        gpu.copy_texture_to_readback(width_px, height_px);
        let rgba_premul = gpu.map_readback_rgba(width_px, height_px)?;
        let rgba = premultiplied_rgba_to_unmultiplied(&rgba_premul);

        Ok(SkiaRenderOutput {
            image: egui::ColorImage::from_rgba_unmultiplied(
                [width_px as usize, height_px as usize],
                &rgba,
            ),
            tip_label_hits: scene.tip_label_hits.clone(),
        })
    }

    fn ensure_gpu(&mut self) -> Result<&mut VelloGpuState, String> {
        if self.gpu.is_none() {
            let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor::default());
            let adapter = pollster::block_on(wgpu::util::initialize_adapter_from_env_or_default(
                &instance, None,
            ))
            .map_err(|err| format!("Failed to request wgpu adapter for vello: {err}"))?;
            let features = adapter.features();
            let desired_features = wgpu::Features::CLEAR_TEXTURE | wgpu::Features::PIPELINE_CACHE;
            let (device, queue) =
                pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
                    label: Some("figtree_vello_device"),
                    required_features: features & desired_features,
                    required_limits: wgpu::Limits::default(),
                    ..Default::default()
                }))
                .map_err(|err| format!("Failed to create wgpu device for vello: {err}"))?;

            let renderer = Renderer::new(&device, RendererOptions::default())
                .map_err(|err| format!("{err}"))?;

            self.gpu = Some(VelloGpuState {
                device,
                queue,
                renderer,
                target_texture: None,
                target_view: None,
                readback_buffer: None,
                target_size: (0, 0),
                padded_bytes_per_row: 0,
                readback_size: 0,
            });
        }
        self.gpu
            .as_mut()
            .ok_or_else(|| "Vello GPU state is unavailable".to_string())
    }
}

impl VelloGpuState {
    fn ensure_targets(&mut self, width: u32, height: u32) {
        if self.target_size == (width, height) && self.target_texture.is_some() {
            return;
        }

        let texture = self.device.create_texture(&wgpu::TextureDescriptor {
            label: Some("figtree_vello_target"),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8Unorm,
            usage: wgpu::TextureUsages::STORAGE_BINDING | wgpu::TextureUsages::COPY_SRC,
            view_formats: &[],
        });
        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        let unpadded_bytes_per_row = width * 4;
        let padded_bytes_per_row = unpadded_bytes_per_row
            .div_ceil(wgpu::COPY_BYTES_PER_ROW_ALIGNMENT)
            * wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;
        let readback_size = padded_bytes_per_row as u64 * height as u64;

        let readback_buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("figtree_vello_readback"),
            size: readback_size,
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
            mapped_at_creation: false,
        });

        self.target_texture = Some(texture);
        self.target_view = Some(view);
        self.readback_buffer = Some(readback_buffer);
        self.target_size = (width, height);
        self.padded_bytes_per_row = padded_bytes_per_row;
        self.readback_size = readback_size;
    }

    fn copy_texture_to_readback(&self, width: u32, height: u32) {
        let texture = self
            .target_texture
            .as_ref()
            .expect("target texture should exist after ensure_targets");
        let readback_buffer = self
            .readback_buffer
            .as_ref()
            .expect("readback buffer should exist after ensure_targets");

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("figtree_vello_readback_encoder"),
            });

        encoder.copy_texture_to_buffer(
            wgpu::TexelCopyTextureInfo {
                texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            wgpu::TexelCopyBufferInfo {
                buffer: readback_buffer,
                layout: wgpu::TexelCopyBufferLayout {
                    offset: 0,
                    bytes_per_row: Some(self.padded_bytes_per_row),
                    rows_per_image: Some(height),
                },
            },
            wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
        );

        self.queue.submit(Some(encoder.finish()));
    }

    fn map_readback_rgba(&self, width: u32, height: u32) -> Result<Vec<u8>, String> {
        let readback_buffer = self
            .readback_buffer
            .as_ref()
            .ok_or_else(|| "Vello readback buffer unavailable".to_string())?;
        let slice = readback_buffer.slice(0..self.readback_size);
        let (tx, rx) = mpsc::channel();
        slice.map_async(wgpu::MapMode::Read, move |result| {
            let _ = tx.send(result);
        });

        self.device
            .poll(wgpu::PollType::wait_indefinitely())
            .map_err(|err| format!("wgpu poll failed while waiting readback: {err}"))?;

        rx.recv()
            .map_err(|err| format!("Failed to receive readback map result: {err}"))?
            .map_err(|err| format!("Failed to map Vello readback buffer: {err}"))?;

        let mapped = slice.get_mapped_range();
        let unpadded_bytes_per_row = (width * 4) as usize;
        let mut rgba = vec![0_u8; unpadded_bytes_per_row * height as usize];
        for row in 0..height as usize {
            let src_start = row * self.padded_bytes_per_row as usize;
            let src_end = src_start + unpadded_bytes_per_row;
            let dst_start = row * unpadded_bytes_per_row;
            let dst_end = dst_start + unpadded_bytes_per_row;
            rgba[dst_start..dst_end].copy_from_slice(&mapped[src_start..src_end]);
        }
        drop(mapped);
        readback_buffer.unmap();

        Ok(rgba)
    }
}

fn encode_primitive(scene: &mut Scene, primitive: &ScenePrimitive, ppp: f32) {
    match primitive {
        ScenePrimitive::FillRect { rect, color } => {
            let krect = KRect::new(
                (rect.min.x * ppp) as f64,
                (rect.min.y * ppp) as f64,
                (rect.max.x * ppp) as f64,
                (rect.max.y * ppp) as f64,
            );
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                to_vello_color(*color),
                None,
                &krect,
            );
        }
        ScenePrimitive::FillCircle {
            center,
            radius,
            color,
        } => {
            let circle = Circle::new((center.x * ppp, center.y * ppp), (radius * ppp) as f64);
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                to_vello_color(*color),
                None,
                &circle,
            );
        }
        ScenePrimitive::FillPolygon { points, color } => {
            if points.len() < 3 {
                return;
            }
            let mut path = BezPath::new();
            path.move_to(((points[0].x * ppp) as f64, (points[0].y * ppp) as f64));
            for p in points.iter().skip(1) {
                path.line_to(((p.x * ppp) as f64, (p.y * ppp) as f64));
            }
            path.close_path();
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                to_vello_color(*color),
                None,
                &path,
            );
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
            let mut path = BezPath::new();
            path.move_to((points[0].x as f64, points[0].y as f64));
            for p in points.iter().skip(1) {
                path.line_to((p.x as f64, p.y as f64));
            }
            path.close_path();
            scene.fill(
                Fill::NonZero,
                Affine::IDENTITY,
                to_vello_color(*color),
                None,
                &path,
            );
        }
        ScenePrimitive::StrokeLine { from, to, style } => {
            let mut path = BezPath::new();
            path.move_to(((from.x * ppp) as f64, (from.y * ppp) as f64));
            path.line_to(((to.x * ppp) as f64, (to.y * ppp) as f64));
            scene.stroke(
                &stroke_style(style, ppp),
                Affine::IDENTITY,
                to_vello_color(style.color),
                None,
                &path,
            );
        }
        ScenePrimitive::StrokePolyline { points, style } => {
            if points.len() < 2 {
                return;
            }
            let mut path = BezPath::new();
            path.move_to(((points[0].x * ppp) as f64, (points[0].y * ppp) as f64));
            for p in points.iter().skip(1) {
                path.line_to(((p.x * ppp) as f64, (p.y * ppp) as f64));
            }
            scene.stroke(
                &stroke_style(style, ppp),
                Affine::IDENTITY,
                to_vello_color(style.color),
                None,
                &path,
            );
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
            let mut path = BezPath::new();
            path.move_to(((child.x * ppp) as f64, (child.y * ppp) as f64));
            path.line_to(((shoulder.x * ppp) as f64, (shoulder.y * ppp) as f64));

            let points = arc_points(*center, *radius, *start_angle, *end_angle, ppp);
            if let Some(first) = points.first() {
                path.move_to((first.x as f64, first.y as f64));
                for p in points.iter().skip(1) {
                    path.line_to((p.x as f64, p.y as f64));
                }
            }
            scene.stroke(
                &stroke_style(style, ppp),
                Affine::IDENTITY,
                to_vello_color(style.color),
                None,
                &path,
            );
        }
        ScenePrimitive::Text { .. } => {}
    }
}

fn stroke_style(style: &crate::tree::scene_graph::StrokeStyle, ppp: f32) -> KStroke {
    let mut stroke = KStroke::new((style.width * ppp).max(1.0) as f64);
    if let Some((dash, gap)) = style.dash {
        stroke.dash_pattern =
            vec![(dash * ppp).max(1.0) as f64, (gap * ppp).max(1.0) as f64].into();
    }
    stroke
}

fn to_vello_color(color: Color32) -> VelloColor {
    VelloColor::from_rgba8(color.r(), color.g(), color.b(), color.a())
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
fn encode_text_primitive(
    scene: &mut Scene,
    font: &Font,
    font_data: &FontData,
    text: &str,
    anchor: Pos2,
    angle: f32,
    align: Align2,
    size: f32,
    color: Color32,
    ppp: f32,
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

    let transform = Affine::translate((anchor.x as f64 * ppp as f64, anchor.y as f64 * ppp as f64))
        * Affine::rotate(angle as f64)
        * Affine::translate((-anchor_local.0 as f64, -anchor_local.1 as f64));

    let mut positioned = Vec::with_capacity(layout.glyphs().len());
    for g in layout.glyphs() {
        let metrics = font.metrics_indexed(g.key.glyph_index, px);
        // fontdue layout positions are derived from outline bounds (metrics.bounds),
        // while metrics.xmin/ymin/width/height are raster box values. Use bounds
        // here to keep baseline and rotation center stable across glyph contents.
        let origin_x = g.x - min_x - metrics.bounds.xmin;
        let origin_y = g.y - min_y + metrics.bounds.ymin + metrics.bounds.height;
        positioned.push(Glyph {
            id: g.key.glyph_index as u32,
            x: origin_x,
            y: origin_y,
        });
    }

    if positioned.is_empty() {
        return;
    }

    scene
        .draw_glyphs(font_data)
        .font_size(px)
        .transform(transform)
        .brush(to_vello_color(color))
        .draw(Fill::NonZero, positioned.into_iter());
}

fn load_system_sans_font_bundle() -> Option<(Font, FontData)> {
    let source = SystemSource::new();
    let handle = source
        .select_best_match(&[FamilyName::SansSerif], &Properties::new())
        .ok()?;
    let font = handle.load().ok()?;
    let bytes = font.copy_font_data()?;
    let fontdue_font = Font::from_bytes(bytes.as_ref().clone(), FontSettings::default()).ok()?;
    let peniko_font = FontData::new(Blob::from(bytes.as_ref().clone()), 0);
    Some((fontdue_font, peniko_font))
}

fn premultiplied_rgba_to_unmultiplied(src: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(src.len());
    for px in src.chunks_exact(4) {
        let a = px[3];
        if a == 0 {
            out.extend_from_slice(&[0, 0, 0, 0]);
            continue;
        }
        if a == u8::MAX {
            out.extend_from_slice(px);
            continue;
        }

        let alpha = a as f32 / 255.0;
        let r = ((px[0] as f32 / alpha).round()).clamp(0.0, 255.0) as u8;
        let g = ((px[1] as f32 / alpha).round()).clamp(0.0, 255.0) as u8;
        let b = ((px[2] as f32 / alpha).round()).clamp(0.0, 255.0) as u8;
        out.extend_from_slice(&[r, g, b, a]);
    }
    out
}
