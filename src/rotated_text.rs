use eframe::egui::{epaint::TextShape, *};

pub trait RotatedText {
    fn rotated_text(
        &self,
        pos: Pos2,
        anchor: Align2,
        text: impl ToString,
        font_id: FontId,
        text_color: Color32,
        angle: f32,
    ) -> Rect;
}

impl RotatedText for Painter {
    fn rotated_text(
        &self,
        pos: Pos2,
        anchor: Align2,
        text: impl ToString,
        font_id: FontId,
        text_color: Color32,
        angle: f32,
    ) -> Rect {
        let galley = self.layout_no_wrap(text.to_string(), font_id, text_color);
        let text_size = galley.size();

        // 计算未旋转时文字相对于锚点的偏移
        let anchor_offset = match anchor {
            Align2::LEFT_CENTER => Vec2::new(0.0, text_size.y / 2.0),
            Align2::RIGHT_CENTER => Vec2::new(text_size.x, text_size.y / 2.0),
            _ => anchor.to_sign() * text_size / 2.0,
        };

        // 应用旋转变换到偏移量
        let cos_a = angle.cos();
        let sin_a = angle.sin();
        let rotated_offset = Vec2::new(
            anchor_offset.x * cos_a - anchor_offset.y * sin_a,
            anchor_offset.x * sin_a + anchor_offset.y * cos_a,
        );

        // 计算文字实际绘制位置
        let text_pos = pos - rotated_offset;

        // 使用TextShape绘制旋转文字
        self.add(TextShape {
            pos: text_pos,
            galley,
            angle,
            override_text_color: Some(text_color),
            fallback_color: text_color,
            underline: Stroke::NONE,
            opacity_factor: 1.0,
        });

        // 计算旋转后的边界框（用于点击检测）
        // 文字四个角相对于锚点位置的坐标
        let corners = match anchor {
            Align2::LEFT_CENTER => [
                Vec2::new(0.0, -text_size.y / 2.0),
                Vec2::new(text_size.x, -text_size.y / 2.0),
                Vec2::new(text_size.x, text_size.y / 2.0),
                Vec2::new(0.0, text_size.y / 2.0),
            ],
            Align2::RIGHT_CENTER => [
                Vec2::new(-text_size.x, -text_size.y / 2.0),
                Vec2::new(0.0, -text_size.y / 2.0),
                Vec2::new(0.0, text_size.y / 2.0),
                Vec2::new(-text_size.x, text_size.y / 2.0),
            ],
            _ => {
                let half = text_size / 2.0;
                let offset = anchor.to_sign() * half;
                [
                    -offset - Vec2::new(half.x, half.y),
                    -offset + Vec2::new(half.x, -half.y),
                    -offset + Vec2::new(half.x, half.y),
                    -offset - Vec2::new(-half.x, half.y),
                ]
            }
        };

        let mut min_x = f32::INFINITY;
        let mut max_x = f32::NEG_INFINITY;
        let mut min_y = f32::INFINITY;
        let mut max_y = f32::NEG_INFINITY;

        // 旋转每个角点并找出边界
        for corner in corners.iter() {
            let rotated_x = corner.x * cos_a - corner.y * sin_a;
            let rotated_y = corner.x * sin_a + corner.y * cos_a;

            min_x = min_x.min(rotated_x);
            max_x = max_x.max(rotated_x);
            min_y = min_y.min(rotated_y);
            max_y = max_y.max(rotated_y);
        }

        // 返回相对于pos的边界框
        Rect::from_min_max(
            Pos2::new(pos.x + min_x, pos.y + min_y),
            Pos2::new(pos.x + max_x, pos.y + max_y),
        )
    }
}
