# FigTree Rust - AGENT 指南（按当前仓库实现更新）

本文档面向在本仓库协作的工程代理（Agent）与开发者，描述当前真实功能、代码结构、约束与推荐工作流。内容以 `main` 工作树当前代码为准。

## 1. 项目快照

- 项目：`figtree_rust`
- 语言：Rust 2021
- GUI：`eframe/egui 0.33`（`wgpu` 后端）
- 渲染后端：
  - 屏幕：`Vello`（默认）与 `Skia` 可切换
  - PDF：`Cairo` 后端导出
  - SVG：自定义 scene graph 导出
- 解析库：`phylotree 0.1.3`
- 当前已支持文件格式：`Newick`、`NEXUS`、`RTR`（自定义会话格式）

## 2. 快速命令

```bash
# 编译检查
cargo check

# 运行 GUI
cargo run -- <tree_file>

# 运行无 GUI 模式
cargo run -- --headless <tree_file>
```

## 3. 目录与职责

- `src/main.rs`：程序入口。
- `src/app/mod.rs`：CLI 参数、GUI/Headless 启动分流。
- `src/gui/mod.rs`：主界面状态、菜单、侧栏、交互、Save/Save As/Export、Undo/Redo。
- `src/io/mod.rs`：格式识别、`Newick/NEXUS/RTR` 读写与解析。
- `src/tree/mod.rs`：树数据结构与变换（order/reroot/transform 等）。
- `src/tree/layout.rs`：布局计算（Rectangular/Circular/Radial/Slanted/Daylight 等）。
- `src/tree/painter.rs`：样式配置、高亮逻辑、命中检测、（egui 逻辑层）绘制策略。
- `src/tree/scene_graph.rs`：后端无关绘制原语。
- `src/tree/vello_renderer.rs`：Vello 渲染实现（默认屏幕后端）。
- `src/tree/skia_renderer.rs`：Skia 渲染实现（可切换）。
- `src/tree/cairo_renderer.rs`：Cairo 绘制适配（PDF 输出）。
- `src/export/svg.rs`：SVG 导出。
- `src/export/pdf.rs`：PDF 导出（基于 Cairo）。

## 4. 当前关键功能（已落地）

### 4.1 文件读写

- 可打开：`*.tree, *.tre, *.trees, *.nexus, *.nex, *.newick, *.nwk, *.rtr`
- 自动识别：扩展名 + 内容头（如 `#NEXUS` / `BEGIN ...`）
- `RTR`：
  - 结构为 NEXUS 兼容 `taxa` + `trees` 块，附加 `begin figtree_rust; set ...; end;`
  - 可序列化布局、渲染、颜色覆盖、高亮、viewer/root/order/transform 等设置

### 4.2 NEXUS 解析能力

- `parse_nexus` 支持 TREES block 的多行 tree 语句累积。
- 语句终止判断考虑引号与注释括号，避免误切分。
- 兼容部分 legacy 注释：如 ` [&label="[&label=0.98]"] ` 会被 fallback 清洗后解析。
- `TRANSLATE` 语句当前主要是跳过，不做完整编号到 taxon 名称映射重写。

### 4.3 GUI 操作与状态

- Edit 菜单：`Undo/Redo`（最多 5 步）、`Clear Selection`、`Clear Highlight`。
- Apply 类修改已改为即时生效，不再依赖点击空白区触发。
- 打开解析失败时会显示警告弹窗（Warning window）。

### 4.4 Save / Save As 语义（RTR）

- `Save`：
  - 若当前源文件是 `RTR`，或本次会话已 `Save As` 过 `RTR`，直接覆盖保存。
  - 否则首次 `Save` 会转到 `Save As` 对话框。
- `Save As...`：
  - 仅保存为 `RTR`。
  - 默认文件名：`原文件名去后缀 + .rtr`，缺省回退 `tree.rtr`。

### 4.5 渲染与导出

- 屏幕默认渲染后端：`Vello`（UI 可切换到 Skia）。
- 纹理尺寸保护：
  - `MAX_RENDER_TEXTURE_EDGE = 8192`
  - `MAX_RENDER_PIXELS = 16_000_000`
  - 自动降采样避免 `wgpu create_texture` 超限崩溃。
- 导出：
  - PNG/JPEG：离屏高分辨率渲染（`export_scale=2.0`），并按比例放大文字/线宽/点大小相关参数，避免“尺寸大但元素过小”。
  - SVG：scene graph 矢量导出。
  - PDF：Cairo PDF surface 绘制，文本可见性已按 Cairo 路径处理。

### 4.6 样式与参数（当前范围）

- Zoom：`1.0 ..= 5.0`（仅 Rectangular/Slanted 生效）
- Expansion：`1.0 ..= 2.0`
- Branch Line Width：`1.0 ..= 5.0`
- Node radius：`1.0 ..= 10.0`
- Highlight 默认色：`Color32::from_rgb(255, 255, 180)`（不提供透明度面板）

### 4.7 Shapes 面板（Tip/Node）

Tip Shape 与 Node Shape 均支持：
- `Shape`: Circle / Square / Diamond
- `Max size`
- `Size by`: Fixed 或属性字段（从解析到的数值属性中选）
- 非 Fixed 时显示 `Min size`
- `Color by`: User Selection / Fixed（Fixed 时可选色）

### 4.8 高亮行为

- 选中分支时，分支本线与高亮同时显示（不再被高亮线覆盖）。
- 支持 `Clear Highlight(s)` 清空全部 clade 高亮。
- Radial/Circular 高亮采用多边形/扇区构造，包含边界与极短分支的稳健处理。

## 5. 已知注意点

- NEXUS 中复杂 `TRANSLATE` 映射仍有提升空间（当前未完整重写 tree token）。
- Vello 后端仍标注为实验性，复杂场景建议保留 Skia 回退路径。
- `src/gui/mod.rs` 体量较大，修改时优先做局部函数化，避免继续膨胀。

## 6. Agent 修改建议

1. 先 `cargo check`，再改代码，再 `cargo check`。
2. 涉及渲染改动时至少验证：
   - Rectangular / Circular / Radial 三布局
   - `align tip labels` 开/关
   - transform branches 开/关
   - 导出 PNG/SVG/PDF 各一次
3. 涉及持久化改动时必须同时更新：
   - `collect_rtr_settings`
   - `apply_rtr_settings`
   - `io::save_rtr` / `parse_rtr_settings_block`（如新增字段）
4. 新增可撤销操作时调用 `record_undo_step()`，并保持重做栈语义正确。
5. 不要引入破坏兼容的 RTR key 重命名；如必须变更，保留旧 key 兼容读取。

## 7. 最小检查清单（提交前）

- `cargo check` 通过。
- 打开 Newick、NEXUS、RTR 各至少一个样例正常。
- Save/Save As 语义符合第 4.4 节。
- 导出 PNG/JPEG/SVG/PDF 不报错，文本可见，线宽与字号比例合理。
- 关键交互（选择、高亮、Undo/Redo、布局切换）正常。

