# rhaeTree - AGENT 指南（基于当前仓库）

## 1. 项目概览

- 项目名：`rhaeTree`（crate: `rhaetree`）
- 语言：Rust 2021
- GUI：`eframe/egui 0.33`（`wgpu`）
- 屏幕渲染后端：`Vello`（默认）/ `Skia`（可切换）
- 矢量导出：SVG（scene graph）、PDF（Cairo）
- 树解析：`phylotree 0.1.3`

## 2. 常用命令

```bash
# 编译检查
cargo check

# 启动 GUI
cargo run -- <tree_file>

# 无 GUI 预览模式（输出树摘要到 stdout）
cargo run -- --headless <tree_file>
```

## 3. 目录职责

- `src/main.rs`：程序入口
- `src/app/mod.rs`：CLI 参数、GUI/Headless 分流
- `src/gui/mod.rs`：主界面状态、菜单、交互、保存/导出、撤销重做
- `src/io/mod.rs`：Newick/NEXUS/RTR 识别、解析、保存
- `src/tree/mod.rs`：树结构与变换（reroot/order/rotate 等）
- `src/tree/layout*.rs`：各布局算法（Rectangular/Circular/Radial/Slanted/Daylight/...）
- `src/tree/painter.rs`：样式参数、颜色覆盖、高亮形状计算
- `src/tree/scene_graph.rs`：后端无关图元构建
- `src/tree/skia_renderer.rs`：Skia 渲染
- `src/tree/vello_renderer.rs`：Vello 渲染
- `src/export/svg.rs`：SVG 导出
- `src/export/pdf.rs`：PDF 导出（Cairo）
- `src/ui/mod.rs`：headless 输出与导出 stub

## 4. 当前实现要点

### 4.1 文件与格式

- 打开支持：`tree/tre/trees/nexus/nex/newick/nwk/rtr`
- 自动识别：扩展名 + 内容特征（`#NEXUS` / `BEGIN ...`）
- `RTR` = NEXUS + `begin rhaetree; set ...; end;` 配置块
- `Save/Save As` 仅写出 `RTR`

### 4.2 渲染分层

- 纹理分层：`Base` / `Decor` / `Interaction`，按此顺序叠加绘制
- **Clade highlight 当前在 `Base` 层绘制**（用于避免遮挡顶层元素）
- 分支选中描边仍在 `Interaction` 层

### 4.3 交互与状态

- Undo/Redo：最多 5 步（`MAX_UNDO_STEPS = 5`）
- Edit 菜单：`Undo`、`Redo`、`Clear Selection`、`Clear Highlight`
- Tree 菜单含 `Reroot`、`Order Increasing/Decreasing`、`Clear Highlights`
- 颜色/高亮修改走 UI 颜色选择器流程

### 4.4 导出

- GUI：支持 `Export PNG/JPEG/SVG/PDF`
- 屏幕渲染纹理保护：
  - `MAX_RENDER_TEXTURE_EDGE = 8192`
  - `MAX_RENDER_PIXELS = 16_000_000`
- PNG/JPEG 使用离屏高分辨率导出（`export_scale = 2.0`）
- **Headless 导出当前是 stub**（`src/ui/mod.rs`）

### 4.5 RTR 设置同步

涉及可持久化配置时，通常需要同步以下路径：

- 收集：`collect_rtr_settings`（`src/gui/mod.rs`）
- 应用：`apply_rtr_settings`（`src/gui/mod.rs`）
- 落盘/读取：`io::save_rtr`、`parse_rtr_settings_block`（`src/io/mod.rs`）

## 5. 已知限制

- NEXUS `TRANSLATE` 语义目前未做完整 token 重写映射（仅保留解析流程）
- Vello 仍标记为实验性（UI 文案：`Vello (Experimental)`）
- `src/gui/mod.rs` 文件体量较大，修改时优先局部化

## 6. Agent 协作准则

1. 改动前后都执行 `cargo check`。
2. 增加会改变树状态或样式的交互时，评估是否需要 `record_undo_step()`。
3. 改动图层归属或渲染条件时，检查对应 `*_render_signature` 哈希是否需同步调整。
4. 不要随意改 RTR key 名称；若必须改，至少保持向后兼容读取。
5. 修改导出逻辑时，至少验证 PNG、SVG、PDF 各一次。

## 7. 提交前最小检查

- `cargo check` 通过
- 打开 Newick / NEXUS / RTR 各至少一个样例
- Save 与 Save As 行为正常（RTR）
- Highlight、Selection、Undo/Redo、布局切换无明显回归
- GUI 导出 PNG/JPEG/SVG/PDF 无报错
