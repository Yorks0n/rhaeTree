# 仓库分析与子树复制修复（2026-10-21）

## 范围与结构

本次检查 Rust 仓库的模型、解析、GUI、布局、渲染和导出路径。结论来自代码阅读和本地编译/测试，未做性能基准测试；下面的优先级不是实测耗时排名。

调用链为 `app → io → Tree/TreeViewer → layout → scene_graph → tiny-skia/Cairo/SVG`。GUI 使用 egui/wgpu 显示栅格纹理；后台 worker 负责 Base/Decor 栅格化，Interaction 同步渲染。已有纹理签名、缩放防抖、视口分块和繁忙任务限流，不建议重做这些机制。

## 冗余与维护成本

| 位置 | 已确认现象 | 建议 |
| --- | --- | --- |
| `Cargo.toml` | `printpdf`、`thiserror` 没有源码引用；PDF 实际走 Cairo | 清理直接依赖及 lockfile，检查各平台构建。收益主要是依赖体积和构建成本，而非帧率 |
| `Tree`（`src/tree/mod.rs`） | 同时持有 `nodes`、原始 `newick`、`phylo`；生产代码不再读取 `phylo`，只有测试读取其大小；树级 `attributes` 无读取 | 解析完成后释放 `phylo`，将原始输入与可编辑模型分离。不要移除正在使用的节点级 attributes |
| `FigTreeGui::load_from_path` / `TreeViewer::set_trees` | bundle 保留一份树、viewer 保留当前树和原始树，加载时至少有三份树数据；GUI 后续仅用 bundle 判断是否已加载 | GUI 仅保留元数据；原始树使用共享不可变存储 |
| `capture_edit_state` | 每次撤销记录克隆整个 TreeViewer，包括所有树以及原始、重定根和排序快照；最多五步 | 优先共享不变数据，随后改为当前树/样式快照或编辑命令。不能简单删除用于恢复的快照 |
| `src/gui/mod.rs` | 超过 6000 行，混合 RTR 设置、导出、撤销、菜单和渲染调度 | 按职责拆分，优先抽离持久化和渲染缓存管理，减少状态失效路径遗漏 |
| `AGENT.md` / `src/ui/mod.rs` | 文档仍描述已不存在的 Vello 文件和切换入口；headless 导出是返回成功的 stub | 同步文档；未实现的 CLI 导出应明确返回错误，或复用已有导出管线 |

## 性能优化顺序

1. **缓存显示树和布局（高优先级）。** `build_display_tree_layout` 每次克隆完整 Tree，执行变换并重建布局。Base/Decor 更新、Interaction 更新、点击命中检测分别调用它。只改选择时也会重新计算布局。建议按拓扑/枝长版本、布局类型、变换参数和标签布局参数缓存；选择和配色变化复用几何结果。布局与场景构建目前仍在 UI 线程，后台 worker 只承担栅格化。
2. **减少全树签名扫描（高优先级）。** 三层签名都经 `hash_shared_render_state → hash_tree` 遍历全树，各类颜色覆盖签名另行全树扫描。建议先共用一次树签名，再引入明确的模型/样式版本号；必须覆盖修改、加载、重定根、撤销和重做，避免缓存错误。
3. **按脏图层提交渲染任务（中优先级）。** `needs_base || needs_decor` 为真时同时构建并渲染两个图层。修改标签也会重画分支层。可将 RenderJob 的图层改为可选，并保留现有 job_id 与视口一致性判断。
4. **缓存字形位图（中优先级）。** `skia_renderer::render_text` 对每个字符调用 `font.rasterize_config`，重复字符没有跨标签或跨帧缓存。可按字体标识、字形键和像素尺寸使用有内存上限的缓存；缩放与字体变化需正确区分缓存键。
5. **优化深树遍历和子树复制（后续）。** `serialize_subtree_newick` 即使复制很小的子树也分配全树大小的访问数组和字符串预留空间；递归序列化与多处布局遍历在极深树上有栈溢出风险。可考虑显式栈、按子树规模分配，并保留环路及重复节点检测。

建议使用 1 千、1 万、10 万叶的平衡树和梳状树，分别测量首次打开、分支选择、缩放、标签修改、撤销、复制小子树的耗时与峰值内存。每次优化比较相同输入的 release 构建，并验证布局、命中检测和导出一致性。

## 本次功能修复

仓库已有 `selected_branch_node`、`copy_selected_subtree`、`serialize_subtree_newick` 和 Edit → Copy Subtree 菜单，因此复用现有链路。

原快捷键只调用 `consume_shortcut(Command+C)`。本地 egui-winit 0.33 源码在检测到复制命令后发送 `Event::Copy` 并立即返回，不再发送 C 的 Key 事件，导致原监听遗漏正常原生快捷键。

新增 `src/gui/shortcuts.rs`，同时处理 Copy 事件与键盘回退，两个事件同时出现时仅触发一次；无分支或控件获得键盘焦点时不消费事件，保留文本复制。复制不改变树，不创建撤销记录。

选中分支后使用 macOS `Cmd+C` 或 Windows/Linux `Ctrl+C`，复制其后代组成的 Newick 文本。沿用现有序列化语义：保留名称、注释、枝长（含选中分支自身的枝长），不输出 RTR 内部节点 ID。例如选择 AB：`((A:1,B:2)AB:3,C:4)Root;` → `(A:1,B:2)AB:3;`。复制的是可编辑树数据，不是图片或仅供显示的枝长变换。

## 验证

- 修改前 `cargo check` 通过，已有未使用字段警告。
- `cargo test`：32 个测试通过，包含新增的原生复制/按键回退去重、无选择、普通 C、文本框焦点四项回归测试，以及已有子树 Newick 序列化测试。
- `cargo clippy --all-targets` 通过，仍有原有代码的未使用字段、manual_clamp、循环写法等警告。
- 执行 `cargo fmt`，同时统一了 GUI 文件内已有的少量格式差异。
- 尚未进行真实桌面剪贴板端到端测试或跨平台 GUI 手工验证；egui 输入路径通过无窗口测试验证。

本次没有实施上述结构和性能重构，也没有修改工作区原先已删除的 `TODOS.md`。
