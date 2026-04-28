# macOS「使用 rhaeTree 打开」支持 — 设计方案

分支：`dev/macos-open-with`

## 1. 目标与用户体验

让 macOS 用户能够把进化树文件（Newick / NEXUS / RTR 等）与 rhaeTree 关联，
并通过以下任一方式打开：

1. Finder 双击：若 rhaeTree 是默认应用，直接启动并加载该文件。
2. Finder 右键 → **打开方式 → rhaeTree**。
3. Finder 右键 → **显示简介 → 打开方式 → 全部更改…** 把某扩展名的默认应用永久设为 rhaeTree。
4. 终端 `open -a rhaeTree path/to/file.tre`。
5. 拖拽文件到 Dock 上的 rhaeTree 图标（运行中或未运行均可）。

预期交互细节：

- **冷启动**：双击 → rhaeTree 启动 → 主窗口直接显示该树（不弹文件选择对话框）。
- **运行时打开**：当前窗口若有未保存改动，弹确认框（"放弃当前改动并打开 X？/取消"）；
  无改动则直接替换当前树。
- **多文件同时打开**：macOS 会把多个文件聚合成一次 `application:openURLs:` 调用。
  当前 rhaeTree 是单窗口应用，方案为「加载第一个，其余忽略并提示」；后续如改成多文档窗口再扩展。
- **错误**：解析失败时弹错误对话框并保留原有窗口状态，不退出。

## 2. 现状梳理

仓库已有的基础设施（不需要重做）：

| 文件 | 现有内容 |
|---|---|
| `scripts/build_macos_app.sh` | 已写出 `CFBundleDocumentTypes` + `UTExportedTypeDeclarations`，扩展名 `tre/tree/trees/nex/nexus/newick/nwk/rtr` |
| `src/macos_file_open.rs` | 用 `objc2` 声明 `RhaeTreeAppDelegate`，实现 `application:openURLs:`，写入全局 `PENDING_PATHS` |
| `src/app/mod.rs:121` | `eframe::run_native` 之前调用 `macos_file_open::install()` |
| `src/gui/mod.rs:667-712` | 启动时与每帧 `consume_system_open_requests` 中调用 `take_pending_paths` |
| `dist/local-test/rhaeTree.app` | 已存在的本地测试包，bundle id `com.yorkson.rhaetree.dev` |

## 3. 关键风险与设计决策

### 3.1【高风险】winit 会覆盖我们的 `NSApp.delegate`

`src/app/mod.rs` 在 `eframe::run_native` 之前调用 `macos_file_open::install()`，
但 eframe→winit 在创建 `NSApplication` 时会**自己 setDelegate**，把 rhaeTree 的
`RhaeTreeAppDelegate` 替换掉。结果：

- 冷启动时 macOS 派发 `application:openURLs:` 给 winit 的 delegate，
  我们的 selector 永远不会被调用，文件被丢弃。
- 现有「`take_pending_paths` 在每帧轮询」的代码因此**实际拿不到任何路径**
  （除非 winit 在 setDelegate 之前 macOS 已经派发完毕，而这无法保证）。

**方案 A — 推荐：方法注入（method swizzling / 动态 class 替换）**

不再调用 `NSApplication setDelegate:`，而是：

1. 启动后立即取 `[NSApp delegate]`（winit 已设置好的实例）。
2. 用 `class_addMethod` 给该实例的 class **动态追加**
   `application:openURLs:` 与 `application:openFile:` 两个 selector，
   实现里调用我们已有的 `push_path`。
3. 若 winit 升级后自带这些 selector，则改用 `method_exchangeImplementations`
   保留原实现并追加我们的处理。

优点：不破坏 winit 的事件循环；冷启动也能拿到。
缺点：依赖 winit 实现细节；需要在 winit 的 `EventsCleared`/首次 `redraw_requested`
之前完成注入（实际可以放在 `eframe::App::new` 第一次 update 内执行）。

**方案 B — 备选：在 winit 之前抢占 NSApplication**

在 `install()` 内提前 `[NSApplication sharedApplication]` 并 `setDelegate:`，
然后让我们的 delegate 在收到 `applicationWillFinishLaunching:` 时再把控制权
转交回 winit。该方案对 eframe 0.33 的 winit 版本耦合更紧，**不推荐**。

**方案 C — 兜底：Apple Event 直接监听**

注册 `kInternetEventClass / kAEGetURL` 与 `kCoreEventClass / kAEOpenDocuments`
事件处理器，绕过 NSApplicationDelegate。Apple 已不推荐，但确实独立于 winit。
作为方案 A 失败时的回退路径。

**最终决定**：实现方案 A，方案 C 作为兜底（在收不到 openURLs 又确实是从
LaunchServices 启动的场景下生效）。

### 3.2 启动期事件丢失（race condition）

即便方案 A 成功，winit 完成 `setDelegate` 与我们 `class_addMethod` 之间存在
时间窗口。冷启动场景下，macOS 通常会在 `applicationDidFinishLaunching:` 之前
就排队 openURLs。

**对策**：

1. 在 `main.rs` 入口（`env_logger::init` 之后、`AppConfig::parse` 之前）
   通过 `[[NSAppleEventManager sharedAppleEventManager] setEventHandler:…]`
   注册 `kAEOpenDocuments` 处理器，先把启动期路径写入 `PENDING_PATHS`。
   这步纯 ObjC，不依赖 NSApplication 实例。
2. 主窗口 `FigTreeGui::new` 内已有 `take_pending_paths` 调用，会消费这些路径。
3. 后续运行时则由方案 A 注入的 `application:openURLs:` 持续产出。

### 3.3 UTI 声明拆分

当前一个 UTI（`<bundle_id>.tree`）覆盖 8 个扩展名，且 `UTTypeConformsTo = public.data`。
问题：

- 用户在「显示简介」里只能整体改（无法只把 `.nex` 设为 rhaeTree、`.nwk` 设为别的工具）。
- `public.data` 让系统不识别它们是文本，预览/Quick Look 默认行为退化。

**方案**：拆成三个 exported UTI，全部 conform 到 `public.plain-text`：

| UTI | 扩展名 | LSHandlerRank |
|---|---|---|
| `<bundle_id>.newick` | `nwk`, `newick` | `Default` |
| `<bundle_id>.nexus` | `nex`, `nexus`, `tre`, `tree`, `trees` | `Default` |
| `<bundle_id>.rtr` | `rtr` | `Owner` |

`.rtr` 是本应用私有格式 → `Owner`（rhaeTree 是该类型的"作者"）；其余通用格式
设 `Default`（用户可改），不强抢系统已有关联。

并新增对应数量的 `CFBundleDocumentTypes` 字典，每个绑一个 UTI。

### 3.4 Bundle Identifier 与文档图标

- 当前脚本默认 `com.yourorg.rhaetree`，本地测试用 `com.yorkson.rhaetree.dev`。
  分发版必须固定一个稳定 ID（建议 `org.rhaetree.app`），否则每次更换 ID
  Launch Services 会把旧关联视为新应用，用户的"默认打开方式"丢失。
- `CFBundleTypeIconFile` 可选指向单独的文档图标（`.icns`）。Phase 1 复用 app 图标，
  Phase 2 再设计专门的 tree 文档图标。

### 3.5 Launch Services 注册刷新

第一次安装到 `/Applications` 后 macOS 会自动扫描；但开发期反复构建到
`dist/local-test/` 时，Finder 经常拿不到最新 UTI。脚本结尾追加：

```bash
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister \
  -f "${APP_BUNDLE}"
```

仅在 macOS 上执行；失败不阻断打包流程（只警告）。

### 3.6 代码签名 / 公证

- 未签名或仅 ad-hoc 签名的 `.app` 在用户机器上首次双击会触发 Gatekeeper 阻止，
  且系统**不会**把它放到「打开方式」候选列表里。
- Phase 1（开发期）：保持 `--codesign-identity -`（ad-hoc），
  本机使用 `xattr -dr com.apple.quarantine` 解除隔离即可。
- Phase 2（发布）：要求 Developer ID Application 证书 + `xcrun notarytool` 公证 + `stapler staple`。
  脚本新增 `--developer-id ID --notarize-keychain-profile NAME` 两个参数，
  签名调用从 `codesign -s -` 升级到 `codesign -s "$DEVELOPER_ID" --options runtime --timestamp`。
- 该项不阻塞设计落地，但写入 `docs/release.md` 作为 release runbook。

## 4. 运行时行为细节

### 4.1 未保存改动的保护

`gui::open_external_paths` 当前直接 `load_from_path`，会丢弃当前编辑。
新增逻辑：

1. 在 `FigTreeGui` 添加 `dirty: bool`（已有 `undo_stack` 的话可用 `!undo_stack.is_empty()` 推导）。
2. `open_external_paths` 收到路径后，若 `dirty`，把路径暂存到 `pending_external: Option<PathBuf>` 并打开模态确认框。
3. 用户确认 → 真正 `load_from_path`；取消 → 丢弃 `pending_external`。

### 4.2 多文件批量

`application:openURLs:` 会一次给来多个 URL。当前 `open_external_paths`
只 `break` 在第一个成功上。维持该行为，但在第 2..n 个被忽略时记录日志：
`info!("rhaeTree is single-window; ignored {} additional files", n-1);`
并在 UI 状态栏短暂提示。

### 4.3 与 CLI 参数的优先级

- 若 `AppConfig::tree_path` 已在命令行指定，则**忽略**冷启动 openURLs 的路径
  （现有代码 `if app.config.tree_path.is_none()` 已是这个行为）。
- 当用户从 Finder 双击启动时，argv 里没有文件路径，CLI parse 不会冲突。

## 5. 改动清单（Phase 1 实现项，按文件划分）

> 本文档只设计方案；以下列出后续实现 PR 需要触达的文件，便于评审估算工作量。

| 文件 | 改动 |
|---|---|
| `src/macos_file_open.rs` | 改为 swizzle 注入（方案 A）；新增 AppleEvent 早期 handler（3.2）；保留现有 `take_pending_paths` API |
| `src/main.rs` | 在 `AppConfig::parse` 之前调用 `macos_file_open::install_early_apple_event_handler()` |
| `src/app/mod.rs` | 删掉当前 `install()` 调用位置；改在 `FigTreeGui::new` 第一帧后注入 |
| `src/gui/mod.rs` | `open_external_paths` 加 dirty 检查与确认对话框；多文件提示 |
| `scripts/build_macos_app.sh` | UTI 拆 3 个；Plist 增加多份 `CFBundleDocumentTypes`；末尾跑 `lsregister -f`；可选 `--developer-id` 参数 |
| `docs/release.md`（新建） | 记录签名 + 公证 + Launch Services 行为说明 |
| `Cargo.toml` | 无新增依赖（`objc2` 与 `objc2-foundation` 已就绪，AppleEvent 可用 `core-foundation`/裸 `extern "C"` 实现） |

## 6. 验证计划

每条都需在 `dist/local-test/rhaeTree.app` 上手工跑过：

1. **冷启动 — 单文件**：退出所有 rhaeTree → Finder 双击 `examples/FigTree.tre` → 主窗口直接显示该树。
2. **冷启动 — 多文件**：Finder 选 3 个 .tre → 右键 → 打开方式 → rhaeTree → 加载第 1 个，状态栏出现"忽略 2 个"提示。
3. **运行时打开（无脏数据）**：rhaeTree 已开 A → 双击 B → 自动切换为 B。
4. **运行时打开（有脏数据）**：rhaeTree 已开 A 并改色 → 双击 B → 弹确认框 → 取消保留 A，确认切到 B。
5. **每个扩展名一次**：`tre tree trees nex nexus newick nwk rtr` 各双击一次都能打开。
6. **「全部更改」**：右键 .nex → 显示简介 → 打开方式 → 全部更改为 rhaeTree → 之后所有 .nex 默认用 rhaeTree。
7. **拖到 Dock 图标**：未运行/运行中各试一次。
8. **`open -a` 命令行**：`open -a "$(pwd)/dist/local-test/rhaeTree.app" examples/FigTree.tre` 正常。
9. **regression**：CLI `cargo run -- examples/FigTree.tre` 与 `--headless` 行为不变。
10. **lsregister**：包刚生成完，`mdls -name kMDItemContentType examples/FigTree.tre` 输出含 `<bundle_id>.nexus`。

## 7. 不在本方案范围

- 多文档窗口（每个文件独立窗口）— 列入未来 P3。
- Quick Look 预览插件 — 单独项目。
- Spotlight 索引器 — 单独项目。
- Windows / Linux 的 file association — 已有 `winres` 处理图标但未做扩展名注册，单独设计。
- 公证 + 自动更新（Sparkle 等）— 列入 release runbook。
