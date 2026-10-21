# rhaeTree — macOS 发布 Runbook

本文档说明分发版 macOS `.app` 的签名、公证与文件关联校验流程。
开发期本机测试不需要走完整流程，参考末尾「快速本机测试」即可。

## 1. Bundle Identifier 与版本号

- 发布版固定 `--bundle-id com.yorkson.rhaetree`。Bundle ID 一旦上线
  就不能再改：Launch Services 把"用户的默认打开方式"按 ID 持久化，
  改 ID 等价于发布一个新应用，旧关联会丢失。
- `--version` 与 `Cargo.toml` 中 `version` 同步即可。
- `--min-macos` 默认 `auto`：读取主程序及全部动态库的 Mach-O 最低系统版本，
  取最大值（至少 12.0）写入 `LSMinimumSystemVersion`。显式指定较低版本会导致
  打包失败。当前 CI 固定使用 `macos-26`，其 Homebrew 库通常要求 macOS 26；
  如需支持 macOS 12–15，必须在兼容的环境中重建所有原生依赖，不能只改 plist。
- macOS 发布包仅包含 Apple Silicon（arm64）程序。

### GitHub Actions 凭据与发布检查

签名使用已有 Secrets：

- `MACOS_CERT_P12_B64`
- `MACOS_CERT_P12_PASSWORD`
- `MACOS_SIGNING_IDENTITY`：Developer ID Application 证书身份

公证还需要以下 Secrets（Apple 开发者账号凭据，与 P12 密码不同）：

- `MACOS_NOTARY_APPLE_ID`：Apple ID
- `MACOS_NOTARY_PASSWORD`：该 Apple ID 的 app-specific password
- `MACOS_NOTARY_TEAM_ID`：签名证书所属 Team ID

缺少公证凭据会在构建前失败，不会继续发布未公证包。工作流依次执行：

1. 递归解析原始二进制和库的绝对路径、`@rpath`、`@loader_path`、
   `@executable_path` 依赖，再复制并改写到 `Contents/Frameworks`。
2. 检查库是否齐全、CPU 架构和最低系统版本，使用 Developer ID 签名。
3. 运行包内程序的 `--help`，检查动态加载能否完成。
4. 提交公证并确认 `Accepted`，装订票据，再生成最终分发 zip。
5. 解压最终 zip，重新检查依赖、签名、公证票据、Gatekeeper 和 `--help`。

缺失或无法解析的非系统库、库文件名冲突、错误架构均会阻断打包。
打包工具需要 Python 3.9 或以上及 Apple Command Line Tools。

本机运行打包回归测试（测试会编译并执行自有的最小 C 程序）：

```bash
python3 -m unittest discover -s scripts/tests -v
python3 scripts/macos_bundle.py verify dist/rhaeTree.app
```

## 2. 代码签名 + 公证（推荐流程）

```bash
# 一次性配置 notarytool 凭据（Apple ID + app-specific password + Team ID）
xcrun notarytool store-credentials rhaetree-notary \
  --apple-id you@example.com \
  --team-id  XXXXXXXXXX \
  --password app-specific-password

# 构建 + 用 Developer ID Application 证书签名（自动加 hardened runtime + timestamp）
scripts/build_macos_app.sh \
  --bundle-id com.yorkson.rhaetree \
  --version 0.9.11 \
  --developer-id "Developer ID Application: YOUR ORG (XXXXXXXXXX)"

# 打 zip 提交公证
ditto -c -k --sequesterRsrc --keepParent dist/rhaeTree.app dist/rhaeTree.zip
xcrun notarytool submit dist/rhaeTree.zip \
  --keychain-profile rhaetree-notary --wait

# 装订工单（用户离线也能通过 Gatekeeper）
xcrun stapler staple dist/rhaeTree.app
xcrun stapler validate dist/rhaeTree.app
spctl --assess --type execute --verbose=4 dist/rhaeTree.app

# 必须在装订后重新压缩，分发的 zip 才包含票据
ditto -c -k --sequesterRsrc --keepParent dist/rhaeTree.app dist/rhaeTree-macos.zip
```

下载后带有隔离属性的未公证 `.app` 可能被 Gatekeeper 阻止。
装订让已公证 app 在离线时也能提供公证票据；代码签名验证成功本身不能代替公证，
也不能证明所有动态库已打包。

## 3. Launch Services 注册与校验

构建脚本结尾会自动执行：

```bash
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f <app>
```

把新的 UTI 声明刷进 LS 数据库。该步失败不阻断打包，但 Finder 可能拿不到
最新关联。可以加 `--no-lsregister` 跳过（例如 CI 中 sandbox 环境）。

校验关联是否生效：

```bash
mdls -name kMDItemContentType examples/FigTree.tre
# 期望输出：kMDItemContentType = "<bundle_id>.nexus"

mdls -name kMDItemContentType some.nwk
# 期望输出：kMDItemContentType = "<bundle_id>.newick"

# 看某个 UTI 当前默认应用的 bundle id：
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -dump \
  | grep -A2 "<bundle_id>.nexus"
```

## 4. 文件类型设计回顾

| UTI | 扩展名 | LSHandlerRank | conform |
|---|---|---|---|
| `<bundle_id>.newick` | `nwk`, `newick` | `Default` | `public.plain-text` |
| `<bundle_id>.nexus`  | `nex`, `nexus`, `tre`, `tree`, `trees` | `Default` | `public.plain-text` |
| `<bundle_id>.rtr`    | `rtr` | `Owner` | `public.plain-text` |

只有 `.rtr` 用 `Owner`（rhaeTree 是该格式作者），其余通用扩展名用
`Default`，让用户自由选择是否设为默认。

## 5. 安装位置与首次启动

- 推荐分发路径：用户拖到 `/Applications`。
- 也可以放 `~/Applications`，但 LS 索引可能延迟。
- 首次双击 `.app`：
  - 已公证 → 直接启动。
  - 已签名未公证 → "无法验证开发者"对话框 → 系统设置 → 隐私与安全 → 仍要打开。
  - ad-hoc / 未签名 → "已损坏，应移到废纸篓" → 用户需手工执行
    `xattr -dr com.apple.quarantine path/to/rhaeTree.app`。

## 6. 快速本机测试

无需公证：

```bash
scripts/build_macos_app.sh \
  --bundle-id com.yorkson.rhaetree.dev \
  --version 1.0.0-local \
  --output-dir dist/local-test

# 解隔离（如果 .app 来自下载或 quarantined location）
xattr -dr com.apple.quarantine dist/local-test/rhaeTree.app

# 直接用命令行打开一个 .tre 验证 application:openURLs: 投递成功
open -a "$(pwd)/dist/local-test/rhaeTree.app" examples/FigTree.tre

# Finder 双击：
open dist/local-test
# 在 Finder 里把某个 .nex 的「打开方式」改为这个 .app，再双击。
```

校验本次实现的关键点（参见 `docs/macos-open-with.md` §6 验证清单）：

1. 冷启动单文件
2. 冷启动多文件 → 状态栏出现"忽略 N 个"提示
3. 运行时打开有未保存改动 → 弹"Discard and open / Cancel"对话框
4. CLI `cargo run -- examples/FigTree.tre` 与 `--headless` 行为不变

## 7. 卸载残留处理

如果调试期反复换 Bundle ID，LS 数据库会留下幽灵关联。彻底重置：

```bash
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister \
  -kill -r -domain local -domain system -domain user
```

执行后 Dock / Finder 的"最近打开"会重建索引，几秒到几十秒之间。
