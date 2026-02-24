# rhaeTree

一个基于 Rust 的系统发育树查看与导出工具。

本项目受我最喜爱的进化树编辑工具[Figtree](https://github.com/rambaut/figtree)启发。后者已经多年没有更新，在新的系统中对高分辨率屏幕适配有问题，因此使用Rust在与Codex的合作下完成此工具，补上了一些我比较常用的功能，但仍有许多不足，依然在积极更新中。


[English](../README.md) | 简体中文

## 功能概览

- 支持加载 `Newick`、`NEXUS`格式进化树
- 支持多种树布局（如矩形、圆形、径向布局）
- 支持交互式选择、分支高亮、着色、重定根、排序
- 支持导出 `PNG`、`JPEG`、`SVG`、`PDF`格式
- 支持以`RTR`格式导出可再次加载的编辑结果

## 快速开始

```bash
# 1) 检查编译
cargo check

# 2）启动软件
cargo run

# 3) 使用示例树启动 GUI
cargo run -- examples/FigTree.tre

```

## 命令行

```bash
cargo run -- [TREE_FILE]
```

## 目录结构

- `src/app/`：应用启动与 CLI 逻辑
- `src/gui/`：主界面与交互逻辑
- `src/io/`：文件解析与 RTR 持久化
- `src/tree/`：树模型、布局、scene graph、渲染器
- `src/export/`：SVG/PDF 导出
- `examples/`：示例树文件
- `docs/`：项目文档

## 说明

- `RTR` 为本工具使用的会话/配置持久化格式。
