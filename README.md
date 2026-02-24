# rhaeTree

A Rust-based phylogenetic tree viewer and exporter.

English | [简体中文](docs/README.zh-CN.md)

## Features

- Load phylogenetic trees in `Newick` and `NEXUS` formats
- Support multiple layouts (e.g. rectangular, circular, radial)
- Interactive selection, branch highlighting, coloring, rerooting, and ordering
- Export to `PNG`, `JPEG`, `SVG`, and `PDF`
- Save editable results as `RTR` for reloading later

## Quick Start

```bash
# 1) Check build
cargo check

# 2) Launch the app
cargo run

# 3) Launch GUI with an example tree
cargo run -- examples/FigTree.tre
```

## Command Line

```bash
cargo run -- [TREE_FILE]
```

## Project Structure

- `src/app/`: app startup and CLI flow
- `src/gui/`: main UI and interaction logic
- `src/io/`: file parsing and RTR persistence
- `src/tree/`: tree model, layouts, scene graph, renderers
- `src/export/`: SVG/PDF export
- `examples/`: sample tree files
- `docs/`: project documentation

## Notes

- `RTR` is the session/config persistence format used by this tool.

## License

Licensed under the MIT License. See [LICENSE](LICENSE).
