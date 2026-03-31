# TODOS — rhaetree

Tracked items from design reviews and CEO review sessions.

---

## P2 — Parallel layer rendering

**What:** Spawn 3 persistent worker threads (one per layer — base, decor, interaction) and dispatch all 3 simultaneously instead of serially.

**Why:** Current serial worker renders all 3 layers sequentially. At 8M pixels × 3 layers = ~180–480ms per full-frame job. Parallel execution would bring this to ~60–160ms (time of slowest layer).

**Pros:** 3× faster full-frame updates; clean architecture (one responsibility per worker).
**Cons:** 3× memory for in-flight renders; more complex job dispatch logic (3 SyncSenders, merge on receive).
**Context:** The background rendering implementation uses a single `RenderWorker` with serial layer rendering. Parallelizing is a follow-up optimization once the serial implementation is stable.

**Effort:** M (human: ~1 day / CC: ~20min) | **Depends on:** Background rendering PR shipped

---

## P2 — Automated tests for RenderWorker

**What:** Add `#[cfg(test)]` unit tests in `src/gui/render_worker.rs`. Test: dispatch a job with a minimal `TreeSceneGraph`, verify result contains a non-empty `ColorImage` of the expected dimensions.

**Why:** No automated test coverage exists. The `RenderWorker` is the most testable new component — pure Rust, no egui dependency, clear input/output. A basic smoke test catches worker panics and validates `Send` bounds at compile time.

**Pros:** First test in the project; establishes a testing pattern for future work.
**Cons:** Requires building a minimal `TreeSceneGraph` for test setup (~30 lines).
**Context:** The worker renders via `render_scene_to_image()` which is already public. Tests don't need `Tree` or egui.

**Effort:** S (human: ~2h / CC: ~10min) | **Depends on:** RenderWorker implementation

---

## P3 — Large tree LOD / tile rendering (10k+ tips)

**What:** Add level-of-detail culling to `build_tree_scene()` — skip branches below a pixel-size threshold at current zoom level.

**Why:** Current rendering is O(n) for scene building and O(pixels) for rasterizing. At 10k+ tips, scene building starts taking 10–50ms on the UI thread and the rendered result is too dense to be useful. LOD opens the tool to real genomic datasets.

**Pros:** Dramatically expands use case. Most phylogenetic datasets in production have 1k–50k tips.
**Cons:** Non-trivial algorithm. Must preserve topological correctness at every zoom level. Needs careful testing.
**Context:** Background rendering is a prerequisite — LOD without background threads would still freeze on the cull computation.

**Effort:** XL (human: ~2 weeks / CC: ~2h) | **Depends on:** Background rendering shipped, parallel layers (optional but beneficial)
