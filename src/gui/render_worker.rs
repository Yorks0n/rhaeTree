use std::sync::mpsc;
use std::thread;

use eframe::egui;

use crate::tree::scene_graph::TreeSceneGraph;
use crate::tree::skia_renderer::{SkiaRenderOutput, SkiaTreeRenderer};

pub struct RenderJob {
    pub base: TreeSceneGraph,
    pub decor: TreeSceneGraph,
    pub ppp: f32,
    pub scale: f32,
    pub job_id: u64,
}

pub struct RenderResult {
    pub base: SkiaRenderOutput,
    pub decor: SkiaRenderOutput,
    pub job_id: u64,
}

pub struct RenderWorker {
    job_sender: mpsc::SyncSender<RenderJob>,
    result_receiver: mpsc::Receiver<RenderResult>,
    pub worker_dead: bool,
}

impl RenderWorker {
    pub fn new(ctx: egui::Context) -> Self {
        // Capacity 1: at most one queued job waiting when worker is busy.
        let (job_tx, job_rx) = mpsc::sync_channel::<RenderJob>(1);
        let (result_tx, result_rx) = mpsc::channel::<RenderResult>();

        thread::spawn(move || {
            let mut renderer = SkiaTreeRenderer::new();
            loop {
                let job = match job_rx.recv() {
                    Ok(j) => j,
                    Err(_) => break, // sender dropped, exit thread
                };

                let base = match renderer.render_scene_to_image(&job.base, job.ppp, job.scale) {
                    Ok(out) => out,
                    Err(e) => {
                        log::error!("RenderWorker: base layer failed: {e}");
                        break;
                    }
                };
                let decor = match renderer.render_scene_to_image(&job.decor, job.ppp, job.scale) {
                    Ok(out) => out,
                    Err(e) => {
                        log::error!("RenderWorker: decor layer failed: {e}");
                        break;
                    }
                };

                if result_tx
                    .send(RenderResult {
                        base,
                        decor,
                        job_id: job.job_id,
                    })
                    .is_err()
                {
                    break; // receiver dropped, exit thread
                }
                ctx.request_repaint();
            }
        });

        Self {
            job_sender: job_tx,
            result_receiver: result_rx,
            worker_dead: false,
        }
    }

    /// Try to submit a job. Returns `false` if the worker thread has died.
    /// If the channel is full (worker is busy with a queued job), the new job
    /// is silently dropped — the caller will retry next frame.
    pub fn try_send(&mut self, job: RenderJob) -> bool {
        if self.worker_dead {
            return false;
        }
        match self.job_sender.try_send(job) {
            Ok(_) => true,
            Err(mpsc::TrySendError::Full(_)) => true, // busy but alive
            Err(mpsc::TrySendError::Disconnected(_)) => {
                self.worker_dead = true;
                false
            }
        }
    }

    /// Poll for a completed result without blocking.
    pub fn try_recv(&self) -> Option<RenderResult> {
        self.result_receiver.try_recv().ok()
    }
}
