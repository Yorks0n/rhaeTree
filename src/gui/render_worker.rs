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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubmitStatus {
    Submitted,
    Busy,
    Dead,
}

pub struct RenderWorker {
    job_sender: mpsc::SyncSender<RenderJob>,
    result_receiver: mpsc::Receiver<RenderResult>,
    pub worker_dead: bool,
    in_flight: bool,
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
            in_flight: false,
        }
    }

    pub fn is_busy(&self) -> bool {
        self.in_flight
    }

    /// Try to submit a job without replacing the currently running one.
    /// The caller should wait for `try_recv` before building another expensive
    /// scene; the next submitted job will then contain the latest UI state.
    pub fn try_send(&mut self, job: RenderJob) -> SubmitStatus {
        if self.worker_dead {
            return SubmitStatus::Dead;
        }
        if self.in_flight {
            return SubmitStatus::Busy;
        }
        match self.job_sender.try_send(job) {
            Ok(_) => {
                self.in_flight = true;
                SubmitStatus::Submitted
            }
            Err(mpsc::TrySendError::Full(_)) => {
                self.in_flight = true;
                SubmitStatus::Busy
            }
            Err(mpsc::TrySendError::Disconnected(_)) => {
                self.worker_dead = true;
                self.in_flight = false;
                SubmitStatus::Dead
            }
        }
    }

    /// Poll for a completed result without blocking.
    pub fn try_recv(&mut self) -> Option<RenderResult> {
        match self.result_receiver.try_recv() {
            Ok(result) => {
                self.in_flight = false;
                Some(result)
            }
            Err(mpsc::TryRecvError::Empty) => None,
            Err(mpsc::TryRecvError::Disconnected) => {
                self.worker_dead = true;
                self.in_flight = false;
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::{Duration, Instant};

    use super::*;

    fn empty_scene() -> TreeSceneGraph {
        TreeSceneGraph {
            size: egui::vec2(2.0, 2.0),
            primitives: Vec::new(),
            tip_label_hits: Vec::new(),
        }
    }

    fn job(job_id: u64) -> RenderJob {
        RenderJob {
            base: empty_scene(),
            decor: empty_scene(),
            ppp: 1.0,
            scale: 1.0,
            job_id,
        }
    }

    #[test]
    fn busy_worker_does_not_accept_or_mislabel_a_second_job() {
        let mut worker = RenderWorker::new(egui::Context::default());
        assert_eq!(worker.try_send(job(1)), SubmitStatus::Submitted);
        assert!(worker.is_busy());
        assert_eq!(worker.try_send(job(2)), SubmitStatus::Busy);

        let deadline = Instant::now() + Duration::from_secs(2);
        let result = loop {
            if let Some(result) = worker.try_recv() {
                break result;
            }
            assert!(Instant::now() < deadline, "render worker timed out");
            std::thread::yield_now();
        };

        assert_eq!(result.job_id, 1);
        assert!(!worker.is_busy());
        assert_eq!(worker.try_send(job(3)), SubmitStatus::Submitted);
    }
}
