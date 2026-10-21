use eframe::egui;

/// Leave copy events for focused widgets (especially text edits). Native
/// egui-winit emits Copy instead of a Key event for Cmd/Ctrl+C.
pub(super) fn consume_subtree_copy(ctx: &egui::Context, has_branch: bool) -> bool {
    if !has_branch || ctx.wants_keyboard_input() {
        return false;
    }
    ctx.input_mut(|input| {
        let mut copy = false;
        input.events.retain(|event| {
            if matches!(event, egui::Event::Copy) {
                copy = true;
                false
            } else {
                true
            }
        });
        let shortcut = egui::KeyboardShortcut::new(egui::Modifiers::COMMAND, egui::Key::C);
        // Consume both forms even if the integration supplies them together.
        let key = input.consume_shortcut(&shortcut);
        copy || key
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(modifiers: egui::Modifiers) -> egui::Event {
        egui::Event::Key {
            key: egui::Key::C,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers,
        }
    }

    #[test]
    fn consumes_native_copy_and_keyboard_fallback_once() {
        for events in [
            vec![egui::Event::Copy],
            vec![key(egui::Modifiers::COMMAND)],
            vec![egui::Event::Copy, key(egui::Modifiers::COMMAND)],
        ] {
            let ctx = egui::Context::default();
            let _ = ctx.run(
                egui::RawInput {
                    events,
                    ..Default::default()
                },
                |ctx| {
                    assert!(consume_subtree_copy(ctx, true));
                    assert!(!consume_subtree_copy(ctx, true));
                },
            );
        }
    }

    #[test]
    fn leaves_copy_alone_without_a_branch() {
        let ctx = egui::Context::default();
        let _ = ctx.run(
            egui::RawInput {
                events: vec![egui::Event::Copy],
                ..Default::default()
            },
            |ctx| {
                assert!(!consume_subtree_copy(ctx, false));
                assert!(ctx.input(|i| i.events.contains(&egui::Event::Copy)));
            },
        );
    }

    #[test]
    fn plain_c_is_not_copy() {
        let ctx = egui::Context::default();
        let _ = ctx.run(
            egui::RawInput {
                events: vec![key(egui::Modifiers::NONE)],
                ..Default::default()
            },
            |ctx| {
                assert!(!consume_subtree_copy(ctx, true));
                assert_eq!(ctx.input(|i| i.events.len()), 1);
            },
        );
    }

    #[test]
    fn focused_text_edit_keeps_native_copy() {
        let ctx = egui::Context::default();
        let mut text = String::from("search text");
        let _ = ctx.run(egui::RawInput::default(), |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                ui.text_edit_singleline(&mut text).request_focus();
            });
        });
        let _ = ctx.run(
            egui::RawInput {
                events: vec![egui::Event::Copy],
                ..Default::default()
            },
            |ctx| {
                assert!(ctx.wants_keyboard_input());
                assert!(!consume_subtree_copy(ctx, true));
                assert!(ctx.input(|i| i.events.contains(&egui::Event::Copy)));
            },
        );
    }
}
