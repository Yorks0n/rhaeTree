#[cfg(target_os = "macos")]
mod imp {
    use std::cell::RefCell;
    use std::path::PathBuf;
    use std::sync::{Mutex, OnceLock};

    use objc2::rc::Retained;
    use objc2::runtime::ProtocolObject;
    use objc2::{declare_class, msg_send_id, mutability, ClassType, DeclaredClass};
    use objc2_app_kit::{NSApplication, NSApplicationDelegate};
    use objc2_foundation::{NSArray, MainThreadMarker, NSObject, NSObjectProtocol, NSURL};

    static PENDING_PATHS: OnceLock<Mutex<Vec<PathBuf>>> = OnceLock::new();
    thread_local! {
        static APP_DELEGATE: RefCell<Option<Retained<AppDelegate>>> = const { RefCell::new(None) };
    }

    fn pending_paths() -> &'static Mutex<Vec<PathBuf>> {
        PENDING_PATHS.get_or_init(|| Mutex::new(Vec::new()))
    }

    fn push_path(path: PathBuf) {
        if let Ok(mut guard) = pending_paths().lock() {
            guard.push(path);
        }
    }

    declare_class!(
        struct AppDelegate;

        unsafe impl ClassType for AppDelegate {
            type Super = NSObject;
            type Mutability = mutability::MainThreadOnly;
            const NAME: &'static str = "RhaeTreeAppDelegate";
        }

        impl DeclaredClass for AppDelegate {
            type Ivars = ();
        }

        unsafe impl NSObjectProtocol for AppDelegate {}

        unsafe impl NSApplicationDelegate for AppDelegate {
            #[method(application:openURLs:)]
            fn application_open_urls(&self, _application: &NSApplication, urls: &NSArray<NSURL>) {
                for url in urls.iter() {
                    let path_opt = unsafe { url.path() };
                    if let Some(path) = path_opt {
                        let text = path.to_string();
                        if !text.is_empty() {
                            push_path(PathBuf::from(text));
                        }
                    }
                }
            }
        }
    );

    impl AppDelegate {
        fn new(mtm: MainThreadMarker) -> Retained<Self> {
            let this = mtm.alloc().set_ivars(());
            unsafe { msg_send_id![super(this), init] }
        }
    }

    pub fn install() {
        // Keep `cargo run` path simple and stable:
        // only install AppKit delegate when running from a bundled `.app`.
        let running_in_bundle = std::env::current_exe()
            .ok()
            .and_then(|exe| exe.to_str().map(|s| s.contains(".app/Contents/MacOS/")))
            .unwrap_or(false);
        if !running_in_bundle {
            return;
        }

        APP_DELEGATE.with(|slot| {
            if slot.borrow().is_some() {
                return;
            }
            let Some(mtm) = MainThreadMarker::new() else {
                return;
            };
            let delegate = AppDelegate::new(mtm);
            let app = NSApplication::sharedApplication(mtm);
            app.setDelegate(Some(ProtocolObject::from_ref(&*delegate)));
            *slot.borrow_mut() = Some(delegate);
        });
    }

    pub fn take_pending_paths() -> Vec<PathBuf> {
        if let Ok(mut guard) = pending_paths().lock() {
            std::mem::take(&mut *guard)
        } else {
            Vec::new()
        }
    }
}

#[cfg(target_os = "macos")]
pub fn install() {
    imp::install();
}

#[cfg(target_os = "macos")]
pub fn take_pending_paths() -> Vec<std::path::PathBuf> {
    imp::take_pending_paths()
}

#[cfg(not(target_os = "macos"))]
pub fn install() {}

#[cfg(not(target_os = "macos"))]
pub fn take_pending_paths() -> Vec<std::path::PathBuf> {
    Vec::new()
}
