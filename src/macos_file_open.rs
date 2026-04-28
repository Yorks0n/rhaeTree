//! macOS "Open With rhaeTree" support.
//!
//! See `docs/macos-open-with.md` for the design rationale.
//!
//! ## Why we swizzle `setDelegate:`
//!
//! eframe → winit owns `NSApp.delegate`. winit's delegate class does not
//! implement `application:openURLs:`, so when LaunchServices launches us with
//! a document (right-click → Open With), AppKit's default openURLs handler
//! falls through to `NSDocumentController` — which then errors with
//! *"rhaeTree cannot open files in the X format"* because we have no
//! `NSDocumentClass`.
//!
//! To intercept that path we install our shim **before** any delegate is set:
//!
//! 1. `install_early_hook()` runs in `main()` before `eframe::run_native()`.
//! 2. It method-swizzles `-[NSApplication setDelegate:]` so any delegate
//!    later passed in (winit's included) gets `application:openURLs:` and
//!    `application:openFile:` added to its class via `class_addMethod` before
//!    being installed.
//! 3. The added methods push incoming paths into a global mailbox that the
//!    GUI drains every frame in `consume_system_open_requests`.
//!
//! `install_runtime_hook()` is kept as a defence-in-depth call from
//! `FigTreeGui::new` in case some bootstrap path bypassed `setDelegate:`.

#[cfg(target_os = "macos")]
mod imp {
    use std::ffi::CString;
    use std::path::PathBuf;
    use std::sync::{Mutex, OnceLock};

    use objc2::ffi as objc_ffi;
    use objc2::runtime::{AnyObject, Sel};
    use objc2::sel;
    use objc2_app_kit::NSApplication;
    use objc2_foundation::{MainThreadMarker, NSArray, NSString, NSURL};

    static PENDING_PATHS: OnceLock<Mutex<Vec<PathBuf>>> = OnceLock::new();
    static EARLY_HOOK_INSTALLED: OnceLock<()> = OnceLock::new();
    static ORIGINAL_SET_DELEGATE: OnceLock<objc_ffi::IMP> = OnceLock::new();

    fn pending_paths() -> &'static Mutex<Vec<PathBuf>> {
        PENDING_PATHS.get_or_init(|| Mutex::new(Vec::new()))
    }

    fn push_path(path: PathBuf) {
        if let Ok(mut guard) = pending_paths().lock() {
            guard.push(path);
        }
    }

    extern "C" fn open_urls_imp(
        _this: *mut AnyObject,
        _cmd: Sel,
        _app: *mut AnyObject,
        urls: *mut AnyObject,
    ) {
        if urls.is_null() {
            return;
        }
        let array: &NSArray<NSURL> = unsafe { &*(urls as *const NSArray<NSURL>) };
        for url in array.iter() {
            if let Some(path) = unsafe { url.path() } {
                let s = path.to_string();
                if !s.is_empty() {
                    log::info!("[macos_open] application:openURLs: -> {}", s);
                    push_path(PathBuf::from(s));
                }
            }
        }
    }

    extern "C" fn open_file_imp(
        _this: *mut AnyObject,
        _cmd: Sel,
        _app: *mut AnyObject,
        filename: *mut AnyObject,
    ) -> objc_ffi::BOOL {
        if filename.is_null() {
            return objc_ffi::NO;
        }
        let s: &NSString = unsafe { &*(filename as *const NSString) };
        let path = s.to_string();
        if path.is_empty() {
            return objc_ffi::NO;
        }
        log::info!("[macos_open] application:openFile: -> {}", path);
        push_path(PathBuf::from(path));
        objc_ffi::YES
    }

    /// Add `application:openURLs:` and `application:openFile:` to the given
    /// delegate's class. `class_addMethod` is idempotent: it only adds when
    /// the method is not already present, so calling this for every delegate
    /// (including swaps) is safe and cheap.
    unsafe fn patch_delegate_class(delegate: *mut AnyObject) {
        if delegate.is_null() {
            return;
        }
        let class_ptr = objc_ffi::object_getClass(delegate.cast()) as *mut objc_ffi::objc_class;
        if class_ptr.is_null() {
            return;
        }

        let types_open_urls = CString::new("v@:@@").unwrap();
        let imp_open_urls: unsafe extern "C" fn() = std::mem::transmute(
            open_urls_imp as extern "C" fn(*mut AnyObject, Sel, *mut AnyObject, *mut AnyObject),
        );
        let added_urls = objc_ffi::class_addMethod(
            class_ptr,
            sel!(application:openURLs:).as_ptr(),
            Some(imp_open_urls),
            types_open_urls.as_ptr(),
        );

        let types_open_file = CString::new("c@:@@").unwrap();
        let imp_open_file: unsafe extern "C" fn() = std::mem::transmute(
            open_file_imp
                as extern "C" fn(
                    *mut AnyObject,
                    Sel,
                    *mut AnyObject,
                    *mut AnyObject,
                ) -> objc_ffi::BOOL,
        );
        let added_file = objc_ffi::class_addMethod(
            class_ptr,
            sel!(application:openFile:).as_ptr(),
            Some(imp_open_file),
            types_open_file.as_ptr(),
        );

        log::info!(
            "[macos_open] patched delegate class (openURLs added={}, openFile added={})",
            added_urls as i32,
            added_file as i32,
        );
    }

    /// Replacement for `-[NSApplication setDelegate:]`. Patches the new
    /// delegate's class first, then forwards to the original implementation.
    extern "C" fn intercepted_set_delegate(
        this: *mut AnyObject,
        cmd: Sel,
        new_delegate: *mut AnyObject,
    ) {
        unsafe {
            patch_delegate_class(new_delegate);
        }
        if let Some(original) = ORIGINAL_SET_DELEGATE.get() {
            unsafe {
                let orig_fn: extern "C" fn(*mut AnyObject, Sel, *mut AnyObject) =
                    std::mem::transmute(*original);
                orig_fn(this, cmd, new_delegate);
            }
        } else {
            log::error!("[macos_open] intercepted_set_delegate but ORIGINAL_SET_DELEGATE is unset");
        }
    }

    /// Install the `setDelegate:` swizzle. Must be called from `main()` on
    /// the main thread, **before** `eframe::run_native()`.
    pub fn install_early_hook() {
        if EARLY_HOOK_INSTALLED.get().is_some() {
            return;
        }
        let Some(_mtm) = MainThreadMarker::new() else {
            log::warn!("[macos_open] install_early_hook called off main thread; skipping");
            return;
        };

        unsafe {
            let nsapp_class = objc_ffi::objc_getClass(c"NSApplication".as_ptr());
            if nsapp_class.is_null() {
                log::error!("[macos_open] NSApplication class not found");
                return;
            }
            let method = objc_ffi::class_getInstanceMethod(
                nsapp_class as *const _,
                sel!(setDelegate:).as_ptr(),
            );
            if method.is_null() {
                log::error!("[macos_open] -[NSApplication setDelegate:] not found");
                return;
            }
            let original_imp =
                objc_ffi::method_getImplementation(method as *const objc_ffi::objc_method);
            if ORIGINAL_SET_DELEGATE.set(original_imp).is_err() {
                log::warn!("[macos_open] ORIGINAL_SET_DELEGATE already initialised");
            }

            let new_imp: unsafe extern "C" fn() = std::mem::transmute(
                intercepted_set_delegate as extern "C" fn(*mut AnyObject, Sel, *mut AnyObject),
            );
            objc_ffi::method_setImplementation(
                method as *const objc_ffi::objc_method,
                Some(new_imp),
            );
        }

        let _ = EARLY_HOOK_INSTALLED.set(());
        log::info!("[macos_open] swizzled -[NSApplication setDelegate:]");
    }

    /// Idempotent fallback: directly patch `NSApp.delegate`'s class, in case
    /// some path bypassed `setDelegate:` (shouldn't happen in normal winit
    /// flow, but cheap insurance).
    pub fn install_runtime_hook() {
        let Some(mtm) = MainThreadMarker::new() else {
            return;
        };
        let app = NSApplication::sharedApplication(mtm);
        let Some(delegate) = (unsafe { app.delegate() }) else {
            return;
        };
        unsafe {
            let delegate_obj: *const AnyObject = (&*delegate) as *const _ as *const AnyObject;
            patch_delegate_class(delegate_obj as *mut AnyObject);
        }
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
pub fn install_early_hook() {
    imp::install_early_hook();
}

#[cfg(target_os = "macos")]
pub fn install_runtime_hook() {
    imp::install_runtime_hook();
}

#[cfg(target_os = "macos")]
pub fn take_pending_paths() -> Vec<std::path::PathBuf> {
    imp::take_pending_paths()
}

#[cfg(not(target_os = "macos"))]
pub fn install_early_hook() {}

#[cfg(not(target_os = "macos"))]
pub fn install_runtime_hook() {}

#[cfg(not(target_os = "macos"))]
pub fn take_pending_paths() -> Vec<std::path::PathBuf> {
    Vec::new()
}
