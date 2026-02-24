fn main() {
    println!("cargo:rerun-if-changed=icon/rhaetree.ico");

    #[cfg(target_os = "windows")]
    {
        let mut res = winres::WindowsResource::new();
        res.set_icon("icon/rhaetree.ico");
        res.compile()
            .expect("failed to compile Windows resources for app icon");
    }
}
