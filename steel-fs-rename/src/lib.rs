use std::fs;

use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

declare_module!(create_module);

fn rename_file(src: String, dest: String) -> Result<(), String> {
    match fs::rename(&src, &dest) {
        Ok(_) => Ok(()),
        Err(ref e) if e.raw_os_error() == Some(18) => fs::copy(&src, &dest)
            .and_then(|_| fs::remove_file(&src))
            .map_err(|e| format!("Cross-device move failed: {}", e)),
        Err(e) => Err(format!("Rename failed: {}", e)),
    }
}

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/fs-rename");
    module.register_fn("rename-file!", rename_file);
    module
}
