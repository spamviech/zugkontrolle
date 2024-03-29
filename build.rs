//! Build-Script für zugkontrolle: erzeuge raspi cfg und setze windows binary icon

use std::env;

fn main() {
    // Make sure build script is run every time the target changes
    // (not strictly necessary, but better be safe than sorry).
    println!("cargo:rerun-if-changed-env=TARGET");
    // cargo allows querying the current target only in the build script.
    let target = env::var("TARGET").unwrap_or_else(|error| {
        println!("cargo:warning=Error reading TARGET environment variable:\n{error}");
        "zugkontrolle-unbekanntes-target".to_owned()
    });
    // Allow to use it as a cfg in normal modules as well
    println!("cargo:rustc-cfg=zugkontrolle_target=\"{target}\"");

    // cfg is for the build script, but we can use the env variables set by cargo
    // https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-build-scripts
    // otherwise, cross compilation from windows targets causes problems
    let is_windows = env::var("CARGO_CFG_WINDOWS").is_ok();
    if is_windows {
        // Setup windows binary icon
        embed_resource::compile("resources.rc", embed_resource::NONE);
    }
}
