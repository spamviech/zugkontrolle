//! Build-Script für zugkontrolle: erzeuge raspi cfg und setze windows binary icon

use std::env;

fn main() {
    // cfg is for the build script, but we can use the env variables set by cargo
    // https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-build-scripts
    // otherwise, cross compilation from windows targets causes problems
    let is_windows = env::var("CARGO_CFG_WINDOWS").is_ok();
    if is_windows {
        // Setup windows binary icon
        embed_resource::compile("resources.rc", embed_resource::NONE);
    }
}
