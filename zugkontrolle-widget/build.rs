//! Build-Script für zugkontrolle-widget: Query das build-target

use std::env;

fn main() {
    // Make the current target available in normal modules.
    let zugkontrolle_target = env::var("TARGET").unwrap_or_else(|error| {
        println!("cargo::warning=Error reading TARGET environment variable:\n{error}");
        String::from("zugkontrolle-unbekanntes-target")
    });
    println!("cargo::rustc-cfg=zugkontrolle_target=\"{zugkontrolle_target}\"");
    println!("cargo::rerun-if-env-changed=TARGET");
}
