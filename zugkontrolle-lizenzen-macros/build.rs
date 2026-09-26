//! Build-Script für zugkontrolle-widget: Query das build-target

use std::env;

use cargo_metadata::{Metadata, MetadataCommand};

fn main() {
    // Make the current target available in normal modules.
    let zugkontrolle_target = env::var("TARGET").unwrap_or_else(|error| {
        println!("cargo::warning=Error reading TARGET environment variable:\n{error}");
        String::from("zugkontrolle-unbekanntes-target")
    });
    println!("cargo::rustc-cfg=zugkontrolle_target=\"{zugkontrolle_target}\"");
    println!("cargo::rustc-env=zugkontrolle_target={zugkontrolle_target}");
    println!("cargo::rerun-if-env-changed=TARGET");
    // Make the current workspace root available in normal modules.
    let workspace_root = match MetadataCommand::new().exec() {
        Ok(Metadata { workspace_root, .. }) => String::from(workspace_root),
        Err(error) => {
            println!("cargo::warning=Error reading workspace_root metadata entry:\n{error}");
            let mut workspace_root = String::from(env!("CARGO_MANIFEST_DIR"));
            workspace_root.push_str("/..");
            workspace_root
        },
    };
    println!("cargo::rustc-env=zugkontrolle_workspace_root={workspace_root}");
    println!("cargo::rerun-if-changed=Cargo.toml");
    println!("cargo::rerun-if-changed={workspace_root}/Cargo.toml");
}
