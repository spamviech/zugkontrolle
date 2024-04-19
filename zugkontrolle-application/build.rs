//! Build-Script für zugkontrolle-application: Query die workspace version

use std::env;

use cargo_metadata::{Metadata, MetadataCommand};

fn main() {
    // Make the current workspace version available in normal modules.
    let (zugkontrolle_version, workspace_root) = match MetadataCommand::new().exec() {
        Ok(Metadata { packages, workspace_root, .. }) => {
            let zugkontrolle_version =
                match packages.into_iter().find(|package| package.name == "zugkontrolle") {
                    Some(package) => format!("{}", package.version),
                    None => String::from(env!("CARGO_PKG_VERSION")),
                };
            (zugkontrolle_version, String::from(workspace_root))
        },
        Err(error) => {
            println!("cargo::warning=Error reading workspace_root metadata entry:\n{error}");
            (String::from(env!("CARGO_PKG_VERSION")), String::from(".."))
        },
    };
    println!("cargo::rustc-env=zugkontrolle_version={zugkontrolle_version}");
    println!("cargo::rerun-if-changed=Cargo.toml");
    println!("cargo::rerun-if-changed={workspace_root}/Cargo.toml");
}
