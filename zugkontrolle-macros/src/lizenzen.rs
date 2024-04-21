//!

use std::{
    collections::{BTreeMap, HashMap},
    path::Path,
};

use proc_macro2::TokenStream;
use quote::quote;

use crate::metadata::{bekannte_targets, verwendete_crates_impl};

/// Lizenz-Dateien, die keinen Standard-Dateipfad verwenden.
fn lizenz_dateien() -> BTreeMap<&'static str, (&'static str, HashMap<&'static str, &'static str>)> {
    // TODO automatisches ausführen von fetch_licenses.py über std::process::Command
    // alternative direkt in rust, z.B. mit dependency
    // cargo-lock = "8.0.1"
    // Nachteil: fetch dauert eine Weile

    [
        ("SourceSerif4-Regular", ("../../fonts/source-serif/LICENSE.md", HashMap::new())),
        ("Bootstrap Icons", ("../../fonts/bootstrap-icons/LICENSE", HashMap::new())),
        ("windows_aarch64_msvc", ("../windows-0.44.0/license-mit", HashMap::new())),
        ("windows_i686_aarch64", ("../windows-0.44.0/license-mit", HashMap::new())),
        ("windows_i686_gnu", ("../windows-0.44.0/license-mit", HashMap::new())),
        ("windows_i686_msvc", ("../windows-0.44.0/license-mit", HashMap::new())),
        ("windows_x86_64_gnu", ("../windows-0.44.0/license-mit", HashMap::new())),
        ("windows_x86_64_msvc", ("../windows-0.44.0/license-mit", HashMap::new())),
        ("widestring", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wgpu", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wgpu-core", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wgpu-hal", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("wgpu-types", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("range-alloc", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("nu-ansi-term", ("LICENCE", HashMap::new())),
        ("hexf-parse", ("../CC0.txt", HashMap::new())),
        ("half", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("gpu-descriptor", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("gpu-descriptor-types", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("gpu-alloc", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("gpu-alloc-types", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("d3d12", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
        ("allocator-api2", ("../LICENSE-APACHE-2.0.txt", HashMap::new())),
    ]
    .into_iter()
    .collect()
}

/// [`crate::target_crate_lizenzen`]
pub(crate) fn target_crate_lizenzen_impl(target: &str) -> TokenStream {
    let verwendete_crates = match verwendete_crates_impl(String::from(target)) {
        Ok(crates) => crates,
        Err(fehlermeldung) => return quote!(compile_error!(#fehlermeldung);),
    };
    let target_crates =
        verwendete_crates.into_iter().map(|package| (package.name, package.version));
    let lizenz_dateien = lizenz_dateien();
    let standard_lizenz_pfade: Vec<_> = [
        "LICENSE",
        "LICENSE-MIT",
        "LICENSE-APACHE",
        "COPYING",
        "license",
        "license-mit",
        "license-apache",
        "copying",
    ]
    .into_iter()
    .flat_map(|pfad| {
        [
            String::from(pfad),
            format!("{pfad}.txt"),
            format!("{pfad}.md"),
            format!("{pfad}-GITHUB"),
            format!("{pfad}-GITHUB.txt"),
        ]
    })
    .collect();
    let mut namen = Vec::new();
    let mut versionen = Vec::new();
    let mut lizenz_pfade = Vec::new();
    let mut fehlermeldungen = Vec::new();
    for (name, version) in target_crates {
        if name.starts_with("zugkontrolle") {
            continue;
        }
        let repo_pfad = env!("zugkontrolle_workspace_root");
        let ordner_pfad = format!("{repo_pfad}/licenses/{name}-{version}");
        let Some(standard_lizenz_pfad) = standard_lizenz_pfade
            .iter()
            .find(|pfad| Path::new(&format!("{ordner_pfad}/{pfad}")).is_file())
        else {
            if false {
                let fehlermeldung = format!("Lizenz-Datei für {name}-{version} nicht gefunden!");
                fehlermeldungen.push(fehlermeldung);
            }
            continue;
        };
        let standard_lizenz_pfad_mit_map = (standard_lizenz_pfad.as_str(), HashMap::new());
        let (pfad, version_spezifisch) =
            lizenz_dateien.get(name.as_str()).unwrap_or(&standard_lizenz_pfad_mit_map);
        let datei = version_spezifisch.get(version.to_string().as_str()).unwrap_or(pfad);
        let lizenz_pfad = format!("{repo_pfad}/licenses/{name}-{version}/{datei}");
        namen.push(name);
        versionen.push(version.to_string());
        lizenz_pfade.push(lizenz_pfad);
    }
    quote! {
        &[#((#namen, #versionen, include_str!{#lizenz_pfade})),*]
        #(compile_error!(#fehlermeldungen);)*
    }
}

/// [`crate::target_crate_lizenzen`]
pub(crate) fn target_crate_lizenzen(input: &TokenStream) -> TokenStream {
    if !input.is_empty() {
        let fehlermeldung = format!("No argument supported, but \"{input}\" was given.");
        return quote!(compile_error!(#fehlermeldung););
    }
    match bekannte_targets() {
        Ok(targets) => {
            let mut output = quote!();
            for target in targets {
                let crate_lizenzen = target_crate_lizenzen_impl(&target);
                output = quote!(
                    #output
                    #[cfg(zugkontrolle_target = #target)]
                    {#crate_lizenzen}
                );
            }
            quote!({#output})
        },
        Err(fehlermeldung) => {
            quote!(compile_error!(#fehlermeldung);)
        },
    }
}
