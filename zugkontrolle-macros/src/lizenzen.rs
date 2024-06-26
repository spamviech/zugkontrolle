//!

use std::{collections::BTreeMap, iter, path::Path, process::Command, str};

use cargo_metadata::{MetadataCommand, Package};
use proc_macro2::TokenStream;
use quote::quote;

/// [`crate::verwendete_crates`]
pub(crate) fn verwendete_crates(target: String) -> Result<Vec<Package>, String> {
    let metadata_res = MetadataCommand::new()
        .other_options([String::from("--filter-platform"), target, String::from("--all-features")])
        .exec();
    let metadata = match metadata_res {
        Ok(metadata) => metadata,
        Err(fehler) => {
            let fehlermeldung = fehler.to_string();
            return Err(fehlermeldung);
        },
    };
    Ok(metadata.packages)
}

/// [`crate::target_crates`]
pub(crate) fn bekannte_targets() -> Result<Vec<String>, String> {
    let stdout = match Command::new("rustc").args(["--print", "target-list"]).output() {
        Ok(output) if output.status.success() => output.stdout,
        Ok(output) => {
            let fehlermeldung =
                format!("`rustc --print target-list` returned a non-zero exit code:\n{output:?}");
            return Err(fehlermeldung);
        },
        Err(fehler) => {
            let fehlermeldung = format!("Failed to execute `rustc --print target-list`:\n{fehler}");
            return Err(fehlermeldung);
        },
    };
    let stdout_string = match str::from_utf8(&stdout) {
        Ok(stdout_string) => stdout_string,
        Err(fehler) => {
            let fehlermeldung =
                format!("`rustc --print target-list` printed non-utf8 to stdout:\n{fehler}");
            return Err(fehlermeldung);
        },
    };

    let crates = stdout_string
        .lines()
        .map(String::from)
        .chain(iter::once(String::from("zugkontrolle-unbekanntes-target")))
        .collect();
    Ok(crates)
}

/// Lizenz-Dateien, die keinen Standard-Dateipfad verwenden.
fn lizenz_dateien() -> BTreeMap<&'static str, &'static str> {
    // TODO automatisches ausführen von fetch_licenses.py über std::process::Command
    // alternative direkt in rust, z.B. mit dependency
    // cargo-lock = "8.0.1"
    // Nachteil: fetch dauert eine Weile

    [
        ("SourceSerif4-Regular", "../../../fonts/source-serif/LICENSE.md"),
        ("Bootstrap Icons", "../../../fonts/bootstrap-icons/LICENSE"),
        ("widestring", "../LICENSE-APACHE-2.0.txt"),
        ("wgpu", "../LICENSE-APACHE-2.0.txt"),
        ("wgpu-core", "../LICENSE-APACHE-2.0.txt"),
        ("wgpu-hal", "../LICENSE-APACHE-2.0.txt"),
        ("wgpu-types", "../LICENSE-APACHE-2.0.txt"),
        ("range-alloc", "../LICENSE-APACHE-2.0.txt"),
        ("nu-ansi-term", "LICENCE"),
        ("hexf-parse", "../CC0.txt"),
        ("half", "../LICENSE-APACHE-2.0.txt"),
        ("gpu-descriptor", "../LICENSE-APACHE-2.0.txt"),
        ("gpu-descriptor-types", "../LICENSE-APACHE-2.0.txt"),
        ("gpu-alloc", "../LICENSE-APACHE-2.0.txt"),
        ("gpu-alloc-types", "../LICENSE-APACHE-2.0.txt"),
        ("d3d12", "../LICENSE-APACHE-2.0.txt"),
        ("allocator-api2", "../LICENSE-APACHE-2.0.txt"),
        ("naga", "../LICENSE-APACHE-2.0.txt"),
        ("ouroboros", "../LICENSE-APACHE-2.0.txt"),
        ("ouroboros_macro", "../LICENSE-APACHE-2.0.txt"),
        ("iced_aw", "9ed46bf/LICENSE"),
    ]
    .into_iter()
    .collect()
}

/// [`crate::target_crate_lizenzen`]
pub(crate) fn target_crate_lizenzen_impl(target: &str) -> (TokenStream, Vec<String>) {
    let verwendete_crates = match verwendete_crates(String::from(target)) {
        Ok(crates) => crates,
        Err(fehlermeldung) => return (quote!([]), vec![fehlermeldung]),
    };
    let target_crates =
        verwendete_crates.into_iter().map(|package| (package.name, package.version.to_string()));
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
    for (name, version) in [
        (String::from("SourceSerif4-Regular"), String::from("4.005")),
        (String::from("Bootstrap Icons"), String::from("v1.11.3")),
    ]
    .into_iter()
    .chain(target_crates)
    {
        if name.starts_with("zugkontrolle") {
            continue;
        }
        let crate_pfad = env!("CARGO_MANIFEST_DIR");
        let ordner_pfad = format!("{crate_pfad}/lizenzen/{name}-{version}");
        let standard_lizenz_pfad = standard_lizenz_pfade
            .iter()
            .find(|pfad| Path::new(&format!("{ordner_pfad}/{pfad}")).is_file());
        let Some(pfad) = lizenz_dateien
            .get(name.as_str())
            .copied()
            .or(standard_lizenz_pfad.as_ref().map(|string| string.as_str()))
        else {
            let fehlermeldung =
                format!("Lizenz-Datei für {name}-{version} nicht in \"{ordner_pfad}\" gefunden!");
            fehlermeldungen.push(fehlermeldung);
            continue;
        };
        let lizenz_pfad = format!("{ordner_pfad}/{pfad}");
        namen.push(name);
        versionen.push(version);
        lizenz_pfade.push(lizenz_pfad);
    }
    let token_stream = quote! {
        &[#((#namen, #versionen, include_str!{#lizenz_pfade})),*]
    };
    (token_stream, fehlermeldungen)
}

/// [`crate::target_crate_lizenzen`]
pub(crate) fn target_crate_lizenzen(input: &TokenStream) -> TokenStream {
    if !input.is_empty() {
        let fehlermeldung = format!("No argument supported, but \"{input}\" was given.");
        return quote! {{
            compile_error!(#fehlermeldung);
            []
        }};
    }
    match bekannte_targets() {
        Ok(targets) => {
            let mut output = quote!();
            for target in targets {
                let (crate_lizenzen, fehlermeldungen) = target_crate_lizenzen_impl(&target);
                let compile_error = quote!(#(compile_error!(#fehlermeldungen);)*);
                output = quote!(
                    #output
                    #[cfg(zugkontrolle_target = #target)]
                    {#compile_error}
                    #[cfg(zugkontrolle_target = #target)]
                    {#crate_lizenzen}
                );
            }
            quote!({#output})
        },
        Err(fehlermeldung) => {
            quote! {{
                compile_error!(#fehlermeldung);
                []
            }}
        },
    }
}
