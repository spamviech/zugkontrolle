//! Parse `cargo metadata` um verwendete crates für ein target zu erhalten.

use std::{iter, process::Command, str};

use cargo_metadata::{MetadataCommand, Package};
use proc_macro2::TokenStream;
use quote::quote;
use syn::LitStr;

pub(crate) fn verwendete_crates(target: LitStr) -> TokenStream {
    let metadata_res = MetadataCommand::new()
        .other_options([
            String::from("--filter-platform"),
            target.value(),
            String::from("--all-features"),
        ])
        .exec();
    let metadata = match metadata_res {
        Ok(metadata) => metadata,
        Err(fehler) => {
            let fehlermeldung = fehler.to_string();
            return quote!(compile_error!(#fehlermeldung));
        },
    };
    let packages = metadata.packages.iter().map(|Package { name, version, .. }| {
        let version_string = version.to_string();
        quote! {(#name, #version_string)}
    });
    quote!([#(#packages),*])
}

pub(crate) fn target_crates(input: TokenStream) -> TokenStream {
    if !input.is_empty() {
        let fehlermeldung = format!("No argument supported, but \"{input}\" was given.");
        return quote!(compile_error!(#fehlermeldung));
    }
    let stdout = match Command::new("rustc").args(["--print", "target-list"]).output() {
        Ok(output) if output.status.success() => output.stdout,
        Ok(output) => {
            let fehlermeldung =
                format!("`rustc --print target-list` returned a non-zero exit code:\n{output:?}");
            return quote!(compile_error!(#fehlermeldung));
        },
        Err(fehler) => {
            let fehlermeldung = format!("Failed to execute `rustc --print target-list`:\n{fehler}");
            return quote!(compile_error!(#fehlermeldung));
        },
    };
    let stdout_string = match str::from_utf8(&stdout) {
        Ok(stdout_string) => stdout_string,
        Err(fehler) => {
            let fehlermeldung =
                format!("`rustc --print target-list` printed non-utf8 to stdout:\n{fehler}");
            return quote!(compile_error!(#fehlermeldung));
        },
    };
    let targets: Vec<_> =
        stdout_string.lines().chain(iter::once("zugkontrolle-unbekanntes-target")).collect();
    quote!({#(
        #[cfg(zugkontrolle_target = #targets)]
        {zugkontrolle_macros::verwendete_crates!(#targets)}
    )*})
}
