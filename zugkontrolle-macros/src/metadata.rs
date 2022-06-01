//! Parse `cargo metadata` um verwendete crates für ein target zu erhalten.

use cargo_metadata::MetadataCommand;
use proc_macro2::TokenStream;
use quote::quote;
use syn::LitStr;

pub(crate) fn verwendete_crates(target: LitStr) -> TokenStream {
    let metadata_res =
        MetadataCommand::new().other_options(["--filter-platform".into(), target.value()]).exec();
    let metadata = match metadata_res {
        Ok(metadata) => metadata,
        Err(fehler) => {
            let fehlermeldung = fehler.to_string();
            return quote!(compile_error!("{}", #fehlermeldung));
        },
    };
    let packages =
        metadata.packages.iter().map(|package| format!("{}-{}", package.name, package.version));
    quote!([#(#packages),*])
}
