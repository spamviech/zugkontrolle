//! Erzeuge From-Implementierung für alle Varianten eines Enums, die genau ein Element halten.

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Field, Fields, FieldsNamed, FieldsUnnamed, ItemEnum};

pub(crate) fn impl_from(item: ItemEnum) -> TokenStream {
    let mut impls = Vec::new();
    let item_ident = item.ident;
    for variant in item.variants {
        let var_ident = variant.ident;
        let (arg_teil, ty) = match &variant.fields {
            Fields::Named(FieldsNamed { named, .. }) if named.len() == 1 => {
                let Field { ident, ty, .. } = &named[0];
                (quote!({ #ident: input }), ty)
            }
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) if unnamed.len() == 1 => {
                let Field { ty, .. } = &unnamed[0];
                (quote!((input)), ty)
            }
            _ => continue,
        };
        impls.push(quote!(
            impl From<#ty> for #item_ident {
                fn from(input: #ty) -> Self {
                    Self::#var_ident #arg_teil
                }
            }
        ));
    }

    quote!(#(#impls)*)
}
