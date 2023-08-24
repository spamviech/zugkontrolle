//! Erzeuge From-Implementierung für alle Varianten eines Enums, die genau ein Element halten.

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Field, Fields, FieldsNamed, FieldsUnnamed, GenericParam, ItemEnum};

pub(crate) fn impl_from(item: ItemEnum) -> TokenStream {
    let mut impls = Vec::new();
    let item_ident = item.ident;
    let item_generics = item.generics;
    let item_ty_generics = {
        let mut generics = item_generics.clone();
        for param in generics.params.iter_mut() {
            match param {
                GenericParam::Type(ty) => ty.bounds.clear(),
                GenericParam::Lifetime(lt) => lt.bounds.clear(),
                GenericParam::Const(con) => con.default = None,
            }
        }
        generics.where_clause = None;
        generics
    };
    for variant in item.variants {
        let var_ident = variant.ident;
        let (arg_teil, idents, types) = match &variant.fields {
            Fields::Named(FieldsNamed { named, .. }) => {
                let mut idents = Vec::new();
                let mut types = Vec::new();
                for Field { ident, ty, .. } in named {
                    idents.push(quote!(#ident));
                    types.push(ty);
                }
                (quote!({ #(#idents),* }), idents, types)
            },
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                let mut idents = Vec::new();
                let mut types = Vec::new();
                for (i, Field { ty, .. }) in unnamed.into_iter().enumerate() {
                    let ident = syn::Ident::new(&format!("arg{i}"), Span::call_site());
                    idents.push(quote!(#ident));
                    types.push(ty);
                }
                (quote!(( #(#idents),* )), idents, types)
            },
            _ => continue,
        };
        let ty = quote!((#(#types),*));
        impls.push(quote!(
            #[allow(unused_qualifications)]
            impl #item_generics From<#ty> for #item_ident #item_ty_generics {
                fn from((#(#idents),*): #ty) -> Self {
                    Self::#var_ident #arg_teil
                }
            }
        ));
    }

    quote!(#(#impls)*)
}
