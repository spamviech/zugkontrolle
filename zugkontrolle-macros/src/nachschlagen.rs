//! Derive of `zugkontrolle::lookup::Lookup` from an enum by creating an associated Elements struct

use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{punctuated::Punctuated, token::Comma, ItemEnum, Path};

/// [`crate::nachschlagen`]
pub(crate) fn impl_nachschlagen(args: &Punctuated<Path, Comma>, item: &ItemEnum) -> TokenStream {
    let mut errors = Vec::new();

    let ItemEnum { vis, variants, ident, .. } = &item;
    let dummy: Punctuated<Path, Comma> = Punctuated::new();
    let (element, struct_name, derives) = if args.len() < 2 {
        errors.push(if args.is_empty() {
            String::from("Lookup Element and Collection missing!")
        } else {
            String::from("Collection missing!")
        });
        // Skip, damit die Iterator-Typen übereinstimmen.
        #[allow(clippy::iter_skip_zero)]
        (None, None, dummy.iter().skip(0))
    } else {
        // sichergestellt durch `arg.len() < 2` check
        #[allow(clippy::indexing_slicing)]
        let fst = &args[0];
        #[allow(clippy::indexing_slicing)]
        let snd = &args[1];
        let derives = args.iter().skip(2);
        (Some(fst), Some(snd), derives)
    };

    let mut struct_definition = None;
    let mut impl_lookup = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident: syn::Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };

        let enum_variants: Vec<syn::Ident> =
            variants.iter().map(|variant| variant.ident.clone()).collect();

        let struct_fields: Vec<syn::Ident> = enum_variants
            .iter()
            .map(|variant| format_ident!("{}", &variant.to_string().to_snake_case()))
            .collect();
        let docstring = format!("Eine Struktur mit von [{ident}]-Varianten abgeleiteten Felder.");
        let field_docstrings = enum_variants.iter().map(|variant| format!("[{ident}::{variant}]"));
        struct_definition = Some(quote! {
            #[doc = #docstring]
            #[derive(#(#derives),*)]
            #vis struct #struct_name {
                #(
                    #[doc = #field_docstrings]
                    pub #struct_fields : #element
                ),*
            }
        });
        impl_lookup = Some(quote! {
            impl #base_ident::util::nachschlagen::Nachschlagen<#ident, #element> for #struct_name {
                fn erhalte(&self, key: &#ident) -> &#element {
                    match key {
                        #(#ident::#enum_variants => &self.#struct_fields),*
                    }
                }
                fn erhalte_mut(&mut self, key: &#ident) -> &mut #element {
                    match key {
                        #(#ident::#enum_variants => &mut self.#struct_fields),*
                    }
                }
                fn für_alle<F: FnMut(#ident, &#element)>(&self, mut action: F) {
                    #(action(#ident::#enum_variants, &self.#struct_fields));*
                }
                fn zuordnen<F: Fn(&#element)->#element>(&self, mut action: F) -> Self {
                    #struct_name {
                        #(#struct_fields: action(&self.#struct_fields)),*
                    }
                }
                fn referenzen<'t>(&'t self) -> Vec<(#ident, &'t #element)> {
                    let #struct_name { #(#struct_fields),* } = self;
                    vec![ #((#ident::#enum_variants, #struct_fields)),* ]
                }
                fn referenzen_mut<'t>(&'t mut self) -> Vec<(#ident, &'t mut #element)> {
                    let #struct_name { #(#struct_fields),* } = self;
                    vec![ #((#ident::#enum_variants, #struct_fields)),* ]
                }
            }
        });
    } else {
        errors.push(String::from("`zugkontrolle` missing in `Cargo.toml`"));
    }

    if !errors.is_empty() {
        let error_message = errors.join("\n");
        return quote! {
            compile_error!(#error_message);
            #item
        };
    }

    quote! {
        #item
        #struct_definition
        #impl_lookup
    }
}
