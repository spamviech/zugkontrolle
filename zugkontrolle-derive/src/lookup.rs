//! Derive of zugkontrolle::gleis::anchor::lookup::Lookup from an enum
//! by creating an associated AnchorPoints struct

use inflector::cases::snakecase::to_snake_case;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};

pub fn impl_lookup(args: Vec<syn::NestedMeta>, item: syn::ItemEnum) -> TokenStream {
    let mut errors = Vec::new();

    let element = if let Some((arg, [])) = args.split_first() {
        Some(arg)
    } else {
        if args.is_empty() {
            errors.push("Lookup Element missing!".to_string())
        } else if args.len() > 1 {
            errors.push(format!("Only one argument supported, but {:?} were given.", args));
        }
        None
    };

    let mut struct_definition = None;
    let mut impl_lookup = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident: syn::Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };

        let syn::ItemEnum { vis, variants, ident, .. } = &item;
        let enum_variants: Vec<syn::Ident> = variants.iter().map(|v| v.ident.clone()).collect();

        // construct a struct using a snake_case field for every variant, each holding an Element
        let struct_name: syn::Ident =
            format_ident!("{}Elements", ident.to_string().trim_end_matches("Name"));
        let struct_fields: Vec<syn::Ident> = enum_variants
                .iter()
                // TODO fix upstream?
                // to_snakecase wrongly adds a '_' before 'ß', even though it it a small letter
                // possibly because there is no real uppercase character of it
                .map(|variant| format_ident!("{}", to_snake_case(&variant.to_string()).replace("_ß", "ß")))
                .collect();
        struct_definition = Some(quote! {
            #[derive(Debug)]
            #vis struct #struct_name {
                #(pub #struct_fields : #element),*
            }
        });
        impl_lookup = Some(quote! {
            impl #base_ident::lookup::Lookup<#ident, #element> for #struct_name {
                fn get(&self, key: &#ident) -> &#element {
                    match key {
                        #(#ident::#enum_variants => &self.#struct_fields),*
                    }
                }
                fn get_mut(&mut self, key: &#ident) -> &mut #element {
                    match key {
                        #(#ident::#enum_variants => &mut self.#struct_fields),*
                    }
                }
                fn for_each<F: FnMut(#ident, &#element)>(&self, mut action: F) {
                    #(action(#ident::#enum_variants, &self.#struct_fields));*
                }
                fn map<F: Fn(&#element)->#element>(&self, mut action: F) -> Self {
                    #struct_name {
                        #(#struct_fields: action(&self.#struct_fields)),*
                    }
                }
            }
        });
    } else {
        errors.push("`zugkontrolle` missing in `Cargo.toml`".to_string())
    }

    if !errors.is_empty() {
        let error_message = errors.join("\n");
        return quote! {
            compile_error!(#error_message);
            #item
        }
    }

    quote! {
        #item
        #struct_definition
        #impl_lookup
    }
}
