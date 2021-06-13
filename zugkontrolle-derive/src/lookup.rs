//! Derive of zugkontrolle::lookup::Lookup from an enum by creating an associated Elements struct

use inflector::cases::snakecase::to_snake_case;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};

pub fn impl_lookup(args: Vec<syn::NestedMeta>, item: syn::ItemEnum) -> TokenStream {
    let mut errors = Vec::new();

    let syn::ItemEnum { vis, variants, ident, .. } = &item;
    let dummy = Vec::new();
    let (element, struct_name, derives) = if args.len() < 2 {
        errors.push(
            if args.is_empty() {
                "Lookup Element and Name suffix missing!".to_string()
            } else {
                "Name suffix missing!".to_string()
            },
        );
        (None, None, dummy.iter().skip(0))
    } else {
        let fst = &args[0];
        let snd = &args[1];
        let derives = args.iter().skip(2);
        let suffix = quote!(#snd).to_string();
        (
            Some(fst),
            Some(format_ident!("{}{}", ident.to_string().trim_end_matches("Name"), suffix)),
            derives,
        )
    };

    let mut struct_definition = None;
    let mut impl_lookup = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident: syn::Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };

        let enum_variants: Vec<syn::Ident> = variants.iter().map(|v| v.ident.clone()).collect();

        let struct_fields: Vec<syn::Ident> = enum_variants
                .iter()
                // TODO fix upstream?
                // to_snakecase wrongly adds a '_' before 'ß', even though it it a small letter
                // possibly because there is no real uppercase character of it
                .map(|variant| format_ident!("{}", to_snake_case(&variant.to_string()).replace("_ß", "ß")))
                .collect();
        struct_definition = Some(quote! {
            #[derive(#(#derives),*)]
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
                fn mut_refs<'t>(&'t mut self) -> Vec<(#ident, &'t mut #element)> {
                    let #struct_name { #(#struct_fields),* } = self;
                    vec![ #((#ident::#enum_variants, #struct_fields)),* ]
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
