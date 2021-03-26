use inflector::cases::snakecase::to_snake_case;
use proc_macro;
use proc_macro::TokenStream;
use proc_macro2;
use quote::{format_ident, quote};
use syn;

/// For external use
#[proc_macro_derive(AnchorLookup)]
pub fn anchor_lookup_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).expect("Failed to parse input!");

    // Build the trait implementation
    impl_anchor_lookup(&ast, "zugkontrolle")
}

/// For internal use only, uses /crate/ as basename
#[proc_macro_derive(AnchorLookupCrate)]
pub fn anchor_lookup_crate_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).expect("Failed to parse input!");

    // Build the trait implementation
    impl_anchor_lookup(&ast, "crate")
}

fn impl_anchor_lookup(ast: &syn::DeriveInput, basename: &str) -> TokenStream {
    let gen: proc_macro2::TokenStream;
    if let syn::Data::Enum(enum_data) = &ast.data {
        let base_ident: syn::Ident = format_ident!("{}", basename);
        let enum_name: &syn::Ident = &ast.ident;
        let enum_vis: &syn::Visibility = &ast.vis;
        let enum_variants: Vec<syn::Ident> =
            enum_data.variants.iter().map(|v| v.ident.clone()).collect();
        // construct a struct using a snake_case field for every variant, each holding an anchor::point
        let struct_name: syn::Ident = format_ident!(
            "{}",
            match enum_name.to_string().as_str() {
                "AnchorName" => "AnchorPoints".to_string(),
                s => "AnchorPoints".to_string() + s,
            }
        );
        let struct_fields: Vec<syn::Ident> = enum_variants
            .iter()
            .map(|ident| format_ident!("{}", to_snake_case(&ident.to_string())))
            .collect();
        // TODO check if this actually works
        // especially: derive and #enum_vis (for private, i.e. no explicit visibility)
        let struct_definition: proc_macro2::TokenStream = quote! {
            #[derive(Debug)]
            #enum_vis struct #struct_name {
                #(#struct_fields : #base_ident::gleis::anchor::Point),*
            }
        };
        // TODO check if we need to qualify idents here
        // format_ident!("{}::{}", #enum_name,#enum_variants)
        let impl_lookup: proc_macro2::TokenStream = quote! {
            impl #base_ident::gleis::widget::AnchorLookup<#enum_name> for #struct_name {
                fn get(&self, key: #enum_name) -> &#base_ident::gleis::anchor::Point {
                    match key {
                        #(#enum_name::#enum_variants => &self.#struct_fields),*
                    }
                }
                fn get_mut(&mut self, key: #enum_name) -> &mut #base_ident::gleis::anchor::Point {
                    match key {
                        #(#enum_name::#enum_variants => &mut self.#struct_fields),*
                    }
                }
                fn map<F: FnMut(&#base_ident::gleis::anchor::Point)>(&self, mut action: F) {
                    #(action(&self.#struct_fields));*
                }
            }
        };
        gen = quote! {
            #struct_definition
            #impl_lookup
        };
    } else {
        panic!("Not an enum: {:?}", ast)
    }
    gen.into()
}
