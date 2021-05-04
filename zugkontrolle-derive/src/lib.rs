use inflector::cases::snakecase::to_snake_case;
use proc_macro;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn;

#[proc_macro_derive(Debug)]
pub fn debug_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).expect("Failed to parse input!");

    // Build the trait implementation
    impl_debug(&ast)
}
fn impl_debug(ast: &syn::DeriveInput) -> proc_macro::TokenStream {
    let syn::DeriveInput { ident, data, generics, .. } = ast;

    // body of the fmt method
    let ident_str = ident.to_string();
    let fmt = match data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => match fields {
            syn::Fields::Named(syn::FieldsNamed { named, .. }) => {
                let fs_iter = named.iter().map(|field| &field.ident);
                let fs_vec: Vec<&Option<syn::Ident>> = fs_iter.clone().collect();
                let fs_str = fs_vec.iter().map(|mid| match mid {
                    Some(id) => id.to_string() + ": ",
                    None => String::new(),
                });
                quote! {
                    let #ident {#(#fs_iter),*} = self;
                    write!(f, "{} {{", #ident_str)?;
                    #(write!(f, "{}{:?}, ", #fs_str, #fs_vec)?);*;
                    write!(f, "}}")
                }
            }
            syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed, .. }) => {
                let fs_iter = unnamed.iter().map(|field| &field.ident);
                let mut i: usize = 0;
                let fs_str: Vec<syn::Ident> = fs_iter
                    .map(|_| {
                        i += 1;
                        format_ident!("i{}", i)
                    })
                    .collect();
                quote! {
                    let #ident (#(#fs_str),*) = self;
                    write!(f, "{} (", #ident_str)?;
                    #(write!(f, "{:?}, ", #fs_str)?);*;
                    write!(f, ")")
                }
            }
            syn::Fields::Unit => quote! {
                write!(f, "{}", #ident_str)?;
            },
        },
        syn::Data::Enum(syn::DataEnum { variants, .. }) => {
            let token_streams: Vec<TokenStream> = variants
                .iter()
                .map(|syn::Variant { ident: variant_ident, fields, .. }| {
                    let variant_ident_str = variant_ident.to_string();
                    match fields {
                        syn::Fields::Named(syn::FieldsNamed { named, .. }) => {
                            let fs_iter = named.iter().map(|field| &field.ident);
                            let fs_vec: Vec<&Option<syn::Ident>> = fs_iter.clone().collect();
                            let fs_str = fs_vec.iter().map(|mid| match mid {
                                Some(id) => id.to_string() + ": ",
                                None => String::new(),
                            });
                            quote! {
                                #ident::#variant_ident {#(#fs_iter),*} => {
                                    write!(f, "{} {{", #variant_ident_str)?;
                                    #(write!(f, "{}{:?}, ", #fs_str, #fs_vec)?);*;
                                    write!(f, "}}")
                                }
                            }
                        }
                        syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed, .. }) => {
                            let fs_iter = unnamed.iter().map(|field| &field.ident);
                            let mut i: usize = 0;
                            let fs_str: Vec<syn::Ident> = fs_iter
                                .map(|_| {
                                    i += 1;
                                    format_ident!("i{}", i)
                                })
                                .collect();
                            quote! {
                                #ident::#variant_ident (#(#fs_str),*) => {
                                    write!(f, "{} (", #variant_ident_str)?;
                                    #(write!(f, "{:?}, ", #fs_str)?);*;
                                    write!(f, ")")
                                }
                            }
                        }
                        syn::Fields::Unit => quote! {
                            #ident::#variant_ident  => write!(f, "{}", #variant_ident_str)
                        },
                    }
                })
                .collect();
            quote! {
                write!(f, "{}::", #ident_str)?;
                match self {
                    #(#token_streams),*
                }
            }
        }
        _ => {
            unimplemented!("Unsupported data! Given ast: {:?}", ast)
        }
    };

    let mut generic_lifetimes = Vec::new();
    let mut generic_types = Vec::new();
    for g in generics.params.iter() {
        match g {
            syn::GenericParam::Lifetime(lt) => generic_lifetimes.push(&lt.lifetime),
            syn::GenericParam::Type(ty) => generic_types.push(&ty.ident),
            syn::GenericParam::Const(_c) => {}
        }
    }

    let gen: TokenStream = quote! {
        impl<#(#generic_lifetimes),* #(#generic_types),*> std::fmt::Debug for #ident<#(#generic_lifetimes),* #(#generic_types),*> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #fmt
            }
        }
    };
    gen.into()
}
#[proc_macro_derive(Clone)]
pub fn clone_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).expect("Failed to parse input!");

    // Build the trait implementation
    impl_clone(&ast)
}
fn impl_clone(ast: &syn::DeriveInput) -> proc_macro::TokenStream {
    let syn::DeriveInput { ident, data, generics, .. } = ast;

    // body of the clone method
    let clone = match data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => match fields {
            syn::Fields::Named(syn::FieldsNamed { named, .. }) => {
                let fs_iter = named.iter().map(|field| &field.ident);
                let fs_vec: Vec<&Option<syn::Ident>> = fs_iter.clone().collect();
                quote! {
                    let #ident {#(#fs_iter),*} = self;
                    #ident {
                        #(#fs_vec: Clone::clone(#fs_vec)),*
                    }
                }
            }
            syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed, .. }) => {
                let fs_iter = unnamed.iter().map(|field| &field.ident);
                let mut i: usize = 0;
                let fs_str: Vec<syn::Ident> = fs_iter
                    .map(|_| {
                        i += 1;
                        format_ident!("i{}", i)
                    })
                    .collect();
                quote! {
                    let #ident (#(#fs_str),*) = self;
                    #ident (#(Clone::clone(#fs_str)),*)
                }
            }
            syn::Fields::Unit => quote! {#ident},
        },
        syn::Data::Enum(syn::DataEnum { variants, .. }) => {
            let token_streams: Vec<TokenStream> = variants
                .iter()
                .map(|syn::Variant { ident: variant_ident, fields, .. }| match fields {
                    syn::Fields::Named(syn::FieldsNamed { named, .. }) => {
                        let fs_iter = named.iter().map(|field| &field.ident);
                        let fs_vec: Vec<&Option<syn::Ident>> = fs_iter.clone().collect();
                        quote! {
                            #ident::#variant_ident {#(#fs_iter),*} => {
                                #ident::#variant_ident {
                                    #(#fs_vec: Clone::clone(#fs_vec)),*
                                }
                            }
                        }
                    }
                    syn::Fields::Unnamed(syn::FieldsUnnamed { unnamed, .. }) => {
                        let fs_iter = unnamed.iter().map(|field| &field.ident);
                        let mut i: usize = 0;
                        let fs_str: Vec<syn::Ident> = fs_iter
                            .map(|_| {
                                i += 1;
                                format_ident!("i{}", i)
                            })
                            .collect();
                        quote! {
                            #ident::#variant_ident (#(#fs_str),*) => {
                                #ident::#variant_ident (#(Clone::clone(#fs_str)),*)
                            }
                        }
                    }
                    syn::Fields::Unit => quote! {#ident::#variant_ident => #ident::#variant_ident},
                })
                .collect();
            quote! {
                match self {
                    #(#token_streams),*
                }
            }
        }
        _ => {
            unimplemented!("Unsupported data! Given ast: {:?}", ast)
        }
    };

    let mut generic_lifetimes = Vec::new();
    let mut generic_types = Vec::new();
    for g in generics.params.iter() {
        match g {
            syn::GenericParam::Lifetime(lt) => generic_lifetimes.push(&lt.lifetime),
            syn::GenericParam::Type(ty) => generic_types.push(&ty.ident),
            syn::GenericParam::Const(_c) => {}
        }
    }

    let gen: TokenStream = quote! {
        impl<#(#generic_lifetimes),* #(#generic_types),*> Clone for #ident<#(#generic_lifetimes),* #(#generic_types),*> {
            fn clone(&self) -> Self {
                #clone
            }
        }
    };
    gen.into()
}

#[proc_macro_derive(Lookup)]
pub fn anchor_lookup_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).expect("Failed to parse input!");

    // Build the trait implementation
    impl_anchor_lookup(&ast)
}

fn impl_anchor_lookup(ast: &syn::DeriveInput) -> proc_macro::TokenStream {
    let syn::DeriveInput { ident, data, vis, .. } = ast;
    let gen: TokenStream = if let syn::Data::Enum(enum_data) = data {
        let base_ident: syn::Ident =
            match crate_name("zugkontrolle").expect("zugkontrolle missing in `Cargo.toml`") {
                FoundCrate::Itself => format_ident!("{}", "crate"),
                FoundCrate::Name(name) => format_ident!("{}", name),
            };
        let enum_name: &syn::Ident = ident;
        let enum_vis: &syn::Visibility = vis;
        let enum_variants: Vec<syn::Ident> =
            enum_data.variants.iter().map(|v| v.ident.clone()).collect();
        // construct a struct using a snake_case field for every variant, each holding an anchor::Anchor
        let struct_name: syn::Ident =
            format_ident!("{}Points", enum_name.to_string().trim_end_matches("Name"));
        let struct_fields: Vec<syn::Ident> = enum_variants
            .iter()
            .map(|ident| format_ident!("{}", to_snake_case(&ident.to_string())))
            .collect();
        let struct_definition: TokenStream = quote! {
            #[derive(Debug)]
            #enum_vis struct #struct_name {
                #(pub #struct_fields : #base_ident::gleis::anchor::Anchor),*
            }
        };
        let impl_lookup: TokenStream = quote! {
            impl #base_ident::gleis::anchor::Lookup<#enum_name> for #struct_name {
                fn get(&self, key: #enum_name) -> &#base_ident::gleis::anchor::Anchor {
                    match key {
                        #(#enum_name::#enum_variants => &self.#struct_fields),*
                    }
                }
                fn get_mut(&mut self, key: #enum_name) -> &mut #base_ident::gleis::anchor::Anchor {
                    match key {
                        #(#enum_name::#enum_variants => &mut self.#struct_fields),*
                    }
                }
                fn foreach<F: FnMut(#enum_name, &#base_ident::gleis::anchor::Anchor)>(&self, mut action: F) {
                    #(action(#enum_name::#enum_variants, &self.#struct_fields));*
                }
                fn map<F: Fn(&#base_ident::gleis::anchor::Anchor)->#base_ident::gleis::anchor::Anchor>(&self, mut action: F) -> Self {
                    #struct_name {
                        #(#struct_fields: action(&self.#struct_fields)),*
                    }
                }
            }
        };
        quote! {
            #struct_definition
            #impl_lookup
        }
    } else {
        panic!("Not an enum: {:?}", ast)
    };
    gen.into()
}
