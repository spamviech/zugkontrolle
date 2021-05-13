//! Implementation of the Clone-Trait without requirement for generic parameters

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub fn impl_clone(ast: &syn::DeriveInput) -> TokenStream {
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
            },
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
            },
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
                    },
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
                    },
                    syn::Fields::Unit => quote! {#ident::#variant_ident => #ident::#variant_ident},
                })
                .collect();
            quote! {
                match self {
                    #(#token_streams),*
                }
            }
        },
        _ => {
            unimplemented!("Unsupported data! Given ast: {:?}", ast)
        },
    };

    let mut generic_lifetimes = Vec::new();
    let mut generic_types = Vec::new();
    for g in generics.params.iter() {
        match g {
            syn::GenericParam::Lifetime(lt) => generic_lifetimes.push(&lt.lifetime),
            syn::GenericParam::Type(ty) => generic_types.push(&ty.ident),
            syn::GenericParam::Const(_c) => {},
        }
    }

    quote! {
        impl<#(#generic_lifetimes),* #(#generic_types),*> Clone for #ident<#(#generic_lifetimes),* #(#generic_types),*> {
            fn clone(&self) -> Self {
                #clone
            }
        }
    }
}
