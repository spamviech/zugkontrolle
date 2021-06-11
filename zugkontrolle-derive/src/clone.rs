//! Implementation of the Clone-Trait without requirement for generic parameters

use std::collections::HashMap;
use std::iter;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::utils::mark_fields_generic;

pub fn impl_clone(ast: &syn::DeriveInput) -> TokenStream {
    let syn::DeriveInput { ident, data, generics, .. } = ast;

    let mut generic_lifetimes = Vec::new();
    let mut generic_types = HashMap::new();
    let mut generic_type_names = Vec::new();
    for g in generics.params.iter() {
        match g {
            syn::GenericParam::Lifetime(lt) => generic_lifetimes.push(&lt.lifetime),
            syn::GenericParam::Type(ty) => {
                generic_type_names.push(&ty.ident);
                generic_types.insert(&ty.ident, (&ty.bounds, false));
            },
            syn::GenericParam::Const(_c) => {},
        }
    }

    // body of the clone method
    let clone = match data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => match fields {
            syn::Fields::Named(syn::FieldsNamed { named, .. }) => {
                mark_fields_generic(named.iter(), &mut generic_types);
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
                mark_fields_generic(unnamed.iter(), &mut generic_types);
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
                        mark_fields_generic(named.iter(), &mut generic_types);
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
                        mark_fields_generic(unnamed.iter(), &mut generic_types);
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
            let error = format!("Unsupported data! Given ast: {:?}", ast);
            return quote! {
                compile_error!(#error)
            }
        },
    };

    // map from generic_type_names to preserve order!
    let generic_type_constraints = generic_type_names.iter().map(|ty| {
        if let Some((ty_bounds, debug_clone)) = generic_types.get(ty) {
            let ty_bounds_iter = ty_bounds.iter().map(|bound| quote!(#bound));
            let bounds: Vec<_> = if *debug_clone {
                ty_bounds_iter.chain(iter::once(quote!(Clone))).collect()
            } else {
                ty_bounds_iter.collect()
            };
            quote!(#ty: #(#bounds),*)
        } else {
            quote!(#ty)
        }
    });

    quote! {
        impl<#(#generic_lifetimes),* #(#generic_type_constraints),*> Clone for #ident<#(#generic_lifetimes),* #(#generic_type_names),*> {
            fn clone(&self) -> Self {
                #clone
            }
        }
    }
}
