//! Implementation of the Clone-Trait without requirement for generic parameters.

use std::collections::HashMap;
use std::iter;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::utils::{mark_fields_generic, parse_attributes};

pub(crate) fn impl_clone(ast: &syn::DeriveInput) -> TokenStream {
    let syn::DeriveInput { ident, data, generics, attrs, .. } = ast;

    let where_predicates = parse_attributes!(attrs, "zugkontrolle_clone");
    let mut where_clause = generics.where_clause.clone().unwrap_or(syn::WhereClause {
        where_token: syn::Token!(where)(proc_macro2::Span::call_site()),
        predicates: syn::punctuated::Punctuated::new(),
    });
    where_clause.predicates.extend(where_predicates);

    let mut generic_lifetimes = Vec::new();
    let mut generic_types = HashMap::new();
    let mut generic_type_names = Vec::new();
    for g in generics.params.iter() {
        match g {
            syn::GenericParam::Lifetime(lt) => generic_lifetimes.push(lt),
            syn::GenericParam::Type(ty) => {
                generic_type_names.push(&ty.ident);
                let _ = generic_types.insert(&ty.ident, (&ty.bounds, false));
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
                        #(#fs_vec: #fs_vec.clone()),*
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
                    #ident (#(#fs_str.clone()),*)
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
                                    #(#fs_vec: #fs_vec.clone()),*
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
                                #ident::#variant_ident (#(#fs_str.clone()),*)
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
            };
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

    let generic_constraints;
    let generic_names;
    if generic_lifetimes.is_empty() {
        generic_constraints = quote! {#(#generic_type_constraints),*};
        generic_names = quote! {#(#generic_type_names),*};
    } else {
        generic_constraints = quote! {#(#generic_lifetimes),*, #(#generic_type_constraints),*};
        generic_names = quote! {#(#generic_lifetimes),*, #(#generic_type_names),*};
    };
    quote! {
        #[allow(single_use_lifetimes)]
        impl<#generic_constraints> Clone for #ident<#generic_names> #where_clause  {
            fn clone(&self) -> Self {
                #clone
            }
        }
    }
}
