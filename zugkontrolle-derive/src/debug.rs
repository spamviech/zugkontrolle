//! Implementation of the Debug-Trait without requirement for generic parameters

use std::collections::HashMap;
use std::iter;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::utils::mark_fields_generic;

pub(crate) fn impl_debug(ast: &syn::DeriveInput) -> TokenStream {
    let syn::DeriveInput { ident, data, generics, .. } = ast;

    let mut generic_lifetimes = Vec::new();
    let mut generic_types = HashMap::new();
    let mut generic_type_names = Vec::new();
    let where_clause = &generics.where_clause;
    for g in generics.params.iter() {
        match g {
            syn::GenericParam::Lifetime(lt) => generic_lifetimes.push(lt),
            syn::GenericParam::Type(ty) => {
                generic_type_names.push(&ty.ident);
                let _ = generic_types.insert(&ty.ident, (&ty.bounds, false));
            }
            syn::GenericParam::Const(_c) => {}
        }
    }

    // body of the fmt method
    let ident_str = ident.to_string();
    let fmt = match data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => match fields {
            syn::Fields::Named(syn::FieldsNamed { named, .. }) => {
                mark_fields_generic(named.iter(), &mut generic_types);
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
                            mark_fields_generic(named.iter(), &mut generic_types);
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
            let error = format!("Unsupported data! Given ast: {:?}", ast);
            return quote! {
                compile_error!(#error)
            };
        }
    };

    // map from generic_type_names to preserve order!
    let generic_type_constraints = generic_type_names.iter().map(|ty| {
        if let Some((ty_bounds, debug_bound)) = generic_types.get(ty) {
            let ty_bounds_iter = ty_bounds.iter().map(|bound| quote!(#bound));
            let bounds: Vec<_> = if *debug_bound {
                ty_bounds_iter.chain(iter::once(quote!(std::fmt::Debug))).collect()
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
        impl<#generic_constraints> std::fmt::Debug for #ident<#generic_names> #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #fmt
            }
        }
    }
}
