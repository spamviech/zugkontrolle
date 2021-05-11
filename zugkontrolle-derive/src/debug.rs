//! Implementation of the Debug-Trait without requirement for generic parameters

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn;

pub fn impl_debug(ast: &syn::DeriveInput) -> TokenStream {
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
                    write!(f, "{} (", #ident_str)?;
                    #(write!(f, "{:?}, ", #fs_str)?);*;
                    write!(f, ")")
                }
            },
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
                                    write!(f, "{} (", #variant_ident_str)?;
                                    #(write!(f, "{:?}, ", #fs_str)?);*;
                                    write!(f, ")")
                                }
                            }
                        },
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
        impl<#(#generic_lifetimes),* #(#generic_types),*> std::fmt::Debug for #ident<#(#generic_lifetimes),* #(#generic_types),*> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #fmt
            }
        }
    }
}