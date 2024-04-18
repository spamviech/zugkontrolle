//! Implementation of the Clone-Trait without requirement for generic parameters.

use std::iter;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    punctuated::{self},
    Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Ident, Token,
    Variant, WhereClause,
};

use crate::util::{
    mark_fields_generic, parse_attributes, partitioniere_generics, PartitionierteGenericParameter,
};

/// Erzeuge den Funktions-Körper (die Implementierung) für die `clone`-Methode.
fn erzeuge_body(
    ast: &DeriveInput,
    ident: &Ident,
    data: &Data,
    generics: &mut PartitionierteGenericParameter<'_>,
) -> TokenStream {
    match data {
        Data::Struct(DataStruct { fields, .. }) => match fields {
            Fields::Named(FieldsNamed { named, .. }) => {
                mark_fields_generic(named.iter(), &mut generics.types);
                let fs_iter = named.iter().map(|field| &field.ident);
                let fs_vec: Vec<&Option<Ident>> = fs_iter.clone().collect();
                quote! {
                    let #ident {#(#fs_iter),*} = self;
                    #ident {
                        #(#fs_vec: #fs_vec.clone()),*
                    }
                }
            },
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                mark_fields_generic(unnamed.iter(), &mut generics.types);
                let fs_iter = unnamed.iter().map(|field| &field.ident);
                let fs_str: Vec<Ident> =
                    fs_iter.enumerate().map(|(i, _)| format_ident!("i{}", i)).collect();
                quote! {
                    let #ident (#(#fs_str),*) = self;
                    #ident (#(#fs_str.clone()),*)
                }
            },
            Fields::Unit => quote! {#ident},
        },
        Data::Enum(DataEnum { variants, .. }) => {
            let token_streams: Vec<TokenStream> = variants
                .iter()
                .map(|Variant { ident: variant_ident, fields, .. }| match fields {
                    Fields::Named(FieldsNamed { named, .. }) => {
                        mark_fields_generic(named.iter(), &mut generics.types);
                        let fs_iter = named.iter().map(|field| &field.ident);
                        let fs_vec: Vec<&Option<Ident>> = fs_iter.clone().collect();
                        quote! {
                            #ident::#variant_ident {#(#fs_iter),*} => {
                                #ident::#variant_ident {
                                    #(#fs_vec: #fs_vec.clone()),*
                                }
                            }
                        }
                    },
                    Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                        mark_fields_generic(unnamed.iter(), &mut generics.types);
                        let fs_iter = unnamed.iter().map(|field| &field.ident);
                        let fs_str: Vec<Ident> =
                            fs_iter.enumerate().map(|(i, _)| format_ident!("i{}", i)).collect();
                        quote! {
                            #ident::#variant_ident (#(#fs_str),*) => {
                                #ident::#variant_ident (#(#fs_str.clone()),*)
                            }
                        }
                    },
                    Fields::Unit => quote! {#ident::#variant_ident => #ident::#variant_ident},
                })
                .collect();
            quote! {
                match self {
                    #(#token_streams),*
                }
            }
        },
        Data::Union(_) => {
            let error = format!("Unsupported data! Given ast: {ast:?}");
            quote! {
                compile_error!(#error)
            }
        },
    }
}

/// [`crate::clone`]
pub(crate) fn impl_clone(ast: &DeriveInput) -> TokenStream {
    let DeriveInput { ident, data, generics, attrs, .. } = ast;

    let where_predicates = parse_attributes!(attrs, "zugkontrolle_clone");
    let mut where_clause = generics.where_clause.clone().unwrap_or(WhereClause {
        where_token: Token!(where)(proc_macro2::Span::call_site()),
        predicates: punctuated::Punctuated::new(),
    });
    where_clause.predicates.extend(where_predicates);

    let mut partitionierte_generic_parameter = partitioniere_generics(generics);

    // body of the clone method
    let clone = erzeuge_body(ast, ident, data, &mut partitionierte_generic_parameter);

    let PartitionierteGenericParameter {
        lifetimes: generic_lifetimes,
        types: generic_types,
        type_names: generic_type_names,
    } = partitionierte_generic_parameter;

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

        impl<#generic_constraints> Clone for #ident<#generic_names> #where_clause  {
            fn clone(&self) -> Self {
                #clone
            }
        }
    }
}
