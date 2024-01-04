//! Implementation of the Debug-Trait without requirement for generic parameters.

use std::iter;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    punctuated, Data, DataEnum, DataStruct, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Ident,
    Index, Token, Variant, WhereClause,
};

use crate::utils::{
    mark_fields_generic, parse_attributes, partitioniere_generics, PartitionierteGenericParameter,
};

/// Erzeuge den Funktions-Körper (die Implementierung) für die `fmt`-Methode.
fn erzeuge_body(
    ast: &DeriveInput,
    ident: &Ident,
    data: &Data,
    generics: &mut PartitionierteGenericParameter<'_>,
) -> TokenStream {
    let ident_str = ident.to_string();
    match data {
        Data::Struct(DataStruct { fields, .. }) => match fields {
            Fields::Named(FieldsNamed { named, .. }) => {
                mark_fields_generic(named.iter(), &mut generics.types);
                let fs_iter = named.iter().map(|field| &field.ident);
                let fs_str = fs_iter.clone().map(|mid| match mid {
                    Some(id) => format!("{id}: "),
                    None => String::new(),
                });
                quote! {
                    f.debug_struct(#ident_str)
                        #(.field(#fs_str, &self.#fs_iter))*
                        .finish()
                }
            },
            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                mark_fields_generic(unnamed.iter(), &mut generics.types);
                let range = (0..unnamed.len()).map(Index::from);
                quote! {
                    f.debug_tuple(#ident_str)
                        #(.field(&self.#range))*
                        .finish()
                }
            },
            Fields::Unit => quote! {
                write!(f, "{}", #ident_str)?;
            },
        },
        Data::Enum(DataEnum { variants, .. }) => {
            let token_streams: Vec<TokenStream> = variants
                .iter()
                .map(|Variant { ident: variant_ident, fields, .. }| {
                    let variant_ident_str = variant_ident.to_string();
                    match fields {
                        Fields::Named(FieldsNamed { named, .. }) => {
                            mark_fields_generic(named.iter(), &mut generics.types);
                            let fs_iter = named.iter().map(|field| &field.ident);
                            let fs_vec: Vec<&Option<Ident>> = fs_iter.collect();
                            let fs_str = fs_vec.iter().map(|mid| match mid {
                                Some(id) => format!("{id}: "),
                                None => String::new(),
                            });
                            quote! {
                                #ident_str::#variant_ident {#(#fs_vec),*} => {
                                    f.debug_struct(#variant_ident_str)
                                        #(.field(#fs_str, #fs_vec))*
                                        .finish()
                                }
                            }
                        },
                        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                            mark_fields_generic(unnamed.iter(), &mut generics.types);
                            let fs_iter = unnamed.iter().map(|field| &field.ident);
                            let fs_str: Vec<Ident> =
                                fs_iter.enumerate().map(|(i, _)| format_ident!("i{}", i)).collect();
                            quote! {
                                #ident_str::#variant_ident (#(#fs_str),*) => {
                                    f.debug_tuple(#ident_str)
                                        #(.field(#fs_str))*
                                        .finish()
                                }
                            }
                        },
                        Fields::Unit => quote! {
                            #ident_str::#variant_ident  => write!(f, "{}", #variant_ident_str)
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
        Data::Union(_) => {
            let error = format!("Unsupported data! Given ast: {ast:?}");
            quote! {
                compile_error!(#error)
            }
        },
    }
}

/// [`crate::debug`]
pub(crate) fn impl_debug(ast: &DeriveInput) -> TokenStream {
    let DeriveInput { ident, data, generics, attrs, .. } = ast;

    let where_predicates = parse_attributes!(attrs, "zugkontrolle_debug");
    let mut where_clause = generics.where_clause.clone().unwrap_or(WhereClause {
        where_token: Token!(where)(proc_macro2::Span::call_site()),
        predicates: punctuated::Punctuated::new(),
    });
    where_clause.predicates.extend(where_predicates);

    let mut partitionierte_generic_parameter = partitioniere_generics(generics);

    // body of the fmt method
    let fmt = erzeuge_body(ast, ident, data, &mut partitionierte_generic_parameter);

    let PartitionierteGenericParameter {
        lifetimes: generic_lifetimes,
        types: generic_types,
        type_names: generic_type_names,
    } = partitionierte_generic_parameter;

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
        #[allow(single_use_lifetimes)]
        impl<#generic_constraints> std::fmt::Debug for #ident<#generic_names> #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #fmt
            }
        }
    }
}
