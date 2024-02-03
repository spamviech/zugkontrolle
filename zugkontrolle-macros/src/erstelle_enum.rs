//! Take a sum-type enum and produce an associated enum without any data

use std::iter::once;

use proc_macro2::{TokenStream, TokenTree};
use quote::{format_ident, quote};
use syn::{parse2, Ident, ItemEnum, Variant, Visibility};

/// Parse die Macro-Argumente.
fn parse_args(args: TokenStream) -> Result<(Option<Visibility>, Option<Ident>), Vec<String>> {
    let mut acc = TokenStream::new();
    let mut arg_vis: Option<Visibility> = None;
    let mut arg_ident: Option<Ident> = None;
    let mut errors = Vec::new();
    let mut parse_acc = |current_acc: &mut TokenStream| {
        // Unterschiedliche Funktion wegen unterschiedlichem Rückgabetyp.
        #[allow(clippy::same_functions_in_if_condition)]
        if let Ok(vis) = parse2(current_acc.clone()) {
            if arg_vis.is_none() {
                arg_vis = Some(vis);
            } else {
                errors.push(format!("duplicate visibility argument {current_acc}"));
            }
        } else if let Ok(ident) = parse2(current_acc.clone()) {
            if arg_ident.is_none() {
                arg_ident = Some(ident);
            } else {
                errors.push(format!("duplicate identifier {current_acc}"));
            }
        } else {
            errors.push(format!("Only identifiers supported, but {current_acc} was given"));
        }
        *current_acc = TokenStream::new();
    };
    for tt in args {
        // if-let-chain ist noch nicht auf stable
        #[allow(clippy::wildcard_enum_match_arm)]
        match tt {
            TokenTree::Punct(punct) if punct.as_char() == ',' => {
                parse_acc(&mut acc);
            },
            _ => acc.extend(once(tt)),
        }
    }
    // berücksichtige fehlendes terminierendes Komma
    if !acc.is_empty() {
        parse_acc(&mut acc);
    }
    if errors.is_empty() {
        Ok((arg_vis, arg_ident))
    } else {
        Err(errors)
    }
}

/// [`crate::erstelle_enum`]
pub(crate) fn erstelle_enum(args: TokenStream, ast: &ItemEnum) -> TokenStream {
    let (arg_vis, arg_ident) = match parse_args(args) {
        Ok(vid_ident) => vid_ident,
        Err(errors) => {
            let error_message = errors.join("\n");
            return quote! {
                compile_error!(#error_message);
                #ast
            };
        },
    };
    let derives = quote!(
        #[derive(Debug, Clone, Copy, PartialEq, Eq, ::kommandozeilen_argumente::EnumArgument)]
        #[kommandozeilen_argumente(case: insensitive)]
    );

    let ItemEnum { vis, ident, variants, attrs, .. } = &ast;
    let enum_vis = arg_vis.unwrap_or(vis.clone());
    let enum_ident = arg_ident.unwrap_or(format_ident!("{}Enum", ident));
    let (enum_variants, enum_variants_docstrings): (Vec<Ident>, Vec<TokenStream>) = variants
        .iter()
        .map(|Variant { ident: var_ident, attrs: var_attrs, .. }| {
            let docstrings = var_attrs.iter().filter(|attr| attr.path().is_ident("doc")).cloned();
            (var_ident.clone(), quote!(#(#docstrings)*))
        })
        .unzip();
    let enum_variants_str: Vec<String> = enum_variants.iter().map(Ident::to_string).collect();
    let docstrings = attrs.iter().filter(|attr| attr.path().is_ident("doc"));
    quote!(
        #ast

        #(#docstrings)*
        #derives
        #enum_vis enum #enum_ident {
            #(
                #enum_variants_docstrings
                #enum_variants
            ),*
        }

        impl std::fmt::Display for #enum_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", self)
            }
        }

        impl From<#enum_ident> for String {
            fn from(modus: #enum_ident) -> Self {
                format!("{}", modus)
            }
        }

        impl std::str::FromStr for #enum_ident {
            type Err = String;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    #(#enum_variants_str => Ok(#enum_ident::#enum_variants)),*,
                    _ => Err(String::from(s)),
                }
            }
        }
    )
}
