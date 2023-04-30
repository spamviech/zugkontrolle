//! Implement a method chain variant for a function working on a mutable (self) reference.

use std::convert::identity;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub(crate) fn make_chain(args: TokenStream, ast: syn::ItemFn) -> TokenStream {
    let mut errors = Vec::new();

    if !args.is_empty() {
        errors.push(format!("no arguments supported, but {:?} was given.", args));
    }

    let syn::ItemFn {
        attrs,
        vis,
        sig:
            syn::Signature {
                constness,
                asyncness,
                unsafety,
                ident,
                generics,
                inputs,
                output,
                abi,
                variadic,
                ..
            },
        ..
    } = &ast;
    let docstrings: Vec<_> = attrs.iter().filter(|attr| attr.path().is_ident("doc")).collect();
    if let syn::ReturnType::Type(_arrow, ty) = output {
        errors.push(format!("only default return type supported, but {:?} was given.", ty));
    }
    if let Some(abi) = abi {
        errors.push(format!("no abi supported, but {:?} was given.", abi));
    }
    if variadic.is_some() {
        errors.push("no variadic supported.".to_string());
    }
    let mut inputs_iter = inputs.iter();
    if let Some(syn::FnArg::Receiver(syn::Receiver { reference, mutability, .. })) =
        inputs_iter.next()
    {
        if reference.is_none() || mutability.is_none() {
            errors.push("first argument must be &mut self.".to_string());
        }
    } else {
        errors.push("first argument must be &mut self.".to_string());
    }
    // collect to vec, to strictly evaluate errors
    let other_input_names: Vec<_> = inputs_iter
        .clone()
        .map(|fn_arg| {
            if let syn::FnArg::Typed(syn::PatType { pat, .. }) = fn_arg {
                if let syn::Pat::Ident(id) = pat.as_ref() {
                    Some(id)
                } else {
                    errors.push(format!(
                        "only literal arguments supported, but {:?} was given.",
                        pat
                    ));
                    None
                }
            } else {
                errors
                    .push(format!("only literal arguments supported, but {:?} was given.", fn_arg));
                None
            }
        })
        .filter_map(identity)
        .collect();

    if errors.len() > 0 {
        let error_message = errors.join("\n");
        return quote! {
            compile_error!(#error_message);
            #ast
        };
    }
    let chain_ident = format_ident!("{}_chain", ident);
    quote! {
        #ast

        #(#docstrings)*
        #vis #constness #asyncness #unsafety fn #chain_ident #generics(mut self, #(#inputs_iter),*) -> Self {
            self.#ident(#(#other_input_names),*);
            self
        }
    }
}
