//! Implement a method chain variant for a function working on a mutable (self) reference.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{FnArg, ItemFn, Pat, PatType, Receiver, ReturnType, Signature};

/// [`crate::make_chain`]
#[allow(clippy::single_call_fn)]
pub(crate) fn make_chain(args: &TokenStream, ast: &ItemFn) -> TokenStream {
    let mut errors = Vec::new();

    if !args.is_empty() {
        errors.push(format!("no arguments supported, but {args:?} was given."));
    }

    let ItemFn {
        attrs,
        vis,
        sig:
            Signature {
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
    if let ReturnType::Type(_arrow, ty) = output {
        errors.push(format!("only default return type supported, but {ty:?} was given."));
    }
    if let Some(abi) = abi {
        errors.push(format!("no abi supported, but {abi:?} was given."));
    }
    if variadic.is_some() {
        errors.push(String::from("no variadic supported."));
    }
    let mut inputs_iter = inputs.iter();
    if let Some(FnArg::Receiver(Receiver { reference, mutability, .. })) = inputs_iter.next() {
        if reference.is_none() || mutability.is_none() {
            errors.push(String::from("first argument must be &mut self."));
        }
    } else {
        errors.push(String::from("first argument must be &mut self."));
    }
    // collect to vec, to strictly evaluate errors
    let other_input_names: Vec<_> = inputs_iter
        .clone()
        .filter_map(|fn_arg| {
            if let FnArg::Typed(PatType { pat, .. }) = fn_arg {
                if let Pat::Ident(id) = pat.as_ref() {
                    Some(id)
                } else {
                    errors
                        .push(format!("only literal arguments supported, but {pat:?} was given.",));
                    None
                }
            } else {
                errors
                    .push(format!("only literal arguments supported, but {fn_arg:?} was given.",));
                None
            }
        })
        .collect();

    if !errors.is_empty() {
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
