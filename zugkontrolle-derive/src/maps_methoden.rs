//! Erstelle Methoden für alle Typen in GleiseMaps

use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{
    token::{And, Mut, SelfValue},
    FnArg, GenericParam, Ident, ImplItemMethod, Pat, PatIdent, PatType, Receiver, Signature, Type,
    TypeParam,
};

fn ersetze_generic(generic: Option<&Ident>, ty: Type) -> Box<dyn Fn(Type) -> Type> {
    Box::new(|ty| todo!())
}

pub fn erstelle_methoden(item: ImplItemMethod) -> TokenStream {
    let mut errors = Vec::new();

    let ImplItemMethod { sig: Signature { ident, generics, inputs, output, .. }, .. } = &item;

    let mut generic_ident = None;
    for generic in generics.params.iter() {
        if let GenericParam::Type(TypeParam { ident, .. }) = generic {
            generic_ident = Some(ident)
        }
    }

    let mut methoden_definitionen: Option<TokenStream> = None;
    let mut input_names = Vec::new();
    let mut input_types: Vec<()> = Vec::new();
    let mut no_receiver = true;
    for fn_arg in inputs {
        match fn_arg {
            FnArg::Typed(PatType { pat, ty, .. }) => {
                match pat.as_ref() {
                    Pat::Ident(PatIdent { ident, .. }) => input_names.push(ident),
                    _ => errors.push(format!(
                        "Only pure name-patterns supported, but {:?} was used!",
                        pat
                    )),
                }
                // TODO
                errors.push(format!("{:?}", ty))
            }
            FnArg::Receiver(Receiver {
                attrs,
                reference: Some((And { spans: _ }, None)),
                mutability: Some(Mut { span: _ }),
                self_token: SelfValue { span: _ },
            }) if attrs.is_empty() => no_receiver = false,
            FnArg::Receiver(receiver) => errors.push(format!(
                "Only '&mut self'-Receiver supported, got '{:?}' instead!",
                receiver
            )),
        }
    }
    if no_receiver {
        errors.push("'&mut self'-Argument required!".to_string())
    }

    if !errors.is_empty() {
        let error_message = errors.join("\n");
        return quote! {
            compile_error!(#error_message);
            #item
        };
    }

    quote! {
        #item
        #methoden_definitionen
    }
}
