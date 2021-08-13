//! Erstelle Methoden für alle Typen in GleiseMaps

use std::iter;

use proc_macro2::{Span, TokenStream};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{
    punctuated::Punctuated,
    token::{And, Comma, Gt, Lt, Mut, RArrow, SelfValue},
    AngleBracketedGenericArguments, FnArg, GenericArgument, GenericParam, Ident, ImplItemMethod,
    Pat, PatIdent, PatType, Path, PathArguments, PathSegment, Receiver, ReturnType, Signature,
    Type, TypeParam, TypePath,
};

fn ersetze_generic(generic: &Ident, insert: Type, ty: Type) -> Type {
    todo!()
}

pub fn erstelle_methoden(item: ImplItemMethod) -> TokenStream {
    let mut errors = Vec::new();

    let mut methoden_definitionen: Option<TokenStream> = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };
        let mut start_segments = Punctuated::new();
        start_segments.push(PathSegment { ident: base_ident, arguments: PathArguments::None });
        start_segments.push(PathSegment {
            ident: format_ident!("application"),
            arguments: PathArguments::None,
        });
        start_segments
            .push(PathSegment { ident: format_ident!("gleis"), arguments: PathArguments::None });
        let ty_args: Punctuated<GenericArgument, Comma> =
            iter::once(GenericArgument::Type(Type::Path(TypePath {
                qself: None,
                path: Path {
                    leading_colon: None,
                    segments: iter::once(PathSegment {
                        ident: format_ident!("Z"),
                        arguments: PathArguments::None,
                    })
                    .collect(),
                },
            })))
            .collect();
        let erzeuge_typ = move |name: &str| {
            let mut segments = start_segments.clone();
            segments.push(PathSegment {
                ident: format_ident!("{}", name),
                arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    colon2_token: None,
                    lt_token: Lt { spans: [Span::mixed_site()] },
                    args: ty_args.clone(),
                    gt_token: Gt { spans: [Span::mixed_site()] },
                }),
            });
            Type::Path(TypePath { qself: None, path: Path { leading_colon: None, segments } })
        };
        let gerade = erzeuge_typ("Gerade");
        let kurve = erzeuge_typ("Kurve");
        let weiche = erzeuge_typ("Weiche");
        let dreiwege_weiche = erzeuge_typ("DreiwegeWeiche");
        let kurven_weiche = erzeuge_typ("KurvenWeiche");
        let s_kurven_weiche = erzeuge_typ("SKurvenWeiche");
        let kreuzung = erzeuge_typ("Kreuzung");

        let ImplItemMethod { sig: Signature { ident, generics, inputs, output, .. }, .. } = &item;
        let gerade_ident = format_ident!("{}_gerade", ident);
        let kurve_ident = format_ident!("{}_kurve", ident);
        let weiche_ident = format_ident!("{}_weiche", ident);
        let dreiwege_weiche_ident = format_ident!("{}_dreiwege_weiche", ident);
        let kurven_weiche_ident = format_ident!("{}_kurven_weiche", ident);
        let s_kurven_weiche_ident = format_ident!("{}_s_kurven_weiche", ident);
        let kreuzung_ident = format_ident!("{}_kreuzung", ident);

        let mut option_generic_ident = None;
        for generic in generics.params.iter() {
            if let GenericParam::Type(TypeParam { ident, .. }) = generic {
                option_generic_ident = Some(ident);
                break;
            }
        }

        if let Some(generic_ident) = option_generic_ident {
            let (
                gerade_output,
                kurve_output,
                weiche_output,
                dreiwege_weiche_output,
                kurven_weiche_output,
                s_kurven_weiche_output,
                kreuzung_output,
            ) = match output {
                ReturnType::Type(RArrow { spans: _ }, ty) => {
                    let return_type = |insert_ty: &Type| {
                        ReturnType::Type(
                            RArrow { spans: [Span::mixed_site(), Span::mixed_site()] },
                            Box::new(ersetze_generic(
                                generic_ident,
                                insert_ty.clone(),
                                ty.as_ref().clone(),
                            )),
                        )
                    };
                    (
                        return_type(&gerade),
                        return_type(&kurve),
                        return_type(&weiche),
                        return_type(&dreiwege_weiche),
                        return_type(&kurven_weiche),
                        return_type(&s_kurven_weiche),
                        return_type(&kreuzung),
                    )
                }
                ReturnType::Default => (
                    ReturnType::Default,
                    ReturnType::Default,
                    ReturnType::Default,
                    ReturnType::Default,
                    ReturnType::Default,
                    ReturnType::Default,
                    ReturnType::Default,
                ),
            };
            let mut input_names = Vec::new();
            let mut gerade_types = Vec::new();
            let mut kurve_types = Vec::new();
            let mut weiche_types = Vec::new();
            let mut dreiwege_weiche_types = Vec::new();
            let mut kurven_weiche_types = Vec::new();
            let mut s_kurven_weiche_types = Vec::new();
            let mut kreuzung_types = Vec::new();
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
                        gerade_types.push(ersetze_generic(
                            generic_ident,
                            gerade.clone(),
                            ty.as_ref().clone(),
                        ));
                        kurve_types.push(ersetze_generic(
                            generic_ident,
                            kurve.clone(),
                            ty.as_ref().clone(),
                        ));
                        weiche_types.push(ersetze_generic(
                            generic_ident,
                            weiche.clone(),
                            ty.as_ref().clone(),
                        ));
                        dreiwege_weiche_types.push(ersetze_generic(
                            generic_ident,
                            dreiwege_weiche.clone(),
                            ty.as_ref().clone(),
                        ));
                        kurven_weiche_types.push(ersetze_generic(
                            generic_ident,
                            kurven_weiche.clone(),
                            ty.as_ref().clone(),
                        ));
                        s_kurven_weiche_types.push(ersetze_generic(
                            generic_ident,
                            s_kurven_weiche.clone(),
                            ty.as_ref().clone(),
                        ));
                        kreuzung_types.push(ersetze_generic(
                            generic_ident,
                            kreuzung.clone(),
                            ty.as_ref().clone(),
                        ));
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

            let erzeuge_methode = |new_ident: Ident, types: Vec<Type>, output: ReturnType| {
                quote! {
                    pub fn #new_ident(&mut self, #(#input_names, #types),*) -> #output {
                        self.#ident(#(#input_names),*)
                    }
                }
            };
            let gerade_methode = erzeuge_methode(gerade_ident, gerade_types, gerade_output);
            let kurve_methode = erzeuge_methode(kurve_ident, kurve_types, kurve_output);
            let weiche_methode = erzeuge_methode(weiche_ident, weiche_types, weiche_output);
            let dreiwege_weiche_methode = erzeuge_methode(
                dreiwege_weiche_ident,
                dreiwege_weiche_types,
                dreiwege_weiche_output,
            );
            let kurven_weiche_methode =
                erzeuge_methode(kurven_weiche_ident, kurven_weiche_types, kurven_weiche_output);
            let s_kurven_weiche_methode = erzeuge_methode(
                s_kurven_weiche_ident,
                s_kurven_weiche_types,
                s_kurven_weiche_output,
            );
            let kreuzung_methode = erzeuge_methode(kreuzung_ident, kreuzung_types, kreuzung_output);

            methoden_definitionen = Some(quote! {
                #gerade_methode
                #kurve_methode
                #weiche_methode
                #dreiwege_weiche_methode
                #kurven_weiche_methode
                #s_kurven_weiche_methode
                #kreuzung_methode
            });
        }
    } else {
        errors.push("`zugkontrolle` missing in `Cargo.toml`".to_string())
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
