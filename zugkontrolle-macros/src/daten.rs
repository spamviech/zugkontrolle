//! Erstelle Methoden für alle Typen in `GleiseDaten`

use proc_macro2::{Span, TokenStream};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{
    punctuated::Punctuated,
    token::{Gt, Lt},
    AngleBracketedGenericArguments, Attribute, FnArg, GenericArgument, GenericParam, Generics,
    Ident, ImplItemFn, Pat, PatIdent, PatType, Path, PathArguments, PathSegment, ReturnType,
    Signature, Type, TypeParam, TypeParamBound,
};

/// Ersetzte den `generic`-Parameter durch den übergebenen [Pfad].
fn ersetze_generic_path(generic: &Ident, insert: &[PathSegment], mut path: Path) -> Path {
    let num_segments = path.segments.len();
    let (segments, segment) =
        path.segments.into_iter().fold((Punctuated::new(), 0usize), |mut acc, mut path_segment| {
            path_segment.arguments = match path_segment.arguments {
                PathArguments::None => PathArguments::None,
                PathArguments::AngleBracketed(mut angle_bracketed) => {
                    angle_bracketed.args = angle_bracketed
                        .args
                        .into_iter()
                        .map(|generic_arg| {
                            if let GenericArgument::Type(ty) = generic_arg {
                                GenericArgument::Type(ersetze_generic(generic, insert, ty))
                            } else {
                                generic_arg
                            }
                        })
                        .collect();
                    PathArguments::AngleBracketed(angle_bracketed)
                },
                PathArguments::Parenthesized(mut parenthesized) => {
                    parenthesized.inputs = parenthesized
                        .inputs
                        .into_iter()
                        .map(|ty| ersetze_generic(generic, insert, ty))
                        .collect();
                    parenthesized.output = match parenthesized.output {
                        ReturnType::Default => ReturnType::Default,
                        ReturnType::Type(r_arrow, ty) => ReturnType::Type(
                            r_arrow,
                            Box::new(ersetze_generic(generic, insert, *ty)),
                        ),
                    };
                    PathArguments::Parenthesized(parenthesized)
                },
            };
            #[allow(clippy::arithmetic_side_effects)]
            {
                acc.1 += 1;
            }
            if &path_segment.ident == generic {
                acc.0.extend(insert.iter().cloned());
            } else {
                acc.0.push(path_segment);
            }
            acc
        });
    assert!(segment == num_segments, "Sanity check");
    path.segments = segments;
    path
}

/// Ersetzte den `generic`-Parameter durch den übergebenen [Type].
fn ersetze_generic(generic: &Ident, insert: &[PathSegment], ty: Type) -> Type {
    match ty {
        Type::Infer(infer) => Type::Infer(infer),
        Type::Macro(mac) => Type::Macro(mac),
        Type::Never(never) => Type::Never(never),
        Type::TraitObject(trait_object) => Type::TraitObject(trait_object),
        Type::ImplTrait(mut impl_trait) => {
            impl_trait.bounds = impl_trait
                .bounds
                .into_iter()
                .map(|bound| {
                    if let TypeParamBound::Trait(mut trait_bound) = bound {
                        trait_bound.path = ersetze_generic_path(generic, insert, trait_bound.path);
                        TypeParamBound::Trait(trait_bound)
                    } else {
                        bound
                    }
                })
                .collect();
            Type::ImplTrait(impl_trait)
        },
        Type::Verbatim(verb) => Type::Verbatim(verb),
        Type::Array(mut type_array) => {
            type_array.elem = Box::new(ersetze_generic(generic, insert, *type_array.elem));
            Type::Array(type_array)
        },
        Type::Group(mut type_group) => {
            type_group.elem = Box::new(ersetze_generic(generic, insert, *type_group.elem));
            Type::Group(type_group)
        },
        Type::Paren(mut type_paren) => {
            type_paren.elem = Box::new(ersetze_generic(generic, insert, *type_paren.elem));
            Type::Paren(type_paren)
        },
        Type::Ptr(mut type_ptr) => {
            type_ptr.elem = Box::new(ersetze_generic(generic, insert, *type_ptr.elem));
            Type::Ptr(type_ptr)
        },
        Type::Slice(mut type_slice) => {
            type_slice.elem = Box::new(ersetze_generic(generic, insert, *type_slice.elem));
            Type::Slice(type_slice)
        },
        Type::Reference(mut type_reference) => {
            type_reference.elem = Box::new(ersetze_generic(generic, insert, *type_reference.elem));
            Type::Reference(type_reference)
        },
        Type::Tuple(mut type_tuple) => {
            type_tuple.elems = type_tuple
                .elems
                .into_iter()
                .map(|elem| ersetze_generic(generic, insert, elem))
                .collect();
            Type::Tuple(type_tuple)
        },
        Type::BareFn(mut type_bar_fn) => {
            type_bar_fn.inputs = type_bar_fn
                .inputs
                .into_iter()
                .map(|mut bare_fn_arg| {
                    bare_fn_arg.ty = ersetze_generic(generic, insert, bare_fn_arg.ty);
                    bare_fn_arg
                })
                .collect();
            type_bar_fn.output = match type_bar_fn.output {
                ReturnType::Default => ReturnType::Default,
                ReturnType::Type(r_arrow, ret_ty) => {
                    ReturnType::Type(r_arrow, Box::new(ersetze_generic(generic, insert, *ret_ty)))
                },
            };
            Type::BareFn(type_bar_fn)
        },
        Type::Path(mut type_path) => {
            type_path.path = ersetze_generic_path(generic, insert, type_path.path);
            type_path.qself = type_path.qself.map(|mut qself| {
                qself.ty = Box::new(ersetze_generic(generic, insert, *qself.ty));
                qself
            });
            Type::Path(type_path)
        },
        _ => {
            #[allow(clippy::unimplemented)]
            {
                unimplemented!("Unsupported Argument type: {:?}", ty)
            }
        },
    }
}

// ist deprecated und wird demnächst entfernt
#[allow(clippy::too_many_lines)]
/// [`crate::erstelle_daten_methoden`]
pub(crate) fn erstelle_methoden(attr: &TokenStream, item: &ImplItemFn) -> TokenStream {
    let mut errors = Vec::new();

    if !attr.is_empty() {
        errors.push(format!("Keine Argumente unterstützt, bekommen: {attr}"));
    }

    let mut methoden_definitionen: Option<TokenStream> = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };
        let start_segments = vec![
            PathSegment { ident: base_ident, arguments: PathArguments::None },
            PathSegment { ident: format_ident!("gleis"), arguments: PathArguments::None },
        ];
        let erzeuge_typ_segments = |alt_modules: Option<&[&str]>, name: &str| {
            let mut segments = start_segments.clone();
            if let Some(modules) = alt_modules {
                for module in modules {
                    segments.push(PathSegment {
                        ident: format_ident!("{}", module),
                        arguments: PathArguments::None,
                    });
                }
            } else {
                segments.push(PathSegment {
                    ident: format_ident!("{}", name.to_lowercase()),
                    arguments: PathArguments::None,
                });
            }
            segments.push(PathSegment {
                ident: format_ident!("{}", name),
                arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    colon2_token: None,
                    lt_token: Lt { spans: [Span::mixed_site()] },
                    args: Punctuated::new(),
                    gt_token: Gt { spans: [Span::mixed_site()] },
                }),
            });
            segments
        };
        let gerade = erzeuge_typ_segments(None, "Gerade");
        let kurve = erzeuge_typ_segments(None, "Kurve");
        let weiche = erzeuge_typ_segments(Some(&["weiche", "gerade"]), "Weiche");
        let dreiwege_weiche = erzeuge_typ_segments(Some(&["weiche", "dreiwege"]), "DreiwegeWeiche");
        let kurven_weiche = erzeuge_typ_segments(Some(&["weiche", "kurve"]), "KurvenWeiche");
        let s_kurven_weiche = erzeuge_typ_segments(Some(&["weiche", "s_kurve"]), "SKurvenWeiche");
        let kreuzung = erzeuge_typ_segments(None, "Kreuzung");

        let ImplItemFn { sig: Signature { ident, generics, inputs, output, .. }, attrs, .. } =
            &item;
        let doc_attrs: Vec<&Attribute> =
            attrs.iter().filter(|this_attr| this_attr.path().is_ident("doc")).collect();
        let gerade_ident = format_ident!("{}_gerade", ident);
        let kurve_ident = format_ident!("{}_kurve", ident);
        let weiche_ident = format_ident!("{}_weiche", ident);
        let dreiwege_weiche_ident = format_ident!("{}_dreiwege_weiche", ident);
        let kurven_weiche_ident = format_ident!("{}_kurven_weiche", ident);
        let s_kurven_weiche_ident = format_ident!("{}_s_kurven_weiche", ident);
        let kreuzung_ident = format_ident!("{}_kreuzung", ident);

        let mut option_generic_ident = None;
        let mut generic_params = Punctuated::new();
        for generic in &generics.params {
            #[allow(clippy::wildcard_enum_match_arm)]
            match generic {
                GenericParam::Type(TypeParam { ident: ty_ident, bounds, .. })
                    if bounds.iter().any(|bound| {
                        if let TypeParamBound::Trait(trait_bound) = bound {
                            trait_bound
                                .path
                                .segments
                                .iter()
                                .last()
                                .map_or(false, |segment| segment.ident == "DatenAuswahl")
                        } else {
                            false
                        }
                    }) =>
                {
                    option_generic_ident = Some(ty_ident);
                },
                other => generic_params.push(other.clone()),
            }
        }
        let new_generics = Generics {
            lt_token: generics.lt_token,
            params: generic_params,
            gt_token: generics.gt_token,
            where_clause: None,
        };

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
                ReturnType::Type(r_arrow, ty) => {
                    let return_type = |insert_ty: &Vec<PathSegment>| {
                        ReturnType::Type(
                            *r_arrow,
                            Box::new(ersetze_generic(
                                generic_ident,
                                insert_ty,
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
                },
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
            let mut receiver = None;
            for fn_arg in inputs {
                match fn_arg {
                    FnArg::Typed(PatType { pat, ty, .. }) => {
                       if let Pat::Ident(PatIdent { ident: pat_ident, .. }) = pat.as_ref(){
                            input_names.push(pat_ident);
                        } else {
                            errors.push(format!(
                                "Only pure name-patterns supported, but {pat:?} was used!",
                            ));
                        }
                        gerade_types.push(ersetze_generic(
                            generic_ident,
                            &gerade,
                            ty.as_ref().clone(),
                        ));
                        kurve_types.push(ersetze_generic(
                            generic_ident,
                            &kurve,
                            ty.as_ref().clone(),
                        ));
                        weiche_types.push(ersetze_generic(
                            generic_ident,
                            &weiche,
                            ty.as_ref().clone(),
                        ));
                        dreiwege_weiche_types.push(ersetze_generic(
                            generic_ident,
                            &dreiwege_weiche,
                            ty.as_ref().clone(),
                        ));
                        kurven_weiche_types.push(ersetze_generic(
                            generic_ident,
                            &kurven_weiche,
                            ty.as_ref().clone(),
                        ));
                        s_kurven_weiche_types.push(ersetze_generic(
                            generic_ident,
                            &s_kurven_weiche,
                            ty.as_ref().clone(),
                        ));
                        kreuzung_types.push(ersetze_generic(
                            generic_ident,
                            &kreuzung,
                            ty.as_ref().clone(),
                        ));
                    },
                    FnArg::Receiver(self_or_mut_self) if self_or_mut_self.attrs.is_empty() => {
                        receiver = Some(self_or_mut_self);
                    },
                    FnArg::Receiver(fn_receiver) => errors.push(format!(
                        "Only Receiver ([&mut] self) without attributes supported, got `{fn_receiver:?}` instead.",
                    )),
                }
            }
            if receiver.is_none() {
                errors.push("Only function with Receiver ([&mut] self) supported!".to_owned());
            }

            let erzeuge_methode = |new_ident: Ident, types: Vec<Type>, output: ReturnType| {
                quote! {
                    #[inline(always)]
                    #(#doc_attrs)*
                    pub fn #new_ident #new_generics (#receiver, #(#input_names: #types),*) #output {
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
        } else {
            errors.push("Kein Parameter mit DatenAuswahl-Constraint.".to_owned());
        }
    } else {
        errors.push("`zugkontrolle` missing in `Cargo.toml`".to_string());
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
