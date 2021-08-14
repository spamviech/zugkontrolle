//! Erstelle Methoden für alle Typen in GleiseMaps

use std::iter;

use proc_macro2::{Span, TokenStream};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{
    punctuated::Punctuated,
    token::{And, As, Comma, Gt, Lt, Mut, SelfValue},
    Type, *,
};

fn ersetze_generic(
    generic: &Ident,
    insert: Vec<PathSegment>,
    trait_segments: Vec<PathSegment>,
    ty: Type,
) -> Type {
    match ty {
        Type::Infer(infer) => Type::Infer(infer),
        Type::Macro(mac) => Type::Macro(mac),
        Type::Never(never) => Type::Never(never),
        Type::TraitObject(trait_object) => Type::TraitObject(trait_object),
        Type::ImplTrait(impl_trait) => Type::ImplTrait(impl_trait),
        Type::Verbatim(verb) => Type::Verbatim(verb),
        Type::Array(mut type_array) => {
            type_array.elem =
                Box::new(ersetze_generic(generic, insert, trait_segments, *type_array.elem));
            Type::Array(type_array)
        }
        Type::Group(mut type_group) => {
            type_group.elem =
                Box::new(ersetze_generic(generic, insert, trait_segments, *type_group.elem));
            Type::Group(type_group)
        }
        Type::Paren(mut type_paren) => {
            type_paren.elem =
                Box::new(ersetze_generic(generic, insert, trait_segments, *type_paren.elem));
            Type::Paren(type_paren)
        }
        Type::Ptr(mut type_ptr) => {
            type_ptr.elem =
                Box::new(ersetze_generic(generic, insert, trait_segments, *type_ptr.elem));
            Type::Ptr(type_ptr)
        }
        Type::Slice(mut type_slice) => {
            type_slice.elem =
                Box::new(ersetze_generic(generic, insert, trait_segments, *type_slice.elem));
            Type::Slice(type_slice)
        }
        Type::Reference(mut type_reference) => {
            type_reference.elem =
                Box::new(ersetze_generic(generic, insert, trait_segments, *type_reference.elem));
            Type::Reference(type_reference)
        }
        Type::Tuple(mut type_tuple) => {
            type_tuple.elems = type_tuple
                .elems
                .into_iter()
                .map(|elem| ersetze_generic(generic, insert.clone(), trait_segments.clone(), elem))
                .collect();
            Type::Tuple(type_tuple)
        }
        Type::BareFn(mut type_bar_fn) => {
            type_bar_fn.inputs = type_bar_fn
                .inputs
                .into_iter()
                .map(|mut bare_fn_arg| {
                    bare_fn_arg.ty = ersetze_generic(
                        generic,
                        insert.clone(),
                        trait_segments.clone(),
                        bare_fn_arg.ty,
                    );
                    bare_fn_arg
                })
                .collect();
            type_bar_fn.output = match type_bar_fn.output {
                ReturnType::Default => ReturnType::Default,
                ReturnType::Type(r_arrow, ty) => ReturnType::Type(
                    r_arrow,
                    Box::new(ersetze_generic(generic, insert.clone(), trait_segments.clone(), *ty)),
                ),
            };
            Type::BareFn(type_bar_fn)
        }
        Type::Path(mut type_path) => {
            let num_segments = type_path.path.segments.len();
            let (segments, segment, qself) = type_path.path.segments.into_iter().fold(
                (Punctuated::new(), 0usize, None),
                |mut acc, mut path_segment| {
                    path_segment.arguments = match path_segment.arguments {
                        PathArguments::None => PathArguments::None,
                        PathArguments::AngleBracketed(mut angle_bracketed) => {
                            angle_bracketed.args = angle_bracketed
                                .args
                                .into_iter()
                                .map(|generic_arg| {
                                    if let GenericArgument::Type(ty) = generic_arg {
                                        GenericArgument::Type(ersetze_generic(
                                            generic,
                                            insert.clone(),
                                            trait_segments.clone(),
                                            ty,
                                        ))
                                    } else {
                                        generic_arg
                                    }
                                })
                                .collect();
                            PathArguments::AngleBracketed(angle_bracketed)
                        }
                        PathArguments::Parenthesized(mut parenthesized) => {
                            parenthesized.inputs = parenthesized
                                .inputs
                                .into_iter()
                                .map(|ty| {
                                    ersetze_generic(
                                        generic,
                                        insert.clone(),
                                        trait_segments.clone(),
                                        ty,
                                    )
                                })
                                .collect();
                            parenthesized.output = match parenthesized.output {
                                ReturnType::Default => ReturnType::Default,
                                ReturnType::Type(r_arrow, ty) => ReturnType::Type(
                                    r_arrow,
                                    Box::new(ersetze_generic(
                                        generic,
                                        insert.clone(),
                                        trait_segments.clone(),
                                        *ty,
                                    )),
                                ),
                            };
                            PathArguments::Parenthesized(parenthesized)
                        }
                    };
                    acc.1 += 1;
                    if &path_segment.ident == generic {
                        if acc.1 < num_segments {
                            acc.0.extend(trait_segments.iter().cloned());
                            // use type associated with trait Zeichnen
                            acc.2 = Some(QSelf {
                                lt_token: Lt { spans: [Span::mixed_site()] },
                                ty: Box::new(Type::Path(TypePath {
                                    qself: None,
                                    path: Path {
                                        leading_colon: None,
                                        segments: insert.iter().cloned().collect(),
                                    },
                                })),
                                position: acc.0.len(),
                                as_token: Some(As { span: Span::mixed_site() }),
                                gt_token: Gt { spans: [Span::mixed_site()] },
                            });
                        } else {
                            // generic is the last (and probably only) path segment
                            acc.0.extend(insert.iter().cloned())
                        }
                    } else {
                        acc.0.push(path_segment)
                    }
                    acc
                },
            );
            assert!(segment == num_segments);
            type_path.qself = qself;
            type_path.path.segments = segments;
            Type::Path(type_path)
        }
        _ => unimplemented!("Unsupported Argument type: {:?}", ty),
    }
}

pub fn erstelle_methoden(item: ImplItemMethod) -> TokenStream {
    let mut errors = Vec::new();

    let mut methoden_definitionen: Option<TokenStream> = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };
        let zeichen_segments = vec![
            PathSegment { ident: base_ident.clone(), arguments: PathArguments::None },
            PathSegment { ident: format_ident!("application"), arguments: PathArguments::None },
            PathSegment { ident: format_ident!("typen"), arguments: PathArguments::None },
            PathSegment { ident: format_ident!("Zeichnen"), arguments: PathArguments::None },
        ];
        let start_segments = vec![
            PathSegment { ident: base_ident, arguments: PathArguments::None },
            PathSegment { ident: format_ident!("application"), arguments: PathArguments::None },
            PathSegment { ident: format_ident!("gleis"), arguments: PathArguments::None },
        ];
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
        let erzeuge_typ_segments = |name: &str| {
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
            segments
        };
        let gerade = erzeuge_typ_segments("Gerade");
        let kurve = erzeuge_typ_segments("Kurve");
        let weiche = erzeuge_typ_segments("Weiche");
        let dreiwege_weiche = erzeuge_typ_segments("DreiwegeWeiche");
        let kurven_weiche = erzeuge_typ_segments("KurvenWeiche");
        let s_kurven_weiche = erzeuge_typ_segments("SKurvenWeiche");
        let kreuzung = erzeuge_typ_segments("Kreuzung");

        let ImplItemMethod {
            sig: Signature { ident, generics, inputs, output, .. }, attrs, ..
        } = &item;
        let doc_attrs: Vec<&Attribute> = attrs
            .iter()
            .filter(|attr| {
                attr.path.segments.len() == 1
                    && attr.path.segments.first().map(|path_segment| path_segment.ident.to_string())
                        == Some("doc".to_string())
            })
            .collect();
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
                ReturnType::Type(r_arrow, ty) => {
                    let return_type = |insert_ty: &Vec<PathSegment>| {
                        ReturnType::Type(
                            r_arrow.clone(),
                            Box::new(ersetze_generic(
                                generic_ident,
                                insert_ty.clone(),
                                zeichen_segments.clone(),
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
                            zeichen_segments.clone(),
                            ty.as_ref().clone(),
                        ));
                        kurve_types.push(ersetze_generic(
                            generic_ident,
                            kurve.clone(),
                            zeichen_segments.clone(),
                            ty.as_ref().clone(),
                        ));
                        weiche_types.push(ersetze_generic(
                            generic_ident,
                            weiche.clone(),
                            zeichen_segments.clone(),
                            ty.as_ref().clone(),
                        ));
                        dreiwege_weiche_types.push(ersetze_generic(
                            generic_ident,
                            dreiwege_weiche.clone(),
                            zeichen_segments.clone(),
                            ty.as_ref().clone(),
                        ));
                        kurven_weiche_types.push(ersetze_generic(
                            generic_ident,
                            kurven_weiche.clone(),
                            zeichen_segments.clone(),
                            ty.as_ref().clone(),
                        ));
                        s_kurven_weiche_types.push(ersetze_generic(
                            generic_ident,
                            s_kurven_weiche.clone(),
                            zeichen_segments.clone(),
                            ty.as_ref().clone(),
                        ));
                        kreuzung_types.push(ersetze_generic(
                            generic_ident,
                            kreuzung.clone(),
                            zeichen_segments.clone(),
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
                    #[inline(always)]
                    #(#doc_attrs)*
                    pub fn #new_ident(&mut self, #(#input_names: #types),*) #output {
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
        quote! {
            compile_error!(#error_message);
            #item
        }
    } else {
        quote! {
            #item
            #methoden_definitionen
        }
    }
}
