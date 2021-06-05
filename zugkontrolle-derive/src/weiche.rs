//! Erzeuge Richtung enum und RichtungAnschlüsse(Save) Strukturen mit Lookup-Implementierung.

use inflector::cases::snakecase::to_snake_case;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};

pub fn alias_save_unit(args: Vec<syn::NestedMeta>, item: syn::ItemStruct) -> TokenStream {
    let mut errors = Vec::new();

    if !args.is_empty() {
        errors.push(format!("No arguments supported, but {:?} was given!", args))
    }

    let syn::ItemStruct { vis, ident, fields, generics, .. } = &item;
    let mut type_definitionen = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident: syn::Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };

        if let Some((
            syn::GenericParam::Type(syn::TypeParam { ident: g, default: Some(_type), .. }),
            params,
        )) = generics.params.iter().collect::<Vec<_>>().split_last()
        {
            if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = fields {
                let (param_fields, other_fields): (Vec<_>, Vec<_>) =
                    named.iter().partition(|syn::Field { ty, .. }| {
                        if let syn::Type::Path(syn::TypePath {
                            path: syn::Path { segments, .. },
                            ..
                        }) = ty
                        {
                            if let Some(syn::PathSegment { ident, .. }) = segments.first() {
                                ident == g
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    });
                let param_fields: Vec<_> =
                    param_fields.into_iter().map(|field| &field.ident).collect();
                let other_fields: Vec<_> =
                    other_fields.into_iter().map(|field| &field.ident).collect();
                let save_ident = format_ident!("{}Save", ident);
                let unit_ident = format_ident!("{}Unit", ident);
                type_definitionen = Some(quote! {
                    #vis type #save_ident<#(#params),*> = #ident<#(#params),*, Option<#base_ident::steuerung::Weiche<RichtungAnschlüsseSave>>>;
                    #vis type #unit_ident<#(#params),*> = #ident<#(#params),*, ()>;
                    impl<#(#params),*> #ident<#(#params),*> {
                        pub fn to_save(&self) -> #save_ident<Z> {
                            let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                            #save_ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(
                                    #param_fields: #param_fields.as_ref().map(
                                        |steuerung| steuerung.to_save()
                                    )
                                ),*
                            }
                        }

                        pub fn to_unit(&self) -> #unit_ident<Z> {
                            let #ident { #(#other_fields),*, .. } = self;
                            #unit_ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(#param_fields: ()),*
                            }
                        }
                    }
                })
            } else {
                errors.push("Only named field supported!".to_string())
            }
        } else {
            errors.push("Missing generics!".to_string())
        }
    } else {
        errors.push("`zugkontrolle` missing in `Cargo.toml`".to_string())
    }

    if !errors.is_empty() {
        let error_message = errors.join("\n");
        return quote! {
            compile_error!(#error_message);
            #item
        }
    }

    quote! {
        #item
        #type_definitionen
    }
}

pub fn create_richtung(args: Vec<syn::NestedMeta>, item: syn::ItemEnum) -> TokenStream {
    let mut errors = Vec::new();

    let syn::ItemEnum { vis, variants, .. } = &item;
    if !args.is_empty() {
        errors.push(format!("No args supported, but {:?} was given!", args));
    }

    let mut enum_definition = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident: syn::Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };

        let enum_variants: Vec<_> = variants
            .iter()
            .filter_map(
                |syn::Variant { ident, .. }| {
                    if ident.to_string() == "Anfang" {
                        None
                    } else {
                        Some(ident)
                    }
                },
            )
            .collect();
        let struct_fields: Vec<syn::Ident> = enum_variants
                    .iter()
                    // TODO fix upstream?
                    // to_snakecase wrongly adds a '_' before 'ß', even though it it a small letter
                    // possibly because there is no real uppercase character of it
                    .map(|variant| format_ident!("{}", to_snake_case(&variant.to_string()).replace("_ß", "ß")))
                    .collect();

        enum_definition = Some(quote! {
            #[zugkontrolle_derive::impl_lookup(#base_ident::anschluss::OutputAnschluss, Anschlüsse, Debug)]
            #[zugkontrolle_derive::impl_lookup(#base_ident::anschluss::OutputSave, AnschlüsseSave, Debug, Clone, Serialize, Deserialize)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
            #vis enum Richtung {
                #(#enum_variants),*
            }
            impl RichtungAnschlüsse {
                pub fn to_save(&self) -> RichtungAnschlüsseSave {
                    let RichtungAnschlüsse { #(#struct_fields),* } = self;
                    RichtungAnschlüsseSave { #(#struct_fields: #struct_fields.to_save()),* }
                }
            }
            impl RichtungAnschlüsseSave {
                pub fn reserviere(
                    self,
                    anschlüsse: &mut #base_ident::anschluss::Anschlüsse,
                ) -> Result<RichtungAnschlüsse, #base_ident::anschluss::Error> {
                    let RichtungAnschlüsseSave {  #(#struct_fields),* } = self;
                    Ok(RichtungAnschlüsse {
                        #(#struct_fields: #struct_fields.reserviere(anschlüsse)?),*
                    })
                }
            }
            impl #base_ident::steuerung::Weiche<RichtungAnschlüsse> {
                pub fn to_save(&self) -> #base_ident::steuerung::Weiche<RichtungAnschlüsseSave> {
                    #base_ident::steuerung::Weiche {anschlüsse: self.anschlüsse.to_save()}
                }
            }
            impl #base_ident::steuerung::Weiche<RichtungAnschlüsseSave> {
                pub fn reserviere(
                    self,
                    anschlüsse: &mut #base_ident::anschluss::Anschlüsse,
                ) -> Result<#base_ident::steuerung::Weiche<RichtungAnschlüsse>, #base_ident::anschluss::Error> {
                    Ok(#base_ident::steuerung::Weiche {anschlüsse: self.anschlüsse.reserviere(anschlüsse)?})
                }
            }
        })
    } else {
        errors.push("`zugkontrolle` missing in `Cargo.toml`".to_string())
    }

    if !errors.is_empty() {
        let error_message = errors.join("\n");
        return quote! {
            compile_error!(#error_message);
            #item
        }
    }

    quote! {
        #item
        #enum_definition
    }
}
