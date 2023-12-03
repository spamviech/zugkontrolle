//! Erzeuge Type-Alias für das letzte Generic.

use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};

pub(crate) fn alias_serialisiert_unit(arg: TokenStream, item: syn::ItemStruct) -> TokenStream {
    let mut errors = Vec::new();

    let syn::ItemStruct { vis, ident, fields, generics, .. } = &item;
    let mut type_definitionen = None;

    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident: syn::Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };
        if let Some((
            syn::GenericParam::Type(syn::TypeParam {
                ident: g, default: Some(default_type), ..
            }),
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
                let serialisiert_ident = format_ident!("{}Serialisiert", ident);
                let unit_ident = format_ident!("{}Unit", ident);
                let params_start = if params.is_empty() { quote!() } else { quote!(#(#params),*,) };
                type_definitionen = Some(quote! {
                    /// Eine serialisierbare Repräsentation.
                    #vis type #serialisiert_ident<#(#params),*> = #ident<#params_start Option<#arg>>;
                    /// Eine Variante ohne Anschlüsse.
                    #vis type #unit_ident<#(#params),*> = #ident<#params_start ()>;
                    impl<#(#params),*> #base_ident::anschluss::de_serialisieren::Serialisiere<
                        #serialisiert_ident<#(#params),*>
                    > for #ident<#(#params),*>
                    {
                        fn serialisiere(&self) -> #serialisiert_ident<#(#params),*> {
                            let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                            #serialisiert_ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(
                                    #param_fields: #param_fields.as_ref().map(
                                        |steuerung| steuerung.serialisiere()
                                    )
                                ),*
                            }
                        }
                        fn anschlüsse(self) -> #base_ident::anschluss::de_serialisieren::Anschlüsse {
                            let mut anschlüsse = #base_ident::anschluss::de_serialisieren::Anschlüsse::default();
                            #(
                                if let Some(steuerung) = self.#param_fields {
                                    anschlüsse.anhängen(steuerung.anschlüsse());
                                }
                            )*
                            anschlüsse
                        }
                    }
                    impl<#(#params),*> #base_ident::anschluss::de_serialisieren::Reserviere<#ident<#(#params),*>> for #serialisiert_ident<#(#params),*> {
                        #[allow(unused_qualifications)]
                        type Arg = <Option<#arg> as #base_ident::anschluss::de_serialisieren::Reserviere<#default_type>>::Arg;

                        fn reserviere(
                            self,
                            lager: &mut #base_ident::anschluss::Lager,
                            anschlüsse: #base_ident::anschluss::de_serialisieren::Anschlüsse,
                            arg: Self::Arg,
                        ) -> #base_ident::anschluss::de_serialisieren::Ergebnis<#ident<#(#params),*>> {
                            let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                            (#(#param_fields),*)
                                .reserviere(lager, anschlüsse, arg)
                                .konvertiere(|(#(#param_fields),*)| {
                                    #ident { #(#other_fields),*, #(#param_fields),* }
                                })
                        }
                    }
                    impl<#(#params),*> #ident<#(#params),*> {
                        /// Clone in eine äquivalente Darstellung ohne Anschlüsse.
                        pub fn mit_unit(&self) -> #unit_ident<#(#params),*> {
                            let #ident { #(#other_fields),*, .. } = self;
                            #unit_ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(#param_fields: ()),*
                            }
                        }
                    }
                    impl<#(#params),*> #unit_ident<#(#params),*> {
                        /// Clone in eine äquivalente Darstellung mit [None] als Anschlüsse.
                        pub fn mit_none<T>(&self) -> #ident<#params_start Option<T>> {
                            let #ident { #(#other_fields),*, .. } = self;
                            #ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(#param_fields: None),*
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
        };
    }

    quote! {
        #item
        #type_definitionen
    }
}
