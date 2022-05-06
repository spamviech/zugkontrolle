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
                let save_ident = format_ident!("{}Serialisiert", ident);
                let unit_ident = format_ident!("{}Unit", ident);
                let params_start = if params.is_empty() { quote!() } else { quote!(#(#params),*,) };
                type_definitionen = Some(quote! {
                    /// Eine Variante ohne Anschlüsse.
                    #vis type #save_ident<#(#params),*> = #ident<#params_start Option<#arg>>;
                    /// Eine serialisierbare Repräsentation.
                    #vis type #unit_ident<#(#params),*> = #ident<#params_start ()>;
                    impl<#(#params),*> #base_ident::anschluss::de_serialisieren::Serialisiere for #ident<#(#params),*> {
                        type Serialisiert = #save_ident<#(#params),*>;
                        fn serialisiere(&self) -> #save_ident<#(#params),*> {
                            let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                            #save_ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(
                                    #param_fields: #param_fields.as_ref().map(
                                        |steuerung| steuerung.serialisiere()
                                    )
                                ),*
                            }
                        }
                        fn anschlüsse(self) -> (
                            Vec<#base_ident::anschluss::pin::pwm::Pin>,
                            Vec<#base_ident::anschluss::OutputAnschluss>,
                            Vec<#base_ident::anschluss::InputAnschluss>
                        ) {
                            let mut pwm0 = Vec::new();
                            let mut output0 = Vec::new();
                            let mut input0 = Vec::new();
                            #(
                                if let Some(steuerung) = self.#param_fields {
                                    let (pwm1, output1, input1) = steuerung.anschlüsse();
                                    pwm0.extend(pwm1);
                                    output0.extend(output1);
                                    input0.extend(input1);
                                }
                            )*
                            (pwm0, output0, input0)
                        }
                    }
                    impl<#(#params),*> #base_ident::anschluss::de_serialisieren::Reserviere<#ident<#(#params),*>> for #save_ident<#(#params),*> {
                        #[allow(unused_qualifications)]
                        type Arg = <Option<#arg> as #base_ident::anschluss::de_serialisieren::Reserviere<#default_type>>::Arg;

                        fn reserviere(
                            self,
                            lager: &mut #base_ident::anschluss::Lager,
                            pwm_pins: Vec<#base_ident::anschluss::pin::pwm::Pin>,
                            output_anschlüsse: Vec<#base_ident::anschluss::OutputAnschluss>,
                            input_anschlüsse: Vec<#base_ident::anschluss::InputAnschluss>,
                            arg: Self::Arg,
                        ) -> #base_ident::anschluss::de_serialisieren::Result<#ident<#(#params),*>> {
                            let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                            let reserviert = (#(#param_fields),*)
                                .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, arg)?
                                .konvertiere(|(#(#param_fields),*)| {
                                    #ident { #(#other_fields),*, #(#param_fields),* }
                                });
                            Ok(reserviert)
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
