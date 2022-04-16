//! Erzeuge Richtung enum und RichtungAnschlüsse(Serialisiert) Strukturen mit Lookup-Implementierung.

use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};

pub(crate) fn erstelle_richtung(args: Vec<syn::NestedMeta>, item: syn::ItemEnum) -> TokenStream {
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
        let default_variant = enum_variants[0];
        let enum_variants_str = enum_variants.iter().map(ToString::to_string);
        let struct_fields: Vec<syn::Ident> = enum_variants
            .iter()
            .map(|variant| format_ident!("{}", &variant.to_string().to_snake_case()))
            .collect();

        enum_definition = Some(quote! {
            type OutputAuswahl = #base_ident::application::anschluss::Zustand<#base_ident::application::anschluss::Output>;
            #[zugkontrolle_macros::impl_nachschlagen(#base_ident::anschluss::OutputAnschluss, RichtungAnschlüsse, Debug)]
            #[zugkontrolle_macros::impl_nachschlagen(#base_ident::anschluss::OutputSerialisiert, RichtungAnschlüsseSerialisiert, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
            #[zugkontrolle_macros::impl_nachschlagen(OutputAuswahl, RichtungAnschlüsseAuswahlZustand, Debug)]
            /// Mögliche Richtungen zum Schalten.
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
            #vis enum Richtung {
                #(
                    #[allow(missing_docs)]
                    #enum_variants
                ),*
            }
            impl Default for Richtung {
                fn default() -> Self {
                    Richtung::#default_variant
                }
            }
            impl std::fmt::Display for Richtung {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}",
                        match self {
                            #( Richtung::#enum_variants =>  #enum_variants_str ),*
                        }
                    )
                }
            }
            impl #base_ident::anschluss::de_serialisieren::Serialisiere for RichtungAnschlüsse {
                type Serialisiert = RichtungAnschlüsseSerialisiert;
                fn serialisiere(&self) -> RichtungAnschlüsseSerialisiert {
                    let RichtungAnschlüsse { #(#struct_fields),* } = self;
                    RichtungAnschlüsseSerialisiert { #(#struct_fields: #struct_fields.serialisiere()),* }
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
                        let (pwm1, output1, input1) = self.#struct_fields.anschlüsse();
                        pwm0.extend(pwm1);
                        output0.extend(output1);
                        input0.extend(input1);
                    )*
                    (pwm0, output0, input0)
                }
            }
            impl #base_ident::anschluss::de_serialisieren::Reserviere<RichtungAnschlüsse> for RichtungAnschlüsseSerialisiert {
                fn reserviere(
                    self,
                    lager: &mut #base_ident::anschluss::Lager,
                    pwm_pins: Vec<#base_ident::anschluss::pin::pwm::Pin>,
                    output_anschlüsse: Vec<#base_ident::anschluss::OutputAnschluss>,
                    input_anschlüsse: Vec<#base_ident::anschluss::InputAnschluss>,
                ) -> #base_ident::anschluss::de_serialisieren::Result<RichtungAnschlüsse> {
                    let RichtungAnschlüsseSerialisiert { #(#struct_fields),* } = self;
                    let reserviert = (#(#struct_fields),*)
                        .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
                        .konvertiere(|(#(#struct_fields),*)| RichtungAnschlüsse { #(#struct_fields),* });
                    Ok(reserviert)
                }
            }
            impl Default for RichtungAnschlüsseSerialisiert {
                fn default() -> Self {
                    RichtungAnschlüsseSerialisiert {
                        #(#struct_fields: #base_ident::anschluss::OutputSerialisiert::Pin {pin:0, polarität: #base_ident::anschluss::polarität::Polarität::Normal}),*
                    }
                }
            }
            impl From<RichtungAnschlüsseSerialisiert> for RichtungAnschlüsseAuswahlZustand {
                fn from(anschlüsse_save: RichtungAnschlüsseSerialisiert) -> Self {
                    RichtungAnschlüsseAuswahlZustand {
                        #(#struct_fields: #base_ident::application::anschluss::Zustand::von_output_serialisiert(anschlüsse_save.#struct_fields)),*
                    }
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
        };
    }

    quote! {
        #item
        #enum_definition
    }
}
