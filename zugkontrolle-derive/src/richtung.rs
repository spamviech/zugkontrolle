//! Erzeuge Richtung enum und RichtungAnschlüsse(Save) Strukturen mit Lookup-Implementierung.

use inflector::cases::snakecase::to_snake_case;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};

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
        let default_variant = enum_variants[0];
        let enum_variants_str = enum_variants.iter().map(ToString::to_string);
        let struct_fields: Vec<syn::Ident> = enum_variants
                    .iter()
                    // TODO fix upstream?
                    // to_snakecase wrongly adds a '_' before 'ß', even though it it a small letter
                    // possibly because there is no real uppercase character of it
                    .map(|variant| format_ident!("{}", to_snake_case(&variant.to_string()).replace("_ß", "ß")))
                    .collect();

        enum_definition = Some(quote! {
            type OutputAuswahl = #base_ident::application::anschluss::Status<#base_ident::application::anschluss::Output>;
            #[zugkontrolle_derive::impl_lookup(#base_ident::anschluss::OutputAnschluss, Anschlüsse, Debug)]
            #[zugkontrolle_derive::impl_lookup(#base_ident::anschluss::OutputSave, AnschlüsseSave, Debug, Clone, Serialize, Deserialize)]
            #[zugkontrolle_derive::impl_lookup(OutputAuswahl, AnschlüsseAuswahlStatus, Debug)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
            #vis enum Richtung {
                #(#enum_variants),*
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
            impl #base_ident::anschluss::speichern::ToSave for RichtungAnschlüsse {
                type Save = RichtungAnschlüsseSave;
                fn to_save(&self) -> RichtungAnschlüsseSave {
                    let RichtungAnschlüsse { #(#struct_fields),* } = self;
                    RichtungAnschlüsseSave { #(#struct_fields: #struct_fields.to_save()),* }
                }
                fn anschlüsse(self) -> (Vec<#base_ident::anschluss::pwm::Pin>, Vec<#base_ident::anschluss::OutputAnschluss>, Vec<#base_ident::anschluss::InputAnschluss>) {
                    let mut pwm0 = Vec::new();
                    let mut output0 = Vec::new();
                    let mut input0 = Vec::new();
                    #(
                        let (pwm1, output1, input1) = self.#struct_fields.anschlüsse();
                        pwm0.extend(pwm1.into_iter());
                        output0.extend(output1.into_iter());
                        input0.extend(input1.into_iter());
                    )*
                    (pwm0, output0, input0)
                }
            }
            impl #base_ident::anschluss::speichern::Reserviere<RichtungAnschlüsse> for RichtungAnschlüsseSave {
                fn reserviere(
                    self,
                    anschlüsse: &mut #base_ident::anschluss::Anschlüsse,
                    pwm_nicht_benötigt: Vec<#base_ident::anschluss::pwm::Pin>,
                    output_nicht_benötigt: Vec<#base_ident::anschluss::OutputAnschluss>,
                    input_nicht_benötigt: Vec<#base_ident::anschluss::InputAnschluss>,
                ) -> #base_ident::anschluss::speichern::Result<RichtungAnschlüsse> {
                    let RichtungAnschlüsseSave {  #(#struct_fields),* } = self;
                    #(let #base_ident::anschluss::speichern::Reserviert {anschluss: #struct_fields, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt} = #struct_fields.reserviere(anschlüsse, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt)?; )*
                    Ok(#base_ident::anschluss::speichern::Reserviert {
                        anschluss: RichtungAnschlüsse {
                            #(#struct_fields),*
                        },
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    })
                }
            }
            impl Default for RichtungAnschlüsseSave {
                fn default() -> Self {
                    RichtungAnschlüsseSave {
                        #(#struct_fields: #base_ident::anschluss::OutputSave::Pin {pin:0, polarität: #base_ident::anschluss::Polarität::Normal}),*
                    }
                }
            }
            impl From<RichtungAnschlüsseSave> for RichtungAnschlüsseAuswahlStatus {
                fn from(anschlüsse_save: RichtungAnschlüsseSave) -> Self {
                    RichtungAnschlüsseAuswahlStatus {
                        #(#struct_fields: #base_ident::application::anschluss::Status::von_output_save(anschlüsse_save.#struct_fields)),*
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
