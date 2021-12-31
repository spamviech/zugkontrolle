//! speichern und laden Methode für Gleise

use std::io::Read;

use serde::{
    de::{self, MapAccess, SeqAccess, Visitor},
    ser::SerializeStruct,
    Deserialize, Serialize,
};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        InputAnschluss, OutputAnschluss,
    },
    application::gleis::gleise::{daten::*, Fehler, Gleise},
    steuerung::{
        geschwindigkeit::{self, GeschwindigkeitSerialisiert, Mittelleiter, Zweileiter},
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
    zugtyp::Zugtyp,
};

pub(in crate::application::gleis::gleise::daten) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, (StreckenabschnittSerialisiert, GleiseDatenSerialisiert)>;
pub(in crate::application::gleis::gleise::daten) type GeschwindigkeitMapSerialisiert<Leiter> =
    HashMap<
        geschwindigkeit::Name,
        (GeschwindigkeitSerialisiert<Leiter>, StreckenabschnittMapSerialisiert),
    >;
#[derive(zugkontrolle_derive::Debug)]
#[zugkontrolle_debug(<Leiter as Serialisiere>::Serialisiert: Debug)]
pub struct ZustandSerialisiert<Leiter: Serialisiere> {
    pub(crate) zugtyp: Zugtyp<Leiter>,
    pub(crate) leiter: String,
    pub(crate) ohne_streckenabschnitt: GleiseDatenSerialisiert,
    pub(crate) ohne_geschwindigkeit: StreckenabschnittMapSerialisiert,
    pub(crate) geschwindigkeiten: GeschwindigkeitMapSerialisiert<Leiter>,
    pub(crate) pläne: Vec<Plan>,
}

// Explizite serde-Implementierung, damit Leiter kein automatisches Constraint bekommt
// https://serde.rs/deserialize-struct.html
impl<Leiter: Serialisiere> Serialize for ZustandSerialisiert<Leiter> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut zustand = serializer.serialize_struct("Zustand", 6)?;
        zustand.serialize_field("zugtyp", &self.zugtyp)?;
        zustand.serialize_field("leiter", &self.leiter)?;
        zustand.serialize_field("ohne_streckenabschnitt", &self.ohne_streckenabschnitt)?;
        zustand.serialize_field("ohne_geschwindigkeit", &self.ohne_geschwindigkeit)?;
        zustand.serialize_field("geschwindigkeiten", &self.geschwindigkeiten)?;
        zustand.serialize_field("pläne", &self.pläne)?;
        zustand.end()
    }
}

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
#[allow(non_camel_case_types)]
enum ZustandField {
    Zugtyp,
    Leiter,
    Ohne_Streckenabschnitt,
    Ohne_Geschwindigkeit,
    Geschwindigkeiten,
    Pläne,
}

struct ZustandVisitor<Leiter>(PhantomData<fn() -> Leiter>);

impl<'de, Leiter: Serialisiere> Visitor<'de> for ZustandVisitor<Leiter> {
    type Value = ZustandSerialisiert<Leiter>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("struct Zustand")
    }

    fn visit_seq<V: SeqAccess<'de>>(
        self,
        mut seq: V,
    ) -> Result<ZustandSerialisiert<Leiter>, V::Error> {
        let invalid_length_fehler = |n: usize| de::Error::invalid_length(n, &self);
        let zugtyp = seq.next_element()?.ok_or_else(|| invalid_length_fehler(0))?;
        let leiter = seq.next_element()?.ok_or_else(|| invalid_length_fehler(1))?;
        let ohne_streckenabschnitt = seq.next_element()?.ok_or_else(|| invalid_length_fehler(2))?;
        let ohne_geschwindigkeit = seq.next_element()?.ok_or_else(|| invalid_length_fehler(3))?;
        let geschwindigkeiten = seq.next_element()?.ok_or_else(|| invalid_length_fehler(4))?;
        let pläne = seq.next_element()?.unwrap_or_else(Vec::new);
        Ok(ZustandSerialisiert {
            zugtyp,
            leiter,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit,
            geschwindigkeiten,
            pläne,
        })
    }

    fn visit_map<V: MapAccess<'de>>(
        self,
        mut map: V,
    ) -> Result<ZustandSerialisiert<Leiter>, V::Error> {
        let mut zugtyp = None;
        let mut leiter = None;
        let mut ohne_streckenabschnitt = None;
        let mut ohne_geschwindigkeit = None;
        let mut geschwindigkeiten = None;
        let mut pläne = None;
        while let Some(key) = map.next_key()? {
            match key {
                ZustandField::Zugtyp => {
                    if zugtyp.is_some() {
                        return Err(de::Error::duplicate_field("zugtyp"));
                    }
                    zugtyp = Some(map.next_value()?)
                }
                ZustandField::Leiter => {
                    if leiter.is_some() {
                        return Err(de::Error::duplicate_field("leiter"));
                    }
                    leiter = Some(map.next_value()?)
                }
                ZustandField::Ohne_Streckenabschnitt => {
                    if ohne_streckenabschnitt.is_some() {
                        return Err(de::Error::duplicate_field("ohne_streckenabschnitt"));
                    }
                    ohne_streckenabschnitt = Some(map.next_value()?)
                }
                ZustandField::Ohne_Geschwindigkeit => {
                    if ohne_geschwindigkeit.is_some() {
                        return Err(de::Error::duplicate_field("ohne_geschwindigkeit"));
                    }
                    ohne_geschwindigkeit = Some(map.next_value()?)
                }
                ZustandField::Geschwindigkeiten => {
                    if geschwindigkeiten.is_some() {
                        return Err(de::Error::duplicate_field("geschwindigkeiten"));
                    }
                    geschwindigkeiten = Some(map.next_value()?)
                }
                ZustandField::Pläne => {
                    if pläne.is_some() {
                        return Err(de::Error::duplicate_field("pläne"));
                    }
                    pläne = Some(map.next_value()?)
                }
            }
        }
        let zugtyp = zugtyp.ok_or_else(|| de::Error::missing_field("zugtyp"))?;
        let leiter = leiter.ok_or_else(|| de::Error::missing_field("leiter"))?;
        let ohne_streckenabschnitt = ohne_streckenabschnitt
            .ok_or_else(|| de::Error::missing_field("ohne_streckenabschnitt"))?;
        let ohne_geschwindigkeit =
            ohne_geschwindigkeit.ok_or_else(|| de::Error::missing_field("ohne_geschwindigkeit"))?;
        let geschwindigkeiten =
            geschwindigkeiten.ok_or_else(|| de::Error::missing_field("geschwindigkeiten"))?;
        let pläne = pläne.unwrap_or_else(Vec::new);
        Ok(ZustandSerialisiert {
            zugtyp,
            leiter,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit,
            geschwindigkeiten,
            pläne,
        })
    }
}

impl<'de, Leiter: Serialisiere> Deserialize<'de> for ZustandSerialisiert<Leiter> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_struct(
            "zustand",
            &["zugtyp", "leiter", "ohne_streckenabschnitt", "geschwindigkeiten", "pläne"],
            ZustandVisitor::<Leiter>(PhantomData),
        )
    }
}

impl<Leiter: Serialisiere + BekannterLeiter> Zustand<Leiter> {
    /// Erzeuge eine serealisierbare Repräsentation.
    pub fn serialisiere(&self) -> ZustandSerialisiert<Leiter> {
        let serialisiere_streckenabschnitt_map = |map: &StreckenabschnittMap| {
            map.iter()
                .map(|(name, (streckenabschnitt, _fließend, daten))| {
                    (name.clone(), (streckenabschnitt.serialisiere(), daten.serialisiere()))
                })
                .collect()
        };

        ZustandSerialisiert {
            zugtyp: self.zugtyp.clone(),
            leiter: Leiter::NAME.to_string(),
            ohne_streckenabschnitt: self.ohne_streckenabschnitt.serialisiere(),
            ohne_geschwindigkeit: serialisiere_streckenabschnitt_map(&self.ohne_geschwindigkeit),
            geschwindigkeiten: self
                .geschwindigkeiten
                .iter()
                .map(|(name, (geschwindigkeit, streckenabschnitt_map))| {
                    (
                        name.clone(),
                        (
                            geschwindigkeit.serialisiere(),
                            serialisiere_streckenabschnitt_map(streckenabschnitt_map),
                        ),
                    )
                })
                .collect(),
            // TODO wirkliche Konvertierung, sobald Plan implementiert ist
            pläne: Vec::new(),
        }
    }

    /// Erhalten alle verwendeten Anschlüsse.
    #[inline(always)]
    pub fn anschlüsse(mut self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.anschlüsse_ausgeben()
    }

    fn anschlüsse_ausgeben(
        &mut self,
    ) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        let mut pwm_pins = Vec::new();
        let mut output_anschlüsse = Vec::new();
        let mut input_anschlüsse = Vec::new();
        fn collect_anschlüsse<S: Serialisiere>(
            struktur: S,
            pwm_pins: &mut Vec<pwm::Pin>,
            output_anschlüsse: &mut Vec<OutputAnschluss>,
            input_anschlüsse: &mut Vec<InputAnschluss>,
        ) {
            let (pwm, output, input) = struktur.anschlüsse();
            pwm_pins.extend(pwm.into_iter());
            output_anschlüsse.extend(output.into_iter());
            input_anschlüsse.extend(input.into_iter());
        }
        fn collect_gleis_anschlüsse<T: DatenAuswahl + Serialisiere>(
            daten: &mut GleiseDaten,
            pwm_pins: &mut Vec<pwm::Pin>,
            output_anschlüsse: &mut Vec<OutputAnschluss>,
            input_anschlüsse: &mut Vec<InputAnschluss>,
        ) {
            while let Some(geom_with_data) =
                daten.rstern_mut::<T>().remove_with_selection_function(SelectAll)
            {
                collect_anschlüsse(
                    geom_with_data.data.definition,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                )
            }
        }
        fn collect_daten_anschlüsse(
            daten: &mut GleiseDaten,
            pwm_pins: &mut Vec<pwm::Pin>,
            output_anschlüsse: &mut Vec<OutputAnschluss>,
            input_anschlüsse: &mut Vec<InputAnschluss>,
        ) {
            macro_rules! collect_gleis_anschlüsse {
                ($($typ: ident),* $(,)?) => {$(
                    collect_gleis_anschlüsse::<$typ>(
                        daten,
                        pwm_pins,
                        output_anschlüsse,
                        input_anschlüsse,
                    )
                );*}
            }
            collect_gleis_anschlüsse! {
                Gerade,
                Kurve,
                Weiche,
                DreiwegeWeiche,
                KurvenWeiche,
                SKurvenWeiche,
                Kreuzung
            }
        }
        collect_daten_anschlüsse(
            &mut self.ohne_streckenabschnitt,
            &mut pwm_pins,
            &mut output_anschlüsse,
            &mut input_anschlüsse,
        );
        fn collect_streckenabschnitt_map_anschlüsse(
            streckenabschnitt_map: &mut StreckenabschnittMap,
            pwm_pins: &mut Vec<pwm::Pin>,
            output_anschlüsse: &mut Vec<OutputAnschluss>,
            input_anschlüsse: &mut Vec<InputAnschluss>,
        ) {
            for (_name, (streckenabschnitt, _fließend, mut daten)) in streckenabschnitt_map.drain()
            {
                collect_anschlüsse(
                    streckenabschnitt,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                );
                collect_daten_anschlüsse(
                    &mut daten,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                );
            }
        }
        collect_streckenabschnitt_map_anschlüsse(
            &mut self.ohne_geschwindigkeit,
            &mut pwm_pins,
            &mut output_anschlüsse,
            &mut input_anschlüsse,
        );
        for (_name, (geschwindigkeit, mut streckenabschnitt_map)) in self.geschwindigkeiten.drain()
        {
            collect_anschlüsse(
                geschwindigkeit,
                &mut pwm_pins,
                &mut output_anschlüsse,
                &mut input_anschlüsse,
            );
            collect_streckenabschnitt_map_anschlüsse(
                &mut streckenabschnitt_map,
                &mut pwm_pins,
                &mut output_anschlüsse,
                &mut input_anschlüsse,
            );
        }
        (pwm_pins, output_anschlüsse, input_anschlüsse)
    }
}

impl<Leiter: Serialisiere + BekannterLeiter> ZustandSerialisiert<Leiter> {
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<Zustand<Leiter>, anschluss::Fehler> {
        let ZustandSerialisiert {
            zugtyp,
            leiter: _,
            ohne_streckenabschnitt,
            ohne_geschwindigkeit,
            geschwindigkeiten,
            // TODO wirkliche Konvertierung, sobald Plan implementiert ist
            pläne: _,
        } = self;
        let Reserviert {
            anschluss: ohne_streckenabschnitt,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = ohne_streckenabschnitt.reserviere(
            zugtyp.spurweite,
            lager,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        fn reserviere_streckenabschnitt_map(
            spurweite: Spurweite,
            lager: &mut anschluss::Lager,
            streckenabschnitt_map: StreckenabschnittMapSerialisiert,
            pwm_pins: Vec<pwm::Pin>,
            output_anschlüsse: Vec<OutputAnschluss>,
            input_anschlüsse: Vec<InputAnschluss>,
        ) -> Result<Reserviert<StreckenabschnittMap>, anschluss::Fehler> {
            streckenabschnitt_map.into_iter().fold(
                Ok(Reserviert {
                    anschluss: HashMap::new(),
                    pwm_nicht_benötigt: pwm_pins,
                    output_nicht_benötigt: output_anschlüsse,
                    input_nicht_benötigt: input_anschlüsse,
                }),
                |acc: Result<_, anschluss::Fehler>, (name, (streckenabschnitt, daten))| {
                    let Reserviert {
                        anschluss: mut map,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    } = acc?;
                    let Reserviert {
                        anschluss: streckenabschnitt,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    } = streckenabschnitt
                        .reserviere(
                            lager,
                            pwm_nicht_benötigt,
                            output_nicht_benötigt,
                            input_nicht_benötigt,
                        )
                        .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
                    let Reserviert {
                        anschluss: daten,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    } = daten.reserviere(
                        spurweite,
                        lager,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    )?;
                    let _ = map.insert(name, (streckenabschnitt, Fließend::Gesperrt, daten));
                    Ok(Reserviert {
                        anschluss: map,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    })
                },
            )
        }
        let Reserviert {
            anschluss: ohne_geschwindigkeit,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = reserviere_streckenabschnitt_map(
            zugtyp.spurweite,
            lager,
            ohne_geschwindigkeit,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        )?;
        let Reserviert {
            anschluss: geschwindigkeiten,
            pwm_nicht_benötigt: _,
            output_nicht_benötigt: _,
            input_nicht_benötigt: _,
        } = geschwindigkeiten.into_iter().fold(
            Ok(Reserviert {
                anschluss: HashMap::new(),
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            }),
            |acc: Result<_, anschluss::Fehler>,
             (name, (geschwindigkeit, streckenabschnitt_map))| {
                let Reserviert {
                    anschluss: mut map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = acc?;
                let Reserviert {
                    anschluss: geschwindigkeit,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = geschwindigkeit
                    .reserviere(
                        lager,
                        pwm_nicht_benötigt,
                        output_nicht_benötigt,
                        input_nicht_benötigt,
                    )
                    .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
                let Reserviert {
                    anschluss: streckenabschnitt_map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = reserviere_streckenabschnitt_map(
                    zugtyp.spurweite,
                    lager,
                    streckenabschnitt_map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                )?;
                let _ = map.insert(name, (geschwindigkeit, streckenabschnitt_map));
                Ok(Reserviert {
                    anschluss: map,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                })
            },
        )?;
        Ok(Zustand { zugtyp, ohne_streckenabschnitt, ohne_geschwindigkeit, geschwindigkeiten })
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct GleiseDatenSerialisiert {
    pub(crate) geraden: Vec<Gleis<GeradeSerialisiert>>,
    pub(crate) kurven: Vec<Gleis<KurveSerialisiert>>,
    pub(crate) weichen: Vec<Gleis<WeicheSerialisiert>>,
    pub(crate) dreiwege_weichen: Vec<Gleis<DreiwegeWeicheSerialisiert>>,
    pub(crate) kurven_weichen: Vec<Gleis<KurvenWeicheSerialisiert>>,
    pub(crate) s_kurven_weichen: Vec<Gleis<SKurvenWeicheSerialisiert>>,
    pub(crate) kreuzungen: Vec<Gleis<KreuzungSerialisiert>>,
}

impl GleiseDatenSerialisiert {
    pub(crate) fn neu() -> Self {
        GleiseDatenSerialisiert {
            geraden: Vec::new(),
            kurven: Vec::new(),
            weichen: Vec::new(),
            dreiwege_weichen: Vec::new(),
            kurven_weichen: Vec::new(),
            s_kurven_weichen: Vec::new(),
            kreuzungen: Vec::new(),
        }
    }
}

impl GleiseDaten {
    /// Erzeuge eine serealisierbare Repräsentation
    fn serialisiere(&self) -> GleiseDatenSerialisiert {
        macro_rules! rstern_to_vecs {
            ($($rstern:ident),* $(,)?) => {
                GleiseDatenSerialisiert {
                    $($rstern: self.$rstern.iter().map(
                        |GeomWithData {data, ..}| {
                            Gleis {
                                position: data.position.clone(),
                                definition: data.definition.serialisiere(),
                            }
                        })
                        .collect()
                    ),*
                }
            };
        }
        rstern_to_vecs! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        }
    }
}

fn reserviere_anschlüsse<T: Zeichnen + Serialisiere>(
    spurweite: Spurweite,
    lager: &mut anschluss::Lager,
    source: Vec<Gleis<<T as Serialisiere>::Serialisiert>>,
    pwm_pins: Vec<pwm::Pin>,
    output_anschlüsse: Vec<OutputAnschluss>,
    input_anschlüsse: Vec<InputAnschluss>,
) -> Result<
    (
        Vec<GeomWithData<Rectangle<Vektor>, Gleis<T>>>,
        Vec<pwm::Pin>,
        Vec<OutputAnschluss>,
        Vec<InputAnschluss>,
    ),
    anschluss::Fehler,
> {
    source.into_iter().fold(
        Ok((Vec::new(), pwm_pins, output_anschlüsse, input_anschlüsse)),
        |acc_res: Result<_, anschluss::Fehler>, gleis_save| {
            let mut acc = acc_res?;
            let Reserviert {
                anschluss: gleis,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            } = gleis_save
                .reserviere(lager, acc.1, acc.2, acc.3)
                .map_err(|de_serialisieren::Fehler { fehler, .. }| fehler)?;
            let rectangle =
                Rectangle::from(gleis.definition.rechteck_an_position(spurweite, &gleis.position));
            acc.0.push(GeomWithData::new(rectangle, gleis));
            Ok((acc.0, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt))
        },
    )
}

impl GleiseDatenSerialisiert {
    /// Reserviere alle benötigten Anschlüsse.
    fn reserviere(
        self,
        spurweite: Spurweite,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<Reserviert<GleiseDaten>, anschluss::Fehler> {
        macro_rules! reserviere_anschlüsse {
            ($($rstern: ident),* $(,)?) => {
                $(
                    let ($rstern, pwm_pins, output_anschlüsse, input_anschlüsse) =
                        reserviere_anschlüsse(
                            spurweite,
                            lager,
                            self.$rstern,
                            pwm_pins,
                            output_anschlüsse,
                            input_anschlüsse
                        )?;
                )*
                Ok(Reserviert {
                    anschluss: GleiseDaten {
                        $($rstern: RTree::bulk_load($rstern)),*
                    },
                    pwm_nicht_benötigt: pwm_pins,
                    output_nicht_benötigt: output_anschlüsse,
                    input_nicht_benötigt: input_anschlüsse,
                })
            };
        }
        reserviere_anschlüsse! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        }
    }
}

pub trait BekannterLeiter: Sized {
    const NAME: &'static str;

    fn bekannter_zugtyp(name: &str) -> Option<Zugtyp<Self>>;
}

impl BekannterLeiter for Mittelleiter {
    const NAME: &'static str = "Mittelleiter";

    fn bekannter_zugtyp(name: &str) -> Option<Zugtyp<Self>> {
        if name == "Märklin" {
            Some(Zugtyp::märklin())
        } else {
            None
        }
    }
}

impl BekannterLeiter for Zweileiter {
    const NAME: &'static str = "Zweileiter";

    fn bekannter_zugtyp(name: &str) -> Option<Zugtyp<Self>> {
        if name == "Lego" {
            Some(Zugtyp::lego())
        } else {
            None
        }
    }
}

impl<Leiter: Serialisiere + BekannterLeiter> Gleise<Leiter> {
    pub fn speichern(&self, pfad: impl AsRef<std::path::Path>) -> Result<(), Fehler> {
        let serialisiert = self.zustand.serialisiere();
        let file = std::fs::File::create(pfad)?;
        bincode::serialize_into(file, &serialisiert).map_err(Fehler::BincodeSerialisieren)
    }

    pub fn laden(
        &mut self,
        lager: &mut anschluss::Lager,
        pfad: impl AsRef<std::path::Path>,
    ) -> Result<(), Fehler> {
        // aktuellen Zustand zurücksetzen, bisherige Anschlüsse sammeln
        self.canvas.leeren();
        let (pwm_pins, output_anschlüsse, input_anschlüsse) = self.zustand.anschlüsse_ausgeben();

        // TODO pivot, skalieren, Modus?
        // last_mouse, last_size nicht anpassen

        // lese & parse Datei
        let mut file = std::fs::File::open(pfad)?;
        let mut content = Vec::new();
        let _ = file.read_to_end(&mut content)?;
        let slice = content.as_slice();
        let zustand_serialisiert: ZustandSerialisiert<Leiter> = bincode::deserialize(slice)
            .or_else(|aktuell| {
                bincode::deserialize(slice)
                    .map_err(|v2| Fehler::BincodeDeserialisieren { aktuell, v2 })
                    .and_then(v2::GleiseVecs::<Leiter>::try_into)
            })?;

        let leiter = Leiter::NAME.to_string();
        if zustand_serialisiert.leiter != leiter {
            return Err(Fehler::FalscherLeiter(zustand_serialisiert.leiter));
        }

        // reserviere Anschlüsse
        self.zustand = zustand_serialisiert.reserviere(
            lager,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        Ok(())
    }
}
